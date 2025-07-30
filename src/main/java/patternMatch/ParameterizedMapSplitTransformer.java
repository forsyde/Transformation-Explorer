package patternMatch;

import forsyde.io.core.*;
import forsyde.io.lib.hierarchy.ForSyDeHierarchy;
import forsyde.io.lib.hierarchy.behavior.FunctionLikeEntity;
import forsyde.io.lib.hierarchy.behavior.moc.sy.SYMap;
import forsyde.io.lib.hierarchy.behavior.moc.sy.SYMapViewer;
import forsyde.io.lib.hierarchy.behavior.moc.sy.SYSignalViewer;
import forsyde.io.lib.hierarchy.behavior.parallel.Vectorizable;
import forsyde.io.lib.hierarchy.implementation.functional.InstrumentedBehaviour;
import forsyde.io.lib.hierarchy.visualization.GreyBoxViewer;
import org.jgrapht.Graph;
import org.jgrapht.GraphPath;
import org.jgrapht.alg.interfaces.ShortestPathAlgorithm;
import org.jgrapht.alg.shortestpath.DijkstraShortestPath;
import org.jgrapht.graph.AsSubgraph;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class ParameterizedMapSplitTransformer implements Transformer {

    private final Map<String, Double> operationCycles;
    public ParameterizedMapSplitTransformer(Map<String, Double> operationCycles) {
        this.operationCycles = operationCycles;
    }

    public Set<SystemGraph> applyTransform(SystemGraph systemGraph) {
        int maxNumCore = 8;
        int minBandwidth = 4; // minimum bandwidth in bytes
        int max_num_hop_bus = 1; // the worst-case number of hops (for NoC) or bus transfers (for a multi-bus system)
        int hop_bus_Transfer = 8; // represents the per-hop (NoC) or per-bus transfer (multi-bus) delay


        Set<SystemGraph> mapSplitTransformedGraphs= new HashSet<>();

        // Synchronous processes recognition
        List<SYMap> syMaps = new ArrayList<>();
        AtomicInteger apply = new AtomicInteger();


        // traverse the vertices to find the vectorizables
        final Set<Vectorizable> vectorizables = new HashSet<>();
        for (Vertex v : systemGraph.vertexSet()) {
            if (ForSyDeHierarchy.Vectorizable.tryView(systemGraph, v).isPresent()) {
                vectorizables.add(ForSyDeHierarchy.Vectorizable.tryView(systemGraph, v).get());
            }
        }

        // traverse the vertices
        Map<SYMap, List<FunctionLikeEntity>> processesAndOrderedExecutables = new HashMap<>();
        for (Vertex v : systemGraph.vertexSet()) {

            // SY vertices recognition
            if (ForSyDeHierarchy.SYMap.tryView(systemGraph, v).isPresent()) {
                syMaps.add(ForSyDeHierarchy.SYMap.tryView(systemGraph, v).get());

                int inPortNum = 0;
                for (EdgeInfo e : systemGraph.incomingEdgesOf(v)) {
                    if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.SYNetworkEdge)) {
                        inPortNum += 1;
                    }
                }
                int outPortNum = 0;
                for (EdgeInfo e : systemGraph.outgoingEdgesOf(v)) {
                    if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.SYNetworkEdge)) {
                        outPortNum += 1;
                    }
                }

                //System.out.println(v.getIdentifier() + ", inputs is " + inPortNum + ", and output is " + outPortNum);

                // mapSY recognition
                if (inPortNum == 1 && outPortNum == 1) {
                    SYMap syMap = ForSyDeHierarchy.SYMap.tryView(systemGraph, v).get();

                    final Set<FunctionLikeEntity> combFunctions = syMap.combFunctions();


                    final Set<EdgeInfo> parallelComputationEdges = new HashSet<>();
                    for (EdgeInfo e : systemGraph.edgeSet()) {
                        if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.ParallelComputationEdge)) {
                            parallelComputationEdges.add(e);
                        }
                    }
                    // Finding the order of the combination functions inside mapSY (for the sake of Map-Split transformation)
                    final Set<Vertex> combsAndVectors = Stream.concat(combFunctions.stream().map(VertexViewer::getViewedVertex), vectorizables.stream().map(VertexViewer::getViewedVertex)).collect(Collectors.toSet());
                    final Graph<Vertex, EdgeInfo> combFunctionsGraph = new AsSubgraph<>(systemGraph, combsAndVectors);
                    final ShortestPathAlgorithm<Vertex, EdgeInfo> shortestPathAlgorithm = new DijkstraShortestPath<>(combFunctionsGraph);
                    List<FunctionLikeEntity> combFunctionsHaskellSorted = new ArrayList<>(combFunctions);
                    combFunctionsHaskellSorted.sort((src, dst) -> {
                        final GraphPath<Vertex, EdgeInfo> src2dst = shortestPathAlgorithm.getPath(src.getViewedVertex(), dst.getViewedVertex());
                        final GraphPath<Vertex, EdgeInfo> dst2src = shortestPathAlgorithm.getPath(dst.getViewedVertex(), src.getViewedVertex());
                        if (src == dst) {
                            return 0;
                        } else if (src2dst != null && dst2src != null) {
                            return src2dst.getLength() - dst2src.getLength();
                        } else if (src2dst != null) {
                            return -1;
                        } else if (dst2src != null) {
                            return 1;
                        } else {
                            return 0;
                        }
                    });
                    processesAndOrderedExecutables.put(ForSyDeHierarchy.SYMap.tryView(systemGraph, v).get(), combFunctionsHaskellSorted);

                    while (combFunctionsHaskellSorted.size() < combFunctions.size()) {
                        boolean isNext = false;
                        final FunctionLikeEntity exe = combFunctionsHaskellSorted.get(combFunctionsHaskellSorted.size() - 1);
                        for (FunctionLikeEntity exe2 : combFunctions) {
                            if (!combFunctionsHaskellSorted.contains(exe2)) {
                                for (EdgeInfo e : systemGraph.getAllEdges(exe.getViewedVertex(), exe2.getViewedVertex())) {
                                    if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.ParallelComputationEdge)) {
                                        isNext = true;
                                    }
                                }
                                if (isNext) {
                                    combFunctionsHaskellSorted.add(exe2);
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }

        for (SYMap syMap : processesAndOrderedExecutables.keySet()) {
            GreyBoxViewer root = null;
            for (EdgeInfo e : systemGraph.incomingEdgesOf(syMap.getViewedVertex())){
                if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.VisualContainment)){
                    final Vertex rootV = systemGraph.getEdgeSource(e);
                    root = ForSyDeHierarchy.GreyBox.tryView(systemGraph, rootV).get();
                }
            }
            if (!processesAndOrderedExecutables.get(syMap).isEmpty() && processesAndOrderedExecutables.get(syMap).size() > 1) {
                if (!processesAndOrderedExecutables.get(syMap).isEmpty() &&
                        processesAndOrderedExecutables.get(syMap).size() > 1) {

                    System.out.println("\n==================================================");
                    System.out.println("        Map-Split (Parametrized)Pattern Detected");
                    System.out.println("==================================================");
                    System.out.printf("%-25s : %s%n", "SYMap Vertex", syMap.getIdentifier());

                    List<FunctionLikeEntity> executables = processesAndOrderedExecutables.get(syMap);
                    String functionLabel = executables.size() == 1 ? "Combinational Function" : "Combinational Functions";
                    //System.out.printf("%-25s : %d %s%n", functionLabel, executables.size(), executables.size() == 1 ? "" : "found");

                    System.out.println("Ordered " + functionLabel + ":");

                    int index = 1;
                    for (FunctionLikeEntity executable : executables) {
                        System.out.printf("  [%d] %s%n", index++, executable.getIdentifier());
                    }

                    System.out.println("--------------------------------------------------\n");

                }

                List<FunctionLikeEntity> executables = processesAndOrderedExecutables.get(syMap);
                //Finding the dimensions of the input (which determines the number of mapSYs and is used for cost calculation)
                Vertex srcSignal = null;
                Vertex dstSignal = null;
                List<Integer> srcSigChildDimentions = new ArrayList<>();
                List<Integer> dstSigChildDimentions = new ArrayList<>();
                Scanner scanner = new Scanner(System.in);
                List<Integer> groupSize = new ArrayList<>();
                for (EdgeInfo e : systemGraph.incomingEdgesOf(syMap)) {
                    if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.SYNetworkEdge)) {
                        srcSignal = systemGraph.queryVertex(e.getSource()).get();
                        for (EdgeInfo ed : systemGraph.outgoingEdgesOf(srcSignal)) {
                            if (ed.hasTrait(ForSyDeHierarchy.EdgeTraits.VisualContainment)){
                                final Vertex sigChild = systemGraph.queryVertex(ed.getTarget()).get();
                                if (ForSyDeHierarchy.Vectorizable.tryView(systemGraph, sigChild).isPresent()){
                                    srcSigChildDimentions = ForSyDeHierarchy.Vectorizable.tryView(systemGraph, sigChild).get().dimensions();
                                    //The first dimension which is the number of rows in the case of a matrix is the number of mapSYs.
//                                                    System.out.println("Please enter the intended size of each process (the dimension of the input signal is "+ srcSigChildDimentions.get(0) + "):");
//                                                    groupSize = scanner.nextInt();
                                }
                            }
                        }
                    }
                }

                int dataSize = 1;
                for (int dim : srcSigChildDimentions) {
                    dataSize *= dim;
                }

                long C_lhs = 0;
                long bestCost = Long.MAX_VALUE;

                // ++++++++++++++++++++++++++++ Original vertex v cost calculation ++++++++++++++++++++++++++++++++++++
                Set<FunctionLikeEntity> combFuncs = new HashSet<>(syMap.combFunctions());
                Set<InstrumentedBehaviour> basicFuncs = new HashSet<>();
                Queue<FunctionLikeEntity> queue = new LinkedList<>(combFuncs);

                while (!queue.isEmpty()) {
                    FunctionLikeEntity kers = queue.poll();

                    if (ForSyDeHierarchy.HasANSICImplementations.tryView(kers).isPresent() |
                            ForSyDeHierarchy.InterleaveV.tryView(kers).isPresent()) {
                        basicFuncs.add(ForSyDeHierarchy.InstrumentedBehaviour.enforce(kers));
                    } else {
                        if (ForSyDeHierarchy.MapV.tryView(kers).isPresent()) {
                            for (FunctionLikeEntity k : ForSyDeHierarchy.MapV.tryView(kers).get().kernels()) {
                                if (combFuncs.add(k)) queue.add(k);
                            }
                        } else if (ForSyDeHierarchy.ReduceV.tryView(kers).isPresent()) {
                            for (FunctionLikeEntity k : ForSyDeHierarchy.ReduceV.tryView(kers).get().kernels()) {
                                if (combFuncs.add(k)) queue.add(k);
                            }
                        }
                    }
                }


                // New map: function ID → cost per element
                Map<String, Long> costPerElementPerFunction = new HashMap<>();

                Map<String, Long> computationCostsPerElement = new HashMap<>();
                Map<String, Long> computationCostsTotal = new HashMap<>();

                for (InstrumentedBehaviour func : basicFuncs) {
                    Map<String, Map<String, Long>> requirements = func.computationalRequirements();

                    for (Map.Entry<String, Map<String, Long>> implEntry : requirements.entrySet()) {
                        String implementation = implEntry.getKey();
                        Map<String, Long> operations = implEntry.getValue();

                        long costPerElement = 0;

                        for (Map.Entry<String, Long> opEntry : operations.entrySet()) {
                            String op = opEntry.getKey();
                            long count = opEntry.getValue();

                            Double cyclesPerOp = operationCycles.get(op);
                            if (cyclesPerOp != null) {
                                costPerElement += (long) (count * cyclesPerOp);
                            } else {
                                System.out.println("Warning: No cycle info for operation: " + op);
                            }
                        }

                        // Store per-function cost for this implementation
                        costPerElementPerFunction.put(func.getIdentifier(), costPerElement);

                        // Aggregate normally for global summaries
                        computationCostsPerElement.merge(implementation, costPerElement, Long::sum);
                        computationCostsTotal.merge(implementation, costPerElement * dataSize, Long::sum);
                    }
                }

                // === Print cost per basic function ===
                System.out.println("\n--------------------------------------------------");
                System.out.println("   Cost Per Element for Basic Functions");
                System.out.println("--------------------------------------------------");
                System.out.printf("%-30s | %-15s%n", "Function", "Cost Per Element");
                System.out.println("--------------------------------------------------");

                for (Map.Entry<String, Long> entry : costPerElementPerFunction.entrySet()) {
                    String funcId = entry.getKey();
                    Long cost = entry.getValue();
                    System.out.printf("%-30s | %-15d%n", funcId, cost);
                }

                System.out.println("--------------------------------------------------");




                // === Print original cost table (C_lhs) ===
                System.out.println("\n--------------------------------------------------");
                System.out.println("    C_lhs Per Implementation (Orignal)");
                System.out.println("--------------------------------------------------");
                System.out.printf("%-25s | %-15s%n", "Implementation", "Total Cost");
                System.out.println("--------------------------------------------------");


                for (String impl : computationCostsTotal.keySet()) {
                    long total = computationCostsTotal.getOrDefault(impl, 0L);
                    System.out.printf("%-25s | %-15d%n", impl, total);
                    C_lhs = Math.max(C_lhs, total);
                }

                System.out.println("--------------------------------------------------");
                System.out.printf("%-25s   %-15d%n", "Worst Cost (Max C_lhs)", C_lhs);
                System.out.println("--------------------------------------------------");

                // ++++++++++++++++++++++++++++ Transformed vertex v cost calculation +++++++++++++++++++++++++++++++++

                //the partitioning parameter
                List<Integer> P = new ArrayList<>();


                System.out.println("\nComputation Costs for Transformed Graph by Partitioning Parameter:");

                Map<String, Long> costTotalPerExecutable = new LinkedHashMap<>();
                Map<String, Long> costPerElementPerExecutable = new LinkedHashMap<>();

                for (int i = 0; i < executables.size(); i++) {
                    FunctionLikeEntity executable = executables.get(i);
                    String id = executable.getIdentifier();

                    // Compute cost per element
                    long costPerElement = computeCostPerElementForExecutable(executable, operationCycles);
                    costPerElementPerExecutable.put(id, costPerElement);

                    // Determine data size
                    int currentDataSize = 1;
                    if (i == 0) {
                        // First executable: use original syMap data size
                        currentDataSize = dataSize;
                    } else {
                        // Otherwise, traverse ParallelComputationEdge to get source dimensions
                        for (EdgeInfo e : systemGraph.incomingEdgesOf(executable)) {
                            if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.ParallelComputationEdge)) {
                                Vertex src = systemGraph.getEdgeSource(e);
                                List<Integer> dims = ForSyDeHierarchy.Vectorizable.tryView(systemGraph, src).get().dimensions();
                                currentDataSize = dims.stream().reduce(1, (a, b) -> a * b);
                            }
                        }
                    }

                    long totalCost = costPerElement * currentDataSize;
                    costTotalPerExecutable.put(id, totalCost);
                }


                System.out.println("\n--------------------------------------------------");
                System.out.println("    Cost Per Element for Each Executable");
                System.out.println("--------------------------------------------------");
                System.out.printf("%-25s | %-15s%n", "Executable", "Cost/Element");
                System.out.println("--------------------------------------------------");

                for (Map.Entry<String, Long> entry : costPerElementPerExecutable.entrySet()) {
                    System.out.printf("%-25s | %-15d%n", entry.getKey(), entry.getValue());
                }

                System.out.println("--------------------------------------------------");

                System.out.println("\n--------------------------------------------------");
                System.out.println("   Total Computation Cost per Executable");
                System.out.println("--------------------------------------------------");
                System.out.printf("%-25s | %-15s%n", "Executable", "Total Cost");
                System.out.println("--------------------------------------------------");

                for (Map.Entry<String, Long> entry : costTotalPerExecutable.entrySet()) {
                    System.out.printf("%-25s | %-15d%n", entry.getKey(), entry.getValue());
                }

                System.out.println("--------------------------------------------------");



                List<List<String>> bestPartition = null;
                if (executables.size() == 2) {
                    // Only one possible partitioning: [f1], [f2]
                    List<List<String>> onlyPartition = List.of(
                            List.of(executables.get(0).getIdentifier()),
                            List.of(executables.get(1).getIdentifier())
                    );

                    // Compute cost of each stage using total costs
                    List<Long> stageCosts = new ArrayList<>();
                    for (List<String> group : onlyPartition) {
                        long stageCost = 0;
                        for (String fID : group) {
                            stageCost += costTotalPerExecutable.getOrDefault(fID, 0L);
                        }
                        stageCosts.add(stageCost);
                    }

                    long rhsCost = Collections.max(stageCosts);
                    System.out.println("Only Partitioning: " + onlyPartition);
                    System.out.println("C_rhs = " + rhsCost);

                } else {
                    // Try all 2 to (n-1)-way partitions
                    int n = executables.size();


                    for (int numGroups = 2; numGroups <= n; numGroups++) {
                        List<List<Integer>> partitions = generateOrderedPartitions(n, numGroups);

                        for (List<Integer> splitPoints : partitions) {
                            List<List<String>> groups = new ArrayList<>();
                            int start = 0;
                            for (int end : splitPoints) {
                                List<String> group = executables.subList(start, end).stream()
                                        .map(FunctionLikeEntity::getIdentifier)
                                        .collect(Collectors.toList());
                                groups.add(group);
                                start = end;
                            }

                            long maxStageCost = 0;
                            for (List<String> group : groups) {
                                long stageCost = 0;
                                for (String fID : group) {
                                    stageCost += costTotalPerExecutable.getOrDefault(fID, 0L);
                                }
                                maxStageCost = Math.max(maxStageCost, stageCost);
                            }

                            if (maxStageCost < bestCost) {
                                bestCost = maxStageCost;
                                bestPartition = groups;
                            }
                        }
                    }

                    System.out.println("Best Partitioning: " + bestPartition);
                    System.out.println("C_rhs = " + bestCost);

                    P = bestPartition.stream()
                            .map(List::size)
                            .collect(Collectors.toList());
                }

                // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                //Applying the transformation on only one of the vertices of a parallel structure
                Vertex parStructParent = null;
                apply.set(1);
                for (EdgeInfo e: systemGraph.incomingEdgesOf(syMap)){
                    if (apply.getPlain() == 1) {
                        if (e.hasTrait(new OpaqueTrait("annotationEdge"))){
                            if (systemGraph.getEdgeSource(e).hasTrait(new OpaqueTrait("ParStruct"))) {
                                if (systemGraph.getEdgeSource(e).getProperty("vectorProcesses") instanceof ArrayList<?> list) {
                                    var curIdx = list.indexOf(syMap.getIdentifier());
                                    boolean isFirst = true;
                                    for (int i = 0; i < list.size() && i < curIdx; i++) {
                                        if (list.get(i) instanceof String otherId) {
                                            isFirst = isFirst && systemGraph.queryVertex(otherId).isEmpty();
                                        }
                                    }
                                    if (!isFirst) apply.set(0);
                                }
                            }
                        }
                    }
                }
                //----------------------------------------------------------------------------

                System.out.println();
                // Map-Split transformation application
                if (apply.getPlain() == 1 && bestCost < C_lhs) {
                    Random random = new Random();
                    int rand = random.nextInt(1000);
                    final SystemGraph transformedGraph = new NamedSystemGraph(syMap.getIdentifier() + rand + "_T");
                    transformedGraph.mergeInPlace(systemGraph);
                    // this need to be done since the root was viewed in the wrong system graph
                    root = ForSyDeHierarchy.GreyBox.tryView(transformedGraph, root.getViewedVertex()).get();
                    final List<SYMapViewer> newSYMaps = new ArrayList<>();
                    //Generating new SYMaps: mapSY(f), mapSY(g), ...
                    int funcIndex = 0;
                    for (List<String> group : bestPartition) {
                        Random random2 = new Random();
                        int rand2 = random2.nextInt(1000);
                        final SYMapViewer newSYMap = ForSyDeHierarchy.SYMap.enforce(
                                transformedGraph,
                                transformedGraph.newVertex("Stage_" + rand2 + "_TSY")
                        );
                        newSYMaps.add(newSYMap);

                        GreyBoxViewer newSYMapGB = ForSyDeHierarchy.GreyBox.enforce(newSYMap);
                        root.addContained(newSYMapGB);

                        for (int i = 0; i < group.size(); i++) {
                            FunctionLikeEntity executable = executables.get(funcIndex++);
                            newSYMap.addCombFunctions(executable);
                            newSYMapGB.addContained(ForSyDeHierarchy.Visualizable.enforce(executable));
                        }
                    }

                    //Generating signals between the new SYMaps: the signal between mapSY(f) and mapSY(g), ...
                    for (int i = 0; i < bestPartition.size() - 1; i++) {
                        final SYMapViewer leftSYMap = newSYMaps.get(i);
                        final SYMapViewer rightSYMap = newSYMaps.get(i + 1);

                        final SYSignalViewer newSYSignal = ForSyDeHierarchy.SYSignal.enforce(
                                transformedGraph,
                                transformedGraph.newVertex(leftSYMap.getIdentifier() + "__to__" + rightSYMap.getIdentifier() + "_Sig")
                        );

                        root.addContained(ForSyDeHierarchy.Visualizable.enforce(newSYSignal));

                        // Find the connecting register between the last executable of left group and the first of right group

                        // Find the connecting register between the last executable of left group and the first of right group
                        String leftFuncID = bestPartition.get(i).get(bestPartition.get(i).size() - 1);
                        String rightFuncID = bestPartition.get(i + 1).get(0);

                        // Use the original `executables` list to directly get the objects
                        FunctionLikeEntity leftFunc = executables.stream()
                                .filter(f -> f.getIdentifier().equals(leftFuncID))
                                .findFirst()
                                .orElseThrow();

                        FunctionLikeEntity rightFunc = executables.stream()
                                .filter(f -> f.getIdentifier().equals(rightFuncID))
                                .findFirst()
                                .orElseThrow();


                        for (EdgeInfo e1 : systemGraph.outgoingEdgesOf(leftFunc.getViewedVertex())) {
                            final Vertex v1 = systemGraph.getEdgeTarget(e1);
                            for (EdgeInfo e2 : systemGraph.incomingEdgesOf(rightFunc.getViewedVertex())) {
                                final Vertex v2 = systemGraph.getEdgeSource(e2);
                                if (v1.getIdentifier().equals(v2.getIdentifier())
                                        && ForSyDeHierarchy.RegisterLike.tryView(systemGraph, v1).isPresent()) {

                                    ForSyDeHierarchy.GreyBox.enforce(newSYSignal).addContained(
                                            ForSyDeHierarchy.Visualizable.enforce(systemGraph, v1)
                                    );

                                    transformedGraph.connect(
                                            newSYSignal.getViewedVertex(), v1,
                                            "producer", "input", ForSyDeHierarchy.EdgeTraits.VisualConnection
                                    );
                                    transformedGraph.connect(
                                            v1, newSYSignal.getViewedVertex(),
                                            "output", "consumers", ForSyDeHierarchy.EdgeTraits.VisualConnection
                                    );
                                }
                            }
                        }

                        // Connect the signal as producer/consumer between new SYMaps
                        transformedGraph.connect(
                                leftSYMap.getViewedVertex(), newSYSignal.getViewedVertex(),
                                "output", "producer", ForSyDeHierarchy.EdgeTraits.SYNetworkEdge
                        );
                        transformedGraph.connect(
                                newSYSignal.getViewedVertex(), rightSYMap.getViewedVertex(),
                                "consumers", "input", ForSyDeHierarchy.EdgeTraits.SYNetworkEdge
                        );

                    //}
                        //-----------------------------------------------------------------------------------

                        leftSYMap.getViewedVertex().addPort("mapSplitTransformedOut");
                        leftSYMap.outputPorts(List.of("mapSplitTransformedOut"));
                        //Connecting the incoming and outgoing edges of the combfunction of the newSYMap which is the executable
                        for (EdgeInfo e : systemGraph.outgoingEdgesOf(getOrderedChildren(transformedGraph, leftSYMap).get(0).getViewedVertex())) {
                            final Vertex dstVertex = transformedGraph.getEdgeTarget(e);
                            if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.VisualContainment)) continue;
                            if (e.getSourcePort().get().equals("combFunctions")) continue;
                            if (e.getSourcePort().get().equals("kernels")) continue;
                            if (ForSyDeHierarchy.MapV.tryView(transformedGraph, getOrderedChildren(transformedGraph, leftSYMap).get(0).getViewedVertex()).isPresent()) {
                                if (!(dstVertex == getOrderedChildren(systemGraph, ForSyDeHierarchy.MapV.tryView(transformedGraph, getOrderedChildren(transformedGraph, leftSYMap).get(0).getViewedVertex()).get()).get(0).getViewedVertex())) {
                                    transformedGraph.connect(getOrderedChildren(transformedGraph, leftSYMap).get(0), leftSYMap, e.getSourcePort().get(), "mapSplitTransformedOut", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                }
                            }
                            if (ForSyDeHierarchy.InterleaveV.tryView(transformedGraph, getOrderedChildren(transformedGraph, leftSYMap).get(0).getViewedVertex()).isPresent()) {
                                // if (!(dstVertex == getOrderedChildren(systemGraph, ForSyDeHierarchy.MapV.tryView(transformedGraph, getOrderedChildren(transformedGraph, leftSYMap).get(0).getViewedVertex()).get()).get(0).getViewedVertex())) {
                                transformedGraph.connect(getOrderedChildren(transformedGraph, leftSYMap).get(0), leftSYMap, e.getSourcePort().get(), "mapSplitTransformedOut", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                // }
                            }
                            if (ForSyDeHierarchy.ReduceV.tryView(transformedGraph, getOrderedChildren(transformedGraph, leftSYMap).get(0).getViewedVertex()).isPresent()) {
                                if (!(dstVertex == getOrderedChildren(systemGraph, ForSyDeHierarchy.ReduceV.tryView(transformedGraph, getOrderedChildren(transformedGraph, leftSYMap).get(0).getViewedVertex()).get()).get(0).getViewedVertex())) {
                                    transformedGraph.connect(getOrderedChildren(transformedGraph, leftSYMap).get(0), leftSYMap, e.getSourcePort().get(), "mapSplitTransformedOut", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                }
                            }
                            if (ForSyDeHierarchy.SYMap.tryView(transformedGraph, getOrderedChildren(transformedGraph, leftSYMap).get(0).getViewedVertex()).isPresent()) {
                                if (!(dstVertex == getOrderedChildren(systemGraph, ForSyDeHierarchy.SYMap.tryView(transformedGraph, getOrderedChildren(transformedGraph, leftSYMap).get(0).getViewedVertex()).get()).get(0).getViewedVertex())) {
                                    transformedGraph.connect(getOrderedChildren(transformedGraph, leftSYMap).get(0), leftSYMap, e.getSourcePort().get(), "mapSplitTransformedOut", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                }
                            }
                        }
                        //--------------------------------------------------------

                        //----------------To be adjusted for parametrized Map-Split-----------------
                        rightSYMap.getViewedVertex().addPort("mapSplitTransformedIn");
                        rightSYMap.inputPorts(List.of("mapSplitTransformedIn"));
                        //Connecting the incoming and outgoing edges of the combfunction of the newSYMap which is the executable
                        for (EdgeInfo e : systemGraph.incomingEdgesOf(getOrderedChildren(transformedGraph, rightSYMap).get(0).getViewedVertex())) {
                            final Vertex srcVertex = transformedGraph.getEdgeSource(e);
                            if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.VisualContainment)) continue;
                            if (e.getSourcePort().get().equals("combFunctions")) continue;
                            if (e.getSourcePort().get().equals("kernels")) continue;
                            if (ForSyDeHierarchy.MapV.tryView(transformedGraph, getOrderedChildren(transformedGraph, rightSYMap).get(0).getViewedVertex()).isPresent()) {
                                if (!(srcVertex == getOrderedChildren(systemGraph, ForSyDeHierarchy.MapV.tryView(transformedGraph, getOrderedChildren(transformedGraph, rightSYMap).get(0).getViewedVertex()).get()).get(0).getViewedVertex())) {
                                    transformedGraph.connect(rightSYMap, getOrderedChildren(transformedGraph, rightSYMap).get(0), "mapSplitTransformedIn", e.getTargetPort().get(), ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                }
                            }
                            if (ForSyDeHierarchy.InterleaveV.tryView(transformedGraph, getOrderedChildren(transformedGraph, rightSYMap).get(0).getViewedVertex()).isPresent()) {
                                // if (!(srcVertex == getOrderedChildren(systemGraph, ForSyDeHierarchy.InterleaveV.tryView(transformedGraph, getOrderedChildren(transformedGraph, rightSYMap).get(0).getViewedVertex()).get()).get(0).getViewedVertex())) {
                                transformedGraph.connect(rightSYMap, getOrderedChildren(transformedGraph, rightSYMap).get(0), "mapSplitTransformedIn", e.getTargetPort().get(), ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                //}
                            }
                            if (ForSyDeHierarchy.ReduceV.tryView(transformedGraph, getOrderedChildren(transformedGraph, rightSYMap).get(0).getViewedVertex()).isPresent()) {
                                if (!(srcVertex == getOrderedChildren(systemGraph, ForSyDeHierarchy.ReduceV.tryView(transformedGraph, getOrderedChildren(transformedGraph, rightSYMap).get(0).getViewedVertex()).get()).get(0).getViewedVertex())) {
                                    transformedGraph.connect(rightSYMap, getOrderedChildren(transformedGraph, rightSYMap).get(0), "mapSplitTransformedIn", e.getTargetPort().get(), ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                }
                            }
                            if (ForSyDeHierarchy.SYMap.tryView(transformedGraph, getOrderedChildren(transformedGraph, rightSYMap).get(0).getViewedVertex()).isPresent()) {
                                if (!(srcVertex == getOrderedChildren(systemGraph, ForSyDeHierarchy.SYMap.tryView(transformedGraph, getOrderedChildren(transformedGraph, rightSYMap).get(0).getViewedVertex()).get()).get(0).getViewedVertex())) {
                                    transformedGraph.connect(rightSYMap, getOrderedChildren(transformedGraph, rightSYMap).get(0), "mapSplitTransformedIn", e.getTargetPort().get(), ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                }
                            }
                        }
                        newSYSignal.producer("mapSplitTransformedOut", leftSYMap);
                        newSYSignal.addConsumers("mapSplitTransformedIn", rightSYMap);
                        transformedGraph.connect(leftSYMap, newSYSignal, "mapSplitTransformedOut", "producer", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                        transformedGraph.connect(newSYSignal, rightSYMap, "consumers", "mapSplitTransformedIn", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                    }
                    //Connecting the incoming edges to the original vertex to the first mapSY inside the Map-Split chain,
                    //and the outgoing edges to the original vertex to the last one in the chain
                    final SYMapViewer firstSyMap = newSYMaps.get(0);
                    final SYMapViewer lastSYMap = newSYMaps.get(newSYMaps.size() - 1);
                    for (String s : syMap.inputPorts()) {
                        firstSyMap.getViewedVertex().addPort(s);
                    }
                    for (String s : syMap.outputPorts()) {
                        lastSYMap.getViewedVertex().addPort(s);
                    }
                    firstSyMap.inputPorts(syMap.inputPorts());
                    lastSYMap.outputPorts(syMap.outputPorts());


                    outerloop1:
                    for (EdgeInfo e : transformedGraph.incomingEdgesOf(syMap.getViewedVertex())) {
                        if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.BehaviourCompositionEdge)) continue;
                        final Vertex srcVertex = transformedGraph.getEdgeSource(e);
                        if (e.getTargetPort().isPresent()) { //&& (!e.hasTrait(ForSyDeHierarchy.EdgeTraits.SYNetworkEdge) || syMap.getInputPorts().contains(e.getTargetPort().get()))
//                        if (processesAndOrderedExecutables.get(syMap).get(0))
                            firstSyMap.getViewedVertex().addPort(e.getTargetPort().get());
                            if (e.getSourcePort().isPresent()) {
                                for (FunctionLikeEntity executable : processesAndOrderedExecutables.get(syMap)) {
                                    if (srcVertex == executable.getViewedVertex()) {
                                        transformedGraph.connect(getOrderedChildren(transformedGraph, lastSYMap).get(0).getViewedVertex(), lastSYMap.getViewedVertex(), e.getSourcePort().get(), e.getTargetPort().get(), ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                        continue outerloop1;
                                    }
                                }
                                for (Trait t : e.edgeTraits) {
                                    transformedGraph.connect(srcVertex, firstSyMap.getViewedVertex(), e.getSourcePort().get(), e.getTargetPort().get(), (EdgeTrait) t);
                                }
                            } else {
                                for (Trait t : e.edgeTraits) {
                                    transformedGraph.connect(srcVertex, firstSyMap.getViewedVertex(), null, e.getTargetPort().get(), (EdgeTrait) t);
                                }
                            }
                        } else {
                            if (e.getSourcePort().isPresent()) {
                                for (SYMap newers : newSYMaps) {
                                    for (Trait t : e.edgeTraits) {
                                        transformedGraph.connect(srcVertex, newers.getViewedVertex(), e.getSourcePort().get(), (EdgeTrait) t);
                                    }
                                }
                            } else {
                                for (SYMap newers : newSYMaps) {
                                    for (Trait t : e.edgeTraits) {
                                        transformedGraph.connect(srcVertex, newers.getViewedVertex(), (EdgeTrait) t);
                                    }
                                }
                            }
                        }

                    }
                    outerloop2:
                    for (EdgeInfo e : transformedGraph.outgoingEdgesOf(syMap.getViewedVertex())) {
                        if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.VisualContainment)) continue;
                        if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.BehaviourCompositionEdge)) continue;
                        final Vertex dstVertex = transformedGraph.getEdgeTarget(e);
                        if (e.getSourcePort().isPresent()) {
                            lastSYMap.getViewedVertex().addPort(e.getSourcePort().get());
                            if (e.getTargetPort().isPresent()) {
                                for (FunctionLikeEntity executable : processesAndOrderedExecutables.get(syMap)) {
                                    if (dstVertex == executable.getViewedVertex()) {
                                        transformedGraph.connect(firstSyMap.getViewedVertex(), getOrderedChildren(transformedGraph, firstSyMap).get(0).getViewedVertex(), e.getSourcePort().get(), e.getTargetPort().get(), ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                        continue outerloop2;
                                    }
                                }
                                for (Trait t : e.edgeTraits) {
                                    transformedGraph.connect(lastSYMap.getViewedVertex(), dstVertex, e.getSourcePort().get(), e.getTargetPort().get(), (EdgeTrait) t);
                                }
                            } else {
                                for (Trait t : e.edgeTraits) {
                                    transformedGraph.connect(lastSYMap.getViewedVertex(), dstVertex, e.getSourcePort().get(), (EdgeTrait) t);
                                }
                            }
                        } else {
                            if (e.getTargetPort().isPresent()) {
                                for (SYMap newers : newSYMaps) {
                                    for (Trait t : e.edgeTraits) {
                                        transformedGraph.connect(newers.getViewedVertex(), dstVertex, null, e.getTargetPort().get(), (EdgeTrait) t);
                                    }
                                }
                            } else {
                                for (SYMap newers : newSYMaps) {
                                    for (Trait t : e.edgeTraits) {
                                        transformedGraph.connect(newers.getViewedVertex(), dstVertex, (EdgeTrait) t);
                                    }
                                }
                            }
                        }
                    }

                    //Removing signals inside mapSY
//                for (int i = 0; i < processesAndOrderedExecutables.get(syMap).size() - 1; i++) {
//                    final Executable exe1 = processesAndOrderedExecutables.get(syMap).get(i);
//                    final Executable exe2 = processesAndOrderedExecutables.get(syMap).get(i + 1);
//                    for (EdgeInfo e1 : SystemGraph.outgoingEdgesOf(exe1.getViewedVertex())) {
//                        final Vertex v1 = SystemGraph.getEdgeTarget(e1);
//                        for (EdgeInfo e2 : SystemGraph.incomingEdgesOf(exe2.getViewedVertex())) {
//                            final Vertex v2 = SystemGraph.getEdgeSource(e2);
//                            if (v1.getIdentifier() == v2.getIdentifier() && DataBlock.conforms(v1)) {
//                                transformedGraph.removeVertex(v1);
//                            }
//                        }
//                    }
//                }
                    //Removing the original mapSY as well as hierarchy inside it besides the signals.
                    transformedGraph.removeVertex(syMap.getViewedVertex());
                    mapSplitTransformedGraphs.add(transformedGraph);
                    //Writing the transformed graphs
//                forSyDeModelHandler.writeModel(transformedGraph, syMap.getIdentifier() + "_transformed.fiodl");
//                forSyDeModelHandler.writeModel(transformedGraph, syMap.getIdentifier() + "_transformed.kgt");
                }

            }
        }
        return mapSplitTransformedGraphs;
    }

    public long computeCostPerElementForExecutable(FunctionLikeEntity executable, Map<String, Double> operationCycles) {
        Set<InstrumentedBehaviour> basicFuncs = new HashSet<>();
        Queue<FunctionLikeEntity> queue = new LinkedList<>();
        queue.add(executable);

        while (!queue.isEmpty()) {
            FunctionLikeEntity current = queue.poll();

            if (ForSyDeHierarchy.HasANSICImplementations.tryView(current).isPresent() ||
                    ForSyDeHierarchy.InterleaveV.tryView(current).isPresent()) {
                basicFuncs.add(ForSyDeHierarchy.InstrumentedBehaviour.enforce(current));
            } else {
                if (ForSyDeHierarchy.MapV.tryView(current).isPresent()) {
                    for (FunctionLikeEntity k : ForSyDeHierarchy.MapV.tryView(current).get().kernels()) {
                        queue.add(k);
                    }
                } else if (ForSyDeHierarchy.ReduceV.tryView(current).isPresent()) {
                    for (FunctionLikeEntity k : ForSyDeHierarchy.ReduceV.tryView(current).get().kernels()) {
                        queue.add(k);
                    }
                }
            }
        }

        long costPerElement = 0;

        for (InstrumentedBehaviour func : basicFuncs) {
            Map<String, Map<String, Long>> requirements = func.computationalRequirements();
            for (Map<String, Long> operations : requirements.values()) {
                for (Map.Entry<String, Long> opEntry : operations.entrySet()) {
                    String op = opEntry.getKey();
                    long count = opEntry.getValue();
                    Double cyclesPerOp = operationCycles.get(op);
                    if (cyclesPerOp != null) {
                        costPerElement += (long) (count * cyclesPerOp);
                    } else {
                        System.out.println("Warning: No cycle info for operation: " + op);
                    }
                }
            }
        }

        return costPerElement;
    }

}
