package patternMatch;

import forsyde.io.core.EdgeInfo;
import forsyde.io.core.OpaqueTrait;
import forsyde.io.core.SystemGraph;
import forsyde.io.core.Vertex;
import forsyde.io.lib.hierarchy.ForSyDeHierarchy;
import forsyde.io.lib.hierarchy.behavior.BehaviourEntity;
import forsyde.io.lib.hierarchy.behavior.FunctionLikeEntity;
import forsyde.io.lib.hierarchy.behavior.moc.sy.SYMap;
import forsyde.io.lib.hierarchy.behavior.moc.sy.SYMapViewer;
import forsyde.io.lib.hierarchy.behavior.moc.sy.SYSignalViewer;
import forsyde.io.lib.hierarchy.behavior.parallel.MapV;
import forsyde.io.lib.hierarchy.behavior.parallel.ReduceV;
import forsyde.io.lib.hierarchy.behavior.parallel.Vectorizable;
import forsyde.io.lib.hierarchy.behavior.parallel.VectorizableViewer;
import forsyde.io.lib.hierarchy.implementation.functional.InstrumentedBehaviour;
import forsyde.io.lib.hierarchy.visualization.*;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

public class MapsyMapvTransformer implements Transformer {

    private final Map<String, Double> operationCycles;
    public MapsyMapvTransformer(Map<String, Double> operationCycles) {
        this.operationCycles = operationCycles;
    }

    @Override
    public Set<SystemGraph> applyTransform(SystemGraph systemGraph) {
        int maxNumCore = 8;
        int minBandwidth = 4; // minimum bandwidth in bytes
        int max_num_hop_bus = 1; // the worst-case number of hops (for NoC) or bus transfers (for a multi-bus system)
        int hop_bus_Transfer = 8; // represents the per-hop (NoC) or per-bus transfer (multi-bus) delay

        Set<SystemGraph> mapsyMapvTransformedGraphs = new HashSet<>();
        // Synchronous processes recognition
        List<SYMapViewer> syMaps = new ArrayList<>();
        AtomicInteger apply = new AtomicInteger();

        // traverse the vertices to find the vectorizables
        final Set<Vectorizable> vectorizables = new HashSet<>();
        for (Vertex v : systemGraph.vertexSet()) {
            ForSyDeHierarchy.Vectorizable.tryView(systemGraph, v).ifPresent(vectorizables::add);
        }

        // traverse the vertices
        Map<SYMap, List<BehaviourEntity>> processesAndOrderedExecutables = new HashMap<>();
        for (Vertex v : systemGraph.vertexSet()) {

            // SY vertices recognition
            ForSyDeHierarchy.SYMap.tryView(systemGraph, v).ifPresent(syMap -> {
                syMaps.add(syMap);

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
                    final Set<FunctionLikeEntity> combFunctions = syMap.combFunctions();

                    // MapSY-MapV pattern recognition
                    if (combFunctions.size() == 1) {
                        for (FunctionLikeEntity combs : combFunctions) {
                            ForSyDeHierarchy.MapV.tryView(combs).ifPresent(mapV -> {
                                MapV mapInside = mapV;
                                System.out.println("\n==================================================");
                                System.out.println("        MapSY-MapV Pattern Detected");
                                System.out.println("==================================================");
                                System.out.printf("%-25s : %s%n", "SYMap Vertex", syMap.getIdentifier());
                                System.out.printf("%-25s : %s%n", "Matched MapV Function", mapV.getIdentifier());

                                GreyBoxViewer root = null;
                                for (EdgeInfo e : systemGraph.incomingEdgesOf(v)) {
                                    if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.VisualContainment)) {
                                        final Vertex rootV = systemGraph.getEdgeSource(e);
                                        root = ForSyDeHierarchy.GreyBox.tryView(systemGraph, rootV).get();
                                    }
                                }
                                //Applying the transformation on only one of the vertices of a parallel structure
                                Vertex parStructParent = null;
                                apply.set(1);
                                for (EdgeInfo e: systemGraph.incomingEdgesOf(v)){
                                    if (apply.getPlain() == 1) {
                                        if (e.hasTrait(new OpaqueTrait("annotationEdge"))){
                                            if (systemGraph.getEdgeSource(e).hasTrait(new OpaqueTrait("ParStruct"))) {
                                                if (systemGraph.getEdgeSource(e).getProperty("vectorProcesses") instanceof ArrayList<?> list) {
                                                    var curIdx = list.indexOf(v.getIdentifier());
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
                                Vertex srcSignal = null;
                                List<Integer> sigChildDimentions = new ArrayList<>();
                                for (EdgeInfo e : systemGraph.incomingEdgesOf(v)) {
                                    if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.SYNetworkEdge)) {
                                        srcSignal = systemGraph.queryVertex(e.getSource()).orElse(null);
                                        if (srcSignal != null) {
                                            for (EdgeInfo ed : systemGraph.outgoingEdgesOf(srcSignal)) {
                                                if (ed.hasTrait(ForSyDeHierarchy.EdgeTraits.VisualContainment)) {
                                                    Vertex sigChild = systemGraph.queryVertex(ed.getTarget()).orElse(null);
                                                    if (sigChild != null && ForSyDeHierarchy.Vectorizable.tryView(systemGraph, sigChild).isPresent()) {
                                                        sigChildDimentions = ForSyDeHierarchy.Vectorizable.tryView(systemGraph, sigChild).get().dimensions();
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }

                                int dataSize = 1;
                                for (int dim : sigChildDimentions) {
                                    dataSize *= dim;
                                }

                                // check if dimensions were found
                                if (sigChildDimentions.isEmpty()) {
                                    System.out.println("Warning: No vectorizable dimensions found. Skipping transformation.");
                                    return;
                                }

                                // ++++++++++++++++++++++++++++ Original vertex v cost calculation ++++++++++++++++++++++++++++++++++++
                                long C_lhs = 0;
                                long C_rhs = 0;
                                Map<Integer, Map<String, Long>> detailedStageCosts = new HashMap<>();

                                Set<FunctionLikeEntity> kernels = new HashSet<>(mapInside.kernels());
                                Set<InstrumentedBehaviour> basicFuncs = new HashSet<>();
                                Queue<FunctionLikeEntity> queue = new LinkedList<>(kernels);

                                while (!queue.isEmpty()) {
                                    FunctionLikeEntity kers = queue.poll();

                                    if (ForSyDeHierarchy.HasANSICImplementations.tryView(kers).isPresent() |
                                            ForSyDeHierarchy.InterleaveV.tryView(kers).isPresent()) {
                                        basicFuncs.add(ForSyDeHierarchy.InstrumentedBehaviour.enforce(kers));
                                    } else {
                                        if (ForSyDeHierarchy.MapV.tryView(kers).isPresent()) {
                                            for (FunctionLikeEntity k : ForSyDeHierarchy.MapV.tryView(kers).get().kernels()) {
                                                if (kernels.add(k)) queue.add(k);
                                            }
                                        } else if (ForSyDeHierarchy.ReduceV.tryView(kers).isPresent()) {
                                            for (FunctionLikeEntity k : ForSyDeHierarchy.ReduceV.tryView(kers).get().kernels()) {
                                                if (kernels.add(k)) queue.add(k);
                                            }
                                        }
                                    }
                                }

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

                                        computationCostsPerElement.merge(implementation, costPerElement, Long::sum);
                                        computationCostsTotal.merge(implementation, costPerElement * dataSize, Long::sum);
                                    }
                                }

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

                                System.out.println("\nComputation Costs for Transformed Graph:");

                                // ++++++++++++++++++++++++++++ Transformed vertex v cost calculation (Non-Parameterized) +++++++++++++++++++++++++++++++++

                                int currentGroupSize = sigChildDimentions.get(0);  // 1 element per group → max parallelism

                                System.out.println("\nComputation Cost for Transformed Graph with Full Parallelism (Group Size = " + currentGroupSize + ")");

                                long C_part_dist = 0;
                                long C_parallel = 0;
                                long C_coll_recomb = 0;
                                long C_distribute = 0;
                                long C_collect = 0;

                                Double loadCycles = operationCycles.get("load");
                                Double storeCycles = operationCycles.get("store");

                                if (loadCycles == null) {
                                    System.out.println("Warning: No cycle info for 'load' operation. Using default 1.0");
                                    loadCycles = 1.0;
                                }
                                if (storeCycles == null) {
                                    System.out.println("Warning: No cycle info for 'store' operation. Using default 1.0");
                                    storeCycles = 1.0;
                                }

                                C_part_dist = (long) (dataSize * 2 * (loadCycles + storeCycles));

                                // C_parallel: max cost among implementations
                                for (Map.Entry<String, Long> entry : computationCostsTotal.entrySet()) {
                                    long totalCost = entry.getValue();
                                    long c_parallel = currentGroupSize > 0 ? totalCost / currentGroupSize : totalCost;
                                    C_parallel = Math.max(C_parallel, c_parallel);
                                }

                                C_coll_recomb = C_part_dist;

                                if (currentGroupSize > 0 && minBandwidth > 0) {
                                    int bytesPerElement = 4;
                                    int dataPerGroup = dataSize / currentGroupSize;
                                    double communicationTimePerGroup = ((double) (dataPerGroup * bytesPerElement)) / minBandwidth;
                                    double latencyPerGroup = max_num_hop_bus * hop_bus_Transfer;
                                    C_distribute = (long) (currentGroupSize * (communicationTimePerGroup + latencyPerGroup));
                                }
                                C_collect = C_distribute;

                                List<Long> stageCosts = Arrays.asList(C_part_dist, C_parallel, C_coll_recomb, C_distribute, C_collect);
                                C_rhs = Collections.max(stageCosts);

                                // === Print results ===
                                System.out.println("\n--------------------------------------------------");
                                System.out.println("C_rhs for Maximum Parallelism (Non-Parameterized)");
                                System.out.println("--------------------------------------------------");
                                System.out.printf("%-30s | %-20d%n", "Number of Parallel Branches", currentGroupSize);
                                System.out.printf("%-30s | %-20d%n", "C_rhs (cycles)", C_rhs);
                                System.out.println("--------------------------------------------------");

                                System.out.println("\nDetailed Cost Breakdown:");
                                System.out.println("--------------------------------------------------------------");
                                System.out.printf("%-20s | %-10d%n", "C_part_dist", C_part_dist);
                                System.out.printf("%-20s | %-10d%n", "C_parallel", C_parallel);
                                System.out.printf("%-20s | %-10d%n", "C_coll_recomb", C_coll_recomb);
                                System.out.printf("%-20s | %-10d%n", "C_distribute", C_distribute);
                                System.out.printf("%-20s | %-10d%n", "C_collect", C_collect);
                                System.out.println("--------------------------------------------------------------");


                                //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                                if (C_lhs > C_rhs && apply.getPlain() == 1 && !(maxNumCore < sigChildDimentions.get(0))){
                                    System.out.println("Applying MapSY-MapV transformation (C_rhs = " + C_rhs + " < C_lhs = " + C_lhs + ")");

                                    // MapSY-MapV transformation application
                                    Random random = new Random();
                                    int rand = random.nextInt(1000);
                                    final SystemGraph transformedGraph = new NamedSystemGraph(syMap.getIdentifier() + rand + "_T");
                                    transformedGraph.mergeInPlace(systemGraph);
                                    final List<SYMapViewer> newSYMaps = new ArrayList<>();
                                    // this need to be done since the root was viewed in the wrong system graph
                                    root = ForSyDeHierarchy.GreyBox.tryView(transformedGraph, root.getViewedVertex()).get();
                                    //Generating new SYMaps: mapSY(f1), mapSY(f2), ...

                                                        final Vertex parallelStruct = transformedGraph.newVertex(v.getIdentifier() + mapInside.getIdentifier());
                                                        parallelStruct.addTraits(new OpaqueTrait("ParStruct"));
                                                        parallelStruct.addPort("tuplets");
                                                        var parList = new ArrayList<String>();

                                                        for (int i = 0; i < sigChildDimentions.get(0); i++) {

                                                            final SYMapViewer newSYMap = ForSyDeHierarchy.SYMap.enforce(transformedGraph, transformedGraph.newVertex(v.getIdentifier() + "TSY" + i));
                                                            final GreyBoxViewer newSYMapGB = ForSyDeHierarchy.GreyBox.enforce(newSYMap);
                                                            for (FunctionLikeEntity child : getOrderedChildren(transformedGraph, mapInside)) {
                                                                newSYMap.addCombFunctions(child);
                                                                newSYMapGB.addContained(ForSyDeHierarchy.Visualizable.enforce(child));
                                                                //getOrderedChildren(transformedGraph, ForSyDeHierarchy.MapV.enforce(getOrderedChildren(transformedGraph, newSYMap).get(0)))
                                                            }
                                                            ForSyDeHierarchy.GreyBox.tryView(mapInside).ifPresent(greyBoxViewer -> {
                                                                for (Visualizable visualizable : greyBoxViewer.contained()) {
                                                                    newSYMapGB.addContained(visualizable);
                                                                }
                                                            });

                                                            transformedGraph.connect(parallelStruct, newSYMap.getViewedVertex(), "tuplets", new OpaqueTrait("annotationEdge"));
                                                            //Making the connections between newSYMaps and their hierarchy below
                                                            parList.add(newSYMap.getIdentifier());
                                                            newSYMaps.add(newSYMap);
                                                            root.addContained(newSYMapGB);

                                                        }
                                                        parallelStruct.putProperty("vectorProcesses", parList);

                                    Vertex dstSignal = null;
                                    for (EdgeInfo e : transformedGraph.outgoingEdgesOf(v)) {
                                        if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.SYNetworkEdge)) {
                                            dstSignal = transformedGraph.queryVertex(e.getTarget()).get();
                                        }
                                    }
                                    //Generating the zipxSY and unZipxSY
                                    final SYMapViewer newZipxSY = ForSyDeHierarchy.SYMap.enforce(transformedGraph, transformedGraph.newVertex(v.getIdentifier() + "zipxSYtransformedSY"));
                                    //newZipxSY.insertCombFunctionsPort(transformedGraph, concat);
                                    var zip = ForSyDeHierarchy.InstrumentedBehaviour.enforce(newZipxSY);
                                    zip.computationalRequirements(Map.of("default", Map.of("merge", 1L)));
                                    GreyBoxViewer newZipxSYGB = ForSyDeHierarchy.GreyBox.enforce(newZipxSY);
                                    root.addContained(newZipxSYGB);
                                    //newZipxSYGB.insertContainedPort(transformedGraph, Visualizable.enforce(concat));
                                    final SYMapViewer newunZipxSY = ForSyDeHierarchy.SYMap.enforce(transformedGraph, transformedGraph.newVertex(v.getIdentifier() + "unZipxSYtransformedSY"));
                                    //newunZipxSY.insertCombFunctionsPort(transformedGraph, concat);
                                    var unzip = ForSyDeHierarchy.InstrumentedBehaviour.enforce(newunZipxSY);
                                    unzip.computationalRequirements(Map.of("default", Map.of("split", 1L)));
                                    GreyBox newunZipxSYGB = ForSyDeHierarchy.GreyBox.enforce(newunZipxSY);
                                    root.addContained(newunZipxSYGB);
                                    //newunZipxSYGB.insertContainedPort(transformedGraph, Visualizable.enforce(concat));

                                    //Connecting the input ports of the original mapSY to the newunZipxSY and the output ports to the new zipxSY
                                    for (String s : syMap.inputPorts()) {
                                        newZipxSY.getViewedVertex().addPort(s);
                                    }
                                    for (String s : syMap.outputPorts()) {
                                        newunZipxSY.getViewedVertex().addPort(s);
                                    }
                                    newZipxSY.inputPorts(syMap.inputPorts());
                                    newunZipxSY.outputPorts(syMap.outputPorts());

                                    ForSyDeHierarchy.SYSignal.tryView(transformedGraph, srcSignal).ifPresent(srcSignalViewer -> {
                                        srcSignalViewer.addConsumers(newZipxSY.inputPorts().get(0), newZipxSY);
                                    });
                                    transformedGraph.connect(srcSignal, newZipxSY.getViewedVertex(), "consumers", newZipxSY.inputPorts().get(0), ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                    if (ForSyDeHierarchy.SYSignal.tryView(transformedGraph, dstSignal).isPresent()) {
                                        ForSyDeHierarchy.SYSignal.tryView(transformedGraph, dstSignal).get().producer(newunZipxSY.outputPorts().get(0), newunZipxSY);
                                    }
                                    transformedGraph.connect(newunZipxSY.getViewedVertex(), dstSignal, newunZipxSY.outputPorts().get(0), "producer", ForSyDeHierarchy.EdgeTraits.VisualConnection);

                                    //Generating signals between newZipxSY and the newSYMaps as well as signals between newSYMaps and newunZipxSY
                                    for (int i = 0; i < newSYMaps.size(); i++) {
                                        final SYMapViewer leftSYMap = newZipxSY;
                                        final SYMapViewer rightSYMap = newSYMaps.get(i);
                                        final SYSignalViewer newSYSignal = ForSyDeHierarchy.SYSignal.enforce(transformedGraph, transformedGraph.newVertex(leftSYMap.getIdentifier() + rightSYMap.getIdentifier() + "Sig"));
                                        //Inserting hierarchy below for the newSYSignals
                                        if (sigChildDimentions.size() > 1) {
                                            final VectorizableViewer newVectorizable = ForSyDeHierarchy.Vectorizable.enforce(transformedGraph, transformedGraph.newVertex(newSYSignal.getIdentifier() + "Vec"));
                                            List<Integer> newVectorizableDimentions = new ArrayList<>();
                                            for (int j = 1; j < sigChildDimentions.size(); j++) {
                                                newVectorizableDimentions.add(sigChildDimentions.get(j));
                                            }
                                            newVectorizable.dimensions(newVectorizableDimentions);
                                            final VisualizableWithPropertiesViewer visualizableWithProperties = ForSyDeHierarchy.VisualizableWithProperties.enforce(newVectorizable);
                                            ForSyDeHierarchy.GreyBox.enforce(newSYSignal).addContained(visualizableWithProperties);
                                            visualizableWithProperties.visualizedPropertiesNames(List.of("dimensions"));

                                            newVectorizable.getViewedVertex().addPort("input");
                                            newVectorizable.getViewedVertex().addPort("output");
                                            //Connecting the input and output ports of the newSignal to its hierarchy below
                                            transformedGraph.connect(newSYSignal, newVectorizable, "producer", "input", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                            transformedGraph.connect(newVectorizable, newSYSignal, "output", "consumers", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                        }
                                        root.addContained(ForSyDeHierarchy.Visualizable.enforce(newSYSignal));
                                        leftSYMap.getViewedVertex().addPort("newZipxSYTransformedOut");
                                        leftSYMap.outputPorts(List.of("newZipxSYTransformedOut"));
                                        //--------------------------------------------------------
                                        rightSYMap.getViewedVertex().addPort("newSYSignalTransformedIn");
                                        rightSYMap.inputPorts(List.of("newSYSignalTransformedIn"));
                                        newSYSignal.producer("newZipxSYTransformedOut", leftSYMap);
                                        newSYSignal.addConsumers("newSYSignalTransformedIn", rightSYMap);
                                        transformedGraph.connect(leftSYMap, newSYSignal, "newZipxSYTransformedOut", "producer", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                        transformedGraph.connect(newSYSignal, rightSYMap, "consumers", "newSYSignalTransformedIn", ForSyDeHierarchy.EdgeTraits.VisualConnection);

                                    }
                                    for (int i = 0; i < newSYMaps.size(); i++) {
                                        final SYMapViewer leftSYMap = newSYMaps.get(i);
                                        final SYMapViewer rightSYMap = newunZipxSY;
                                        final SYSignalViewer newSYSignal = ForSyDeHierarchy.SYSignal.enforce(transformedGraph, transformedGraph.newVertex(leftSYMap.getIdentifier() + rightSYMap.getIdentifier() + "Sig"));
                                        //Inserting hierarchy below for the newSYSignals
                                        if (sigChildDimentions.size() > 1) {
                                            final VectorizableViewer newVectorizable = ForSyDeHierarchy.Vectorizable.enforce(transformedGraph, transformedGraph.newVertex(newSYSignal.getIdentifier() + "Vec"));
                                            List<Integer> newVectorizableDimentions = new ArrayList<>();
                                            for (int j = 1; j < sigChildDimentions.size(); j++) {
                                                newVectorizableDimentions.add(sigChildDimentions.get(j));
                                            }
                                            newVectorizable.dimensions(newVectorizableDimentions);
                                            final VisualizableWithPropertiesViewer visualizableWithProperties = ForSyDeHierarchy.VisualizableWithProperties.enforce(newVectorizable);
                                            ForSyDeHierarchy.GreyBox.enforce(newSYSignal).addContained(ForSyDeHierarchy.VisualizableWithProperties.enforce(newVectorizable));
                                            visualizableWithProperties.visualizedPropertiesNames(List.of("dimensions"));
                                            newVectorizable.getViewedVertex().addPort("input");
                                            newVectorizable.getViewedVertex().addPort("output");
                                            //Connecting the input and output ports of the newSignal to its hierarchy below
                                            transformedGraph.connect(newSYSignal, newVectorizable, "producer", "input", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                            transformedGraph.connect(newVectorizable, newSYSignal, "output", "consumers", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                        }
                                        root.addContained(ForSyDeHierarchy.Visualizable.enforce(newSYSignal));
                                        leftSYMap.getViewedVertex().addPort("newSYSignalTransformedOut");
                                        leftSYMap.outputPorts(List.of("newSYSignalTransformedOut"));

                                        //Connecting the incoming and outgoing edges of the combfunction of the newSYMap which are the children of mapInside
                                        //getOrderedChildren(transformedGraph, ForSyDeHierarchy.MapV.enforce(getOrderedChildren(transformedGraph, newSYMaps.get(0)).get(0)))
                                        for (EdgeInfo e : transformedGraph.outgoingEdgesOf(v)) {
                                            final Vertex dstVertex = transformedGraph.getEdgeTarget(e);
                                            if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.VisualContainment)) continue;
                                            if (e.getSourcePort().get().equals("combFunctions")) continue;
//                                        final FunctionLikeEntity child = getOrderedChildren(transformedGraph, mapInside).get(0);
//                                        if (ForSyDeHierarchy.HasANSICImplementations.tryView(child).isPresent()) {
//                                            final List<String> childInputs = ForSyDeHierarchy.HasANSICImplementations.tryView(child).get().inputArgumentPorts();
//                                            final List<String> childOutputs = ForSyDeHierarchy.HasANSICImplementations.tryView(child).get().outputArgumentPorts();
//                                            for (int j = 0; j < childInputs.size(); j++) {
//                                                transformedGraph.connect(newSYMaps.get(i), child, newSYMaps.get(i).inputPorts().get(j), childInputs.get(j), ForSyDeHierarchy.EdgeTraits.BehaviourCompositionEdge, ForSyDeHierarchy.EdgeTraits.VisualConnection);
//                                            }
//                                            for (int j = 0; j < childOutputs.size(); j++) {
//                                                transformedGraph.connect(child, newSYMaps.get(i), childOutputs.get(j), newSYMaps.get(i).outputPorts().get(j), ForSyDeHierarchy.EdgeTraits.BehaviourCompositionEdge, ForSyDeHierarchy.EdgeTraits.VisualConnection);
//                                            }
//                                            if (ForSyDeHierarchy.HasANSICImplementations.tryView(child).get().returnPort().isPresent()) {
//                                                transformedGraph.connect(child, newSYMaps.get(i), ForSyDeHierarchy.HasANSICImplementations.tryView(child).get().returnPort().get(), newSYMaps.get(i).outputPorts().get(newSYMaps.get(i).outputPorts().size() - 1), ForSyDeHierarchy.EdgeTraits.BehaviourCompositionEdge, ForSyDeHierarchy.EdgeTraits.VisualConnection);
//                                            }
//                                        } else {
                                            if (dstVertex == getOrderedChildren(transformedGraph, ForSyDeHierarchy.SYMap.enforce(transformedGraph, v)).get(0).getViewedVertex()) {
                                                transformedGraph.connect(leftSYMap, getOrderedChildren(transformedGraph, leftSYMap).get(0), "newSYSignalTransformedIn", e.getTargetPort().get(), ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                            }
                                            //}
                                        }
                                        for (EdgeInfo e : transformedGraph.incomingEdgesOf(v)) {
                                            final Vertex srcVertex = transformedGraph.getEdgeSource(e);
                                            if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.VisualContainment)) continue;
                                            if (e.getSourcePort().get().equals("combFunctions")) continue;
                                            if (srcVertex == getOrderedChildren(transformedGraph, ForSyDeHierarchy.SYMap.enforce(transformedGraph, v)).get(0).getViewedVertex()) {
                                                int size = getOrderedChildren(transformedGraph, leftSYMap).size();
                                                transformedGraph.connect(getOrderedChildren(transformedGraph, leftSYMap).get(size - 1), leftSYMap, getOrderedChildren(transformedGraph, leftSYMap).get(size - 1).outputPorts().get(0), "newSYSignalTransformedOut", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                                //transformedGraph.connect(getOrderedChildren(transformedGraph, leftSYMap).get(size - 1), leftSYMap, e.getSourcePort().get(), "newSYSignalTransformedOut", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                            }

                                        }

                                        rightSYMap.getViewedVertex().addPort("newunZipxSYTransformedIn");
                                        rightSYMap.inputPorts(List.of("newunZipxSYTransformedIn"));
                                        newSYSignal.producer("newSYSignalTransformedOut", leftSYMap);
                                        newSYSignal.addConsumers("newunZipxSYTransformedIn", rightSYMap);
                                        transformedGraph.connect(leftSYMap, newSYSignal, "newSYSignalTransformedOut", "producer", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                        transformedGraph.connect(newSYSignal, rightSYMap, "consumers", "newunZipxSYTransformedIn", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                    }
                                    //---------------------------------------------------------------------------
                                    //Removing the original mapSY as well as hierarchy inside it besides the signals.
//                                Vertex childV = getOrderedChildren(SystemGraph, SYMap.tryView(v).get()).get(0).getViewedVertex();
//                                    if (parStructParent != null && parStructParent.getProperty("vectorProcesses") instanceof ArrayList<?> list){
////                                        transformedGraph.removeVertex(parStructParent);
////                                        var clonedParStruct = transformedGraph.newVertex(parStructParent.getIdentifier());
//                                        list.remove(0);
//                                    }

                                    transformedGraph.removeVertex(v);
                                    int incomingNonKernels = 0;
                                    for (EdgeInfo e : transformedGraph.incomingEdgesOf(mapInside)) {
                                        boolean isKernel = ForSyDeHierarchy.FunctionLikeEntity.tryView(transformedGraph, transformedGraph.getEdgeSource(e))
                                                .map(functionLikeEntityViewer -> mapInside.kernels().contains(functionLikeEntityViewer))
                                                .orElse(false);
                                        if (!isKernel && e.hasTrait(ForSyDeHierarchy.EdgeTraits.BehaviourCompositionEdge)) {
                                            incomingNonKernels += 1;
                                        }
                                    }
                                    if (incomingNonKernels == 0) {
                                        transformedGraph.removeVertex(mapInside.getViewedVertex());
                                    }

//                                deleteHierarchy(transformedGraph, v);
                                    mapsyMapvTransformedGraphs.add(transformedGraph);
                                } else {
                                    if (C_lhs <= C_rhs) {
                                        System.out.println("C_lhs < C_rhs (C_lhs=" + C_lhs + ", C_rhs=" + C_rhs + ")");
                                    }
                                    if (apply.getPlain() != 1) {
                                        System.out.println("Not the first process in the parallel structure.");
                                    }
                                    if (maxNumCore < sigChildDimentions.get(0)) {
                                        System.out.println("maxNumCore < sigChildDimentions[0] (maxNumCore=" + maxNumCore + ", sigChildDimentions[0]=" + sigChildDimentions.get(0) + ")");
                                    }
                                    System.out.println("Skipping MapSY-MapV transformation.");
                                }
                            });
                        }
                    }

                }
            });
        }
        return mapsyMapvTransformedGraphs;
    }
}
