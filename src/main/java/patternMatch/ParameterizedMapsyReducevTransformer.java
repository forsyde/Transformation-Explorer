//package patternMatch;
//
//import forsyde.io.core.EdgeInfo;
//import forsyde.io.core.OpaqueTrait;
//import forsyde.io.core.SystemGraph;
//import forsyde.io.core.Vertex;
//import forsyde.io.lib.hierarchy.ForSyDeHierarchy;
//import forsyde.io.lib.hierarchy.behavior.FunctionLikeEntity;
//import forsyde.io.lib.hierarchy.behavior.moc.sy.SYMap;
//import forsyde.io.lib.hierarchy.behavior.moc.sy.SYMapViewer;
//import forsyde.io.lib.hierarchy.behavior.moc.sy.SYSignalViewer;
//import forsyde.io.lib.hierarchy.behavior.parallel.*;
//import forsyde.io.lib.hierarchy.implementation.functional.InstrumentedBehaviour;
//import forsyde.io.lib.hierarchy.visualization.GreyBoxViewer;
//import forsyde.io.lib.hierarchy.visualization.VisualizableWithPropertiesViewer;
//
//import java.util.*;
//
//public class ParameterizedMapsyReducevTransformer implements Transformer {
//
//    private final Map<String, Double> operationCycles;
//
//    public ParameterizedMapsyReducevTransformer(Map<String, Double> operationCycles) {
//        this.operationCycles = operationCycles;
//    }
//
//    @Override
//    public Set<SystemGraph> applyTransform(SystemGraph systemGraph) {
//        int maxNumCore = 8;
//        int minBandwidth = 4; // minimum bandwidth in bytes
//        int max_num_hop_bus = 1; // the worst-case number of hops (for NoC) or bus transfers (for a multi-bus system)
//        int hop_bus_Transfer = 8; // represents the per-hop (NoC) or per-bus transfer (multi-bus) delay
//
//        Set<SystemGraph> ParameterizedMapsyReducevTransformedGraphs= new HashSet<>();
//        // Synchronous processes recognition
//        List<SYMap> syMaps = new ArrayList<>();
//        List<MapV> mapVs = new ArrayList<>();
//        List<ReduceV> reduceVs = new ArrayList<>();
//
//        // traverse the vertices to find the vectorizables
//        final Set<Vectorizable> vectorizables = new HashSet<>();
//        for (Vertex v : systemGraph.vertexSet()) {
//            if (ForSyDeHierarchy.Vectorizable.tryView(systemGraph, v).isPresent()) {
//                vectorizables.add(ForSyDeHierarchy.Vectorizable.tryView(systemGraph, v).get());
//            }
//        }
//
//        // traverse the vertices
//        for (Vertex v : systemGraph.vertexSet()) {
//
//            // SY vertices recognition
//            if (ForSyDeHierarchy.SYMap.tryView(systemGraph, v).isPresent()) {
//                syMaps.add(ForSyDeHierarchy.SYMap.tryView(systemGraph, v).get());
//
//                int inPortNum = 0;
//                for (EdgeInfo e : systemGraph.incomingEdgesOf(v)) {
//                    if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.SYNetworkEdge)) {
//                        inPortNum += 1;
//                    }
//                }
//                int outPortNum = 0;
//                for (EdgeInfo e : systemGraph.outgoingEdgesOf(v)) {
//                    if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.SYNetworkEdge)) {
//                        outPortNum += 1;
//                    }
//                }
//
//                //System.out.println(v.getIdentifier() + ", inputs is " + inPortNum + ", and output is " + outPortNum);
//
//                // mapSY recognition
//                if (inPortNum == 1 && outPortNum == 1) {
//                    SYMap syMap = ForSyDeHierarchy.SYMap.tryView(systemGraph, v).get();
//
//                    final Set<FunctionLikeEntity> combFunctions = syMap.combFunctions();
//
//                    // MapSY-MapV pattern recognition
//                    if (combFunctions.size() == 1) {
//                        ReduceV reduceInside = null;
//                        for (FunctionLikeEntity combs : combFunctions) {
//                            if (ForSyDeHierarchy.ReduceV.tryView(combs).isPresent()) {
//                                reduceInside = ForSyDeHierarchy.ReduceV.tryView(combs).get();
//                                System.out.println("\n==================================================");
//                                System.out.println("   MapSY-ReduceV (Parametrized) Pattern Detected");
//                                System.out.println("==================================================");
//                                System.out.printf("%-25s : %s%n", "SYMap Vertex", v.getIdentifier());
//                                System.out.printf("%-25s : %s%n", "Matched ReduceV Function", reduceInside.getIdentifier());
//
//
//                                // Optional: show parent component (e.g., GreyBox visual container)
//                                GreyBoxViewer root = null;
//                                for (EdgeInfo e : systemGraph.incomingEdgesOf(v)) {
//                                    if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.VisualContainment)) {
//                                        final Vertex rootV = systemGraph.getEdgeSource(e);
//                                        root = ForSyDeHierarchy.GreyBox.tryView(systemGraph, rootV).orElse(null);
//                                    }
//                                }
//
//                                System.out.println("--------------------------------------------------\n");
//
//
//                                //Finding the dimensions of the input (which determines the number of mapSYs and is used for cost calculation)
//                                Vertex srcSignal = null;
//                                Vertex dstSignal = null;
//                                List<Integer> srcSigChildDimentions = new ArrayList<>();
//                                List<Integer> dstSigChildDimentions = new ArrayList<>();
//                                Scanner scanner = new Scanner(System.in);
//                                List<Integer> groupSize = new ArrayList<>();
//                                for (EdgeInfo e : systemGraph.incomingEdgesOf(v)) {
//                                    if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.SYNetworkEdge)) {
//                                        srcSignal = systemGraph.queryVertex(e.getSource()).get();
//                                        for (EdgeInfo ed : systemGraph.outgoingEdgesOf(srcSignal)) {
//                                            if (ed.hasTrait(ForSyDeHierarchy.EdgeTraits.VisualContainment)){
//                                                final Vertex sigChild = systemGraph.queryVertex(ed.getTarget()).get();
//                                                if (ForSyDeHierarchy.Vectorizable.tryView(systemGraph, sigChild).isPresent()){
//                                                    srcSigChildDimentions = ForSyDeHierarchy.Vectorizable.tryView(systemGraph, sigChild).get().dimensions();
//                                                    //The first dimension which is the number of rows in the case of a matrix is the number of mapSYs.
////                                                    System.out.println("Please enter the intended size of each process (the dimension of the input signal is "+ srcSigChildDimentions.get(0) + "):");
////                                                    groupSize = scanner.nextInt();
//                                                }
//                                            }
//                                        }
//                                    }
//                                }
//
//                                for(int i = 2; i<= srcSigChildDimentions.get(0); i++) {
//                                    if(srcSigChildDimentions.get(0) % i == 0 && (srcSigChildDimentions.get(0)/i)<= maxNumCore) {
//                                        groupSize.add(i);
//                                    }
//                                }
//
//                                int dataSize = 1;
//                                for (int dim : srcSigChildDimentions) {
//                                    dataSize *= dim;
//                                }
//
//                                int bestGroupSize = -1;
//                                long bestCost = Long.MAX_VALUE;
//                                long C_lhs = 0;
//                                Map<Integer, Long> rhsCostsByGroupSize = new HashMap<>();
//                                Map<Integer, Map<String, Long>> detailedStageCosts = new HashMap<>();
//
//                                if (groupSize == null || groupSize.isEmpty()) {
//                                    System.out.println("\nWarning: No group sizes available. Skipping transformation evaluation.");
//                                } else {
//                                    // ++++++++++++++++++++++++++++ Original vertex v cost calculation ++++++++++++++++++++++++++++++++++++
//                                    Set<FunctionLikeEntity> kernels = new HashSet<>(reduceInside.kernels());
//                                    Set<InstrumentedBehaviour> basicFuncs = new HashSet<>();
//                                    Queue<FunctionLikeEntity> queue = new LinkedList<>(kernels);
//
//                                    while (!queue.isEmpty()) {
//                                        FunctionLikeEntity kers = queue.poll();
//
//                                        if (ForSyDeHierarchy.HasANSICImplementations.tryView(kers).isPresent() |
//                                                ForSyDeHierarchy.InterleaveV.tryView(kers).isPresent()) {
//                                            basicFuncs.add(ForSyDeHierarchy.InstrumentedBehaviour.enforce(kers));
//                                        } else {
//                                            if (ForSyDeHierarchy.MapV.tryView(kers).isPresent()) {
//                                                for (FunctionLikeEntity k : ForSyDeHierarchy.MapV.tryView(kers).get().kernels()) {
//                                                    if (kernels.add(k)) queue.add(k);
//                                                }
//                                            } else if (ForSyDeHierarchy.ReduceV.tryView(kers).isPresent()) {
//                                                for (FunctionLikeEntity k : ForSyDeHierarchy.ReduceV.tryView(kers).get().kernels()) {
//                                                    if (kernels.add(k)) queue.add(k);
//                                                }
//                                            }
//                                        }
//                                    }
//
//                                    Map<String, Long> computationCostsPerElement = new HashMap<>();
//                                    Map<String, Long> computationCostsTotal = new HashMap<>();
//
//                                    for (InstrumentedBehaviour func : basicFuncs) {
//                                        Map<String, Map<String, Long>> requirements = func.computationalRequirements();
//
//                                        for (Map.Entry<String, Map<String, Long>> implEntry : requirements.entrySet()) {
//                                            String implementation = implEntry.getKey();
//                                            Map<String, Long> operations = implEntry.getValue();
//
//                                            long costPerElement = 0;
//
//                                            for (Map.Entry<String, Long> opEntry : operations.entrySet()) {
//                                                String op = opEntry.getKey();
//                                                long count = opEntry.getValue();
//
//                                                Double cyclesPerOp = operationCycles.get(op);
//                                                if (cyclesPerOp != null) {
//                                                    costPerElement += (long) (count * cyclesPerOp);
//                                                } else {
//                                                    System.out.println("Warning: No cycle info for operation: " + op);
//                                                }
//                                            }
//
//                                            computationCostsPerElement.merge(implementation, costPerElement, Long::sum);
//                                            computationCostsTotal.merge(implementation, costPerElement * dataSize, Long::sum);
//                                        }
//                                    }
//
//                                    // === Print original cost table (C_lhs) ===
//                                    System.out.println("\n--------------------------------------------------");
//                                    System.out.println("    C_lhs Per Implementation (Orignal)");
//                                    System.out.println("--------------------------------------------------");
//                                    System.out.printf("%-25s | %-15s%n", "Implementation", "Total Cost");
//                                    System.out.println("--------------------------------------------------");
//
//
//                                    for (String impl : computationCostsTotal.keySet()) {
//                                        long total = computationCostsTotal.getOrDefault(impl, 0L);
//                                        System.out.printf("%-25s | %-15d%n", impl, total);
//                                        C_lhs = Math.max(C_lhs, total);
//                                    }
//
//                                    System.out.println("--------------------------------------------------");
//                                    System.out.printf("%-25s   %-15d%n", "Worst Cost (Max C_lhs)", C_lhs);
//                                    System.out.println("--------------------------------------------------");
//
//                                    // ++++++++++++++++++++++++++++ Transformed vertex v cost calculation +++++++++++++++++++++++++++++++++
//
//                                    System.out.println("\nComputation Costs for Transformed Graph by Partitioning Parameter:");
//
//
//
//                                    for (int j = 0; j < groupSize.size(); j++) {
//                                        int currentGroupSize = groupSize.get(j);
//
//                                        long C_reshape_part = 0;
//                                        long C_parallel = 0;
//                                        long C_final_reduce= 0;
//                                        long C_distribute = 0;
//                                        long C_collect = 0;
//                                        long C_pipeline = 0;
//
//                                        Double loadCycles = operationCycles.get("load");
//                                        Double storeCycles = operationCycles.get("store");
//
//                                        if (loadCycles == null) {
//                                            System.out.println("Warning: No cycle info for 'load' operation. Using default 1.0");
//                                            loadCycles = 1.0;
//                                        }
//                                        if (storeCycles == null) {
//                                            System.out.println("Warning: No cycle info for 'store' operation. Using default 1.0");
//                                            storeCycles = 1.0;
//                                        }
//
//                                        C_reshape_part = (long) (dataSize * 2 * (loadCycles + storeCycles));
//
//                                        // C_parallel: max cost among implementations
//                                        //Currently working on this part
//                                        for (Map.Entry<String, Long> entry : computationCostsTotal.entrySet()) {
//                                            long totalCost = entry.getValue();
//                                            long c_parallel = ((srcSigChildDimentions.get(0)/currentGroupSize) * (loadCycles + storeCycles)) + currentGroupSize > 0 ? totalCost / currentGroupSize : totalCost;
//                                            C_parallel = Math.max(C_parallel, c_parallel);
//                                        }
//
//                                        //C_coll_recomb = C_reshape_part;
//
//                                        if (currentGroupSize > 0 && minBandwidth > 0) {
//                                            int bytesPerElement = 4;
//                                            int dataPerGroup = dataSize / currentGroupSize;
//                                            double communicationTimePerGroup = ((double) (dataPerGroup * bytesPerElement)) / minBandwidth;
//                                            double latencyPerGroup = max_num_hop_bus * hop_bus_Transfer;
//                                            C_distribute = (long) (currentGroupSize * (communicationTimePerGroup + latencyPerGroup));
//                                        }
//                                        C_collect = C_distribute;
//
//                                        List<Long> stageCosts = Arrays.asList(C_part_dist, C_parallel, C_coll_recomb, C_distribute, C_collect);
//                                        long C_rhs = Collections.max(stageCosts);
//
//                                        rhsCostsByGroupSize.put(currentGroupSize, C_rhs);
//
//                                        Map<String, Long> stageCostMap = new LinkedHashMap<>();
//                                        stageCostMap.put("C_part_dist", C_part_dist);
//                                        stageCostMap.put("C_parallel", C_parallel);
//                                        stageCostMap.put("C_coll_recomb", C_coll_recomb);
//                                        stageCostMap.put("C_distribute", C_distribute);
//                                        stageCostMap.put("C_collect", C_collect);
//                                        detailedStageCosts.put(currentGroupSize, stageCostMap);
//
//                                        if (C_rhs < bestCost || (C_rhs == bestCost && currentGroupSize > bestGroupSize)) {
//                                            bestCost = C_rhs;
//                                            bestGroupSize = currentGroupSize;
//                                        }
//
//                                    }
//
//                                    // === Print C_rhs summary table ===
//                                    System.out.println("\n--------------------------------------------------");
//                                    System.out.println("C_rhs per Partitioning Parameter (Transformed)");
//                                    System.out.println("--------------------------------------------------");
//                                    System.out.printf("%-30s | %-20s%n", "Number of Parallel Branches", "C_rhs (cycles)");
//                                    System.out.println("--------------------------------------------------");
//
//                                    rhsCostsByGroupSize.entrySet().stream()
//                                            .sorted(Map.Entry.comparingByKey())
//                                            .forEach(entry -> System.out.printf("%-30d | %-20d%n", entry.getKey(), entry.getValue()));
//
//                                    System.out.println("--------------------------------------------------");
//                                    System.out.printf("%-30s | %-20d%n", "Best Cost (Min C_rhs)", bestCost);
//                                    System.out.printf("%-30s | %-20d%n", "Best Partitioning Parameter", bestGroupSize);
//                                    System.out.println("--------------------------------------------------");
//
//                                    // === Print detailed stage costs table ===
//                                    System.out.println("\nDetailed Cost Breakdown per Partitioning Parameter");
//                                    System.out.println("-----------------------------------------------------------------------------------------");
//                                    System.out.printf("%-10s | %-12s | %-12s | %-14s | %-14s | %-12s%n",
//                                            "Branches", "C_part_dist", "C_parallel", "C_coll_recomb", "C_distribute", "C_collect");
//                                    System.out.println("-----------------------------------------------------------------------------------------");
//
//                                    detailedStageCosts.entrySet().stream()
//                                            .sorted(Map.Entry.comparingByKey())
//                                            .forEach(entry -> {
//                                                int branches = entry.getKey();
//                                                Map<String, Long> costs = entry.getValue();
//                                                System.out.printf("%-10d | %-12d | %-12d | %-14d | %-14d | %-12d%n",
//                                                        branches,
//                                                        costs.getOrDefault("C_part_dist", 0L),
//                                                        costs.getOrDefault("C_parallel", 0L),
//                                                        costs.getOrDefault("C_coll_recomb", 0L),
//                                                        costs.getOrDefault("C_distribute", 0L),
//                                                        costs.getOrDefault("C_collect", 0L)
//                                                );
//                                            });
//
//                                    System.out.println("-----------------------------------------------------------------------------------------");
//
//
//
//                                    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//                                }
//                                // MapSY-MapV transformation application
//
//                                if (bestGroupSize > 0 && C_lhs > bestCost && !(bestGroupSize == srcSigChildDimentions.get(0))) {
//                                    System.out.println("Applying MapSY-MapV transformation for partitioning parameter = " + bestGroupSize + " (C_rhs = " + bestCost + " < C_lhs = " + C_lhs + ")");
//
//                                    Random random = new Random();
//                                    int rand = random.nextInt(1000);
//                                    final SystemGraph transformedGraph = new NamedSystemGraph(syMap.getIdentifier()+ rand + "_T");
//                                    transformedGraph.mergeInPlace(systemGraph);
//                                    // this need to be done since the root was viewed in the wrong system graph
//                                    root = ForSyDeHierarchy.GreyBox.tryView(transformedGraph, root.getViewedVertex()).get();
//                                    final List<SYMapViewer> newSYMaps = new ArrayList<>();
//                                    for (int i=0; i < bestGroupSize; i++){
//                                        final Vertex parallelStruct = transformedGraph.newVertex("parallelStruct");
//                                        parallelStruct.addTraits(new OpaqueTrait("ParStruct"));
//                                        parallelStruct.addPort("tuplets");
//
//                                        final SYMapViewer newSYMap = ForSyDeHierarchy.SYMap.enforce(transformedGraph, transformedGraph.newVertex(v.getIdentifier() + "TSY" + i));
//                                        final GreyBoxViewer newSYMapGB = ForSyDeHierarchy.GreyBox.enforce(newSYMap);
//                                        newSYMap.addCombFunctions(mapInside);
//                                        newSYMapGB.addContained(ForSyDeHierarchy.Visualizable.enforce(mapInside));
//                                        newSYMapGB.addContained(ForSyDeHierarchy.Visualizable.enforce(mapInside));
//
//                                        transformedGraph.connect(parallelStruct, newSYMap.getViewedVertex(), "tuplets", ForSyDeHierarchy.EdgeTraits.BehaviourCompositionEdge);
//                                        //Making the connections between newSYMaps and their hierarchy below
//                                        newSYMaps.add(newSYMap);
//                                        root.addContained(newSYMapGB);
//
//                                    }
//                                    for (EdgeInfo e : transformedGraph.outgoingEdgesOf(v)) {
//                                        if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.SYNetworkEdge)) {
//                                            dstSignal = transformedGraph.queryVertex(e.getTarget()).get();
//                                            for (EdgeInfo ed : transformedGraph.outgoingEdgesOf(dstSignal)) {
//                                                if (ed.hasTrait(ForSyDeHierarchy.EdgeTraits.VisualContainment)){
//                                                    final Vertex sigChild = transformedGraph.queryVertex(ed.getTarget()).get();
//                                                    if (ForSyDeHierarchy.Vectorizable.tryView(transformedGraph, sigChild).isPresent()){
//                                                        dstSigChildDimentions = ForSyDeHierarchy.Vectorizable.tryView(transformedGraph, sigChild).get().dimensions();
//                                                    }
//                                                }
//                                            }
//                                        }
//                                    }
//                                    //Generating the zipxSY and unZipxSY
//                                    final SYMapViewer newZipxSY = ForSyDeHierarchy.SYMap.enforce(transformedGraph, transformedGraph.newVertex(v.getIdentifier() + "zipxSYtransformedSY"));
//                                    //newZipxSY.insertCombFunctionsPort(transformedGraph, concat);
//                                    var zip = ForSyDeHierarchy.InstrumentedBehaviour.enforce(newZipxSY);
//                                    zip.computationalRequirements(Map.of("default", Map.of("merge", 1L)));
//                                    GreyBoxViewer newZipxSYGB = ForSyDeHierarchy.GreyBox.enforce(newZipxSY);
//                                    root.addContained(newZipxSYGB);
//                                    //newZipxSYGB.insertContainedPort(transformedGraph, Visualizable.enforce(concat));
//                                    final SYMapViewer newunZipxSY = ForSyDeHierarchy.SYMap.enforce(transformedGraph, transformedGraph.newVertex(v.getIdentifier() + "unZipxSYtransformedSY"));
//                                    //newunZipxSY.insertCombFunctionsPort(transformedGraph, concat);
//                                    var unzip = ForSyDeHierarchy.InstrumentedBehaviour.enforce(newunZipxSY);
//                                    unzip.computationalRequirements(Map.of("default", Map.of("split", 1L)));
//                                    GreyBoxViewer newunZipxSYGB = ForSyDeHierarchy.GreyBox.enforce(newunZipxSY);
//                                    root.addContained(newunZipxSYGB);
//                                    //newunZipxSYGB.insertContainedPort(transformedGraph, Visualizable.enforce(concat));
//
//                                    //Connecting the input ports of the original mapSY to the newunZipxSY and the output ports to the new zipxSY
//                                    for (String s : ForSyDeHierarchy.SYMap.tryView(transformedGraph, v).get().inputPorts()) {
//                                        newZipxSY.getViewedVertex().addPort(s);
//                                    }
//                                    for (String s : ForSyDeHierarchy.SYMap.tryView(transformedGraph, v).get().outputPorts()) {
//                                        newunZipxSY.getViewedVertex().addPort(s);
//                                    }
//                                    newZipxSY.inputPorts(ForSyDeHierarchy.SYMap.tryView(transformedGraph, v).get().inputPorts());
//                                    newunZipxSY.outputPorts(ForSyDeHierarchy.SYMap.tryView(transformedGraph, v).get().outputPorts());
//
//                                    if (ForSyDeHierarchy.SYSignal.tryView(transformedGraph, srcSignal).isPresent()) {
//                                        ForSyDeHierarchy.SYSignal.tryView(transformedGraph, srcSignal).get().addConsumers(newZipxSY.inputPorts().get(0), newZipxSY);
//                                    }
//                                    transformedGraph.connect(srcSignal, ForSyDeHierarchy.SYMap.tryView(newZipxSY).get().getViewedVertex(), "consumers", newZipxSY.inputPorts().get(0), ForSyDeHierarchy.EdgeTraits.VisualConnection);
//                                    if (ForSyDeHierarchy.SYSignal.tryView(transformedGraph, dstSignal).isPresent()) {
//                                        ForSyDeHierarchy.SYSignal.tryView(transformedGraph, dstSignal).get().producer(newunZipxSY.outputPorts().get(0), newunZipxSY);
//                                    }
//                                    transformedGraph.connect(ForSyDeHierarchy.SYMap.tryView(newunZipxSY).get().getViewedVertex(), dstSignal, newunZipxSY.outputPorts().get(0), "producer", ForSyDeHierarchy.EdgeTraits.VisualConnection);
//                                    //Generating signals between newZipxSY and the newSYMaps as well as signals between newSYMaps and newunZipxSY
//                                    for (int i = 0; i < newSYMaps.size(); i++) {
//                                        final SYMapViewer leftSYMap = newZipxSY;
//                                        final SYMapViewer rightSYMap = newSYMaps.get(i);
//                                        final SYSignalViewer newSYSignal = ForSyDeHierarchy.SYSignal.enforce(transformedGraph, transformedGraph.newVertex(leftSYMap.getIdentifier() + rightSYMap.getIdentifier() + "Sig"));
//                                        //Inserting hierarchy below for the newSYSignals
//                                        final VectorizableViewer newVectorizable = ForSyDeHierarchy.Vectorizable.enforce(transformedGraph, transformedGraph.newVertex(newSYSignal.getIdentifier()+"Vec"));
//                                        List<Integer> newVectorizableDimentions = new ArrayList<>();
//                                        newVectorizableDimentions.add(srcSigChildDimentions.get(0)/bestGroupSize);
//                                        for (int k=1; k < srcSigChildDimentions.size(); k++){
//                                            newVectorizableDimentions.add(srcSigChildDimentions.get(k));
//                                        }
//                                        newVectorizable.dimensions(newVectorizableDimentions);
//                                        final VisualizableWithPropertiesViewer visualizableWithProperties = ForSyDeHierarchy.VisualizableWithProperties.enforce(newVectorizable);
//                                        ForSyDeHierarchy.GreyBox.enforce(newSYSignal).addContained(visualizableWithProperties);
//                                        visualizableWithProperties.visualizedPropertiesNames(List.of("dimensions"));
//
//                                        newVectorizable.getViewedVertex().addPort("input");
//                                        newVectorizable.getViewedVertex().addPort("output");
//                                        //Connecting the input and output ports of the newSignal to its hierarchy below
//                                        transformedGraph.connect(newSYSignal, newVectorizable, "producer", "input", ForSyDeHierarchy.EdgeTraits.VisualConnection);
//                                        transformedGraph.connect(newVectorizable, newSYSignal, "output", "consumers", ForSyDeHierarchy.EdgeTraits.VisualConnection);
//                                        //--------------------------------------------------------------
//
//                                        root.addContained(ForSyDeHierarchy.Visualizable.enforce(newSYSignal));
//                                        leftSYMap.getViewedVertex().addPort("newZipxSYTransformedOut");
//                                        leftSYMap.outputPorts(List.of("newZipxSYTransformedOut"));
//                                        //--------------------------------------------------------
//                                        rightSYMap.getViewedVertex().addPort("newSYSignalTransformedIn");
//                                        rightSYMap.inputPorts(List.of("newSYSignalTransformedIn"));
//                                        newSYSignal.producer("newZipxSYTransformedOut", leftSYMap);
//                                        newSYSignal.addConsumers("newSYSignalTransformedIn", rightSYMap);
//                                        transformedGraph.connect(leftSYMap,newSYSignal,"newZipxSYTransformedOut", "producer", ForSyDeHierarchy.EdgeTraits.VisualConnection);
//                                        transformedGraph.connect(newSYSignal,rightSYMap,"consumers", "newSYSignalTransformedIn", ForSyDeHierarchy.EdgeTraits.VisualConnection);
//
//                                    }
//                                    for (int i = 0; i < newSYMaps.size(); i++) {
//                                        final SYMapViewer leftSYMap = newSYMaps.get(i);
//                                        final SYMapViewer rightSYMap = newunZipxSY;
//                                        final SYSignalViewer newSYSignal = ForSyDeHierarchy.SYSignal.enforce(transformedGraph, transformedGraph.newVertex(leftSYMap.getIdentifier() + rightSYMap.getIdentifier() + "Sig"));
//                                        //Inserting hierarchy below for the newSYSignals
//                                        final VectorizableViewer newVectorizable = ForSyDeHierarchy.Vectorizable.enforce(transformedGraph, transformedGraph.newVertex(newSYSignal.getIdentifier()+"Vec"));
//                                        List<Integer> newVectorizableDimentions = new ArrayList<>();
//                                        newVectorizableDimentions.add(srcSigChildDimentions.get(0)/bestGroupSize);
//                                        for (int k=1; k < dstSigChildDimentions.size(); k++){
//                                            newVectorizableDimentions.add(dstSigChildDimentions.get(k));
//                                        }
//                                        newVectorizable.dimensions(newVectorizableDimentions);
//                                        final VisualizableWithPropertiesViewer visualizableWithProperties = ForSyDeHierarchy.VisualizableWithProperties.enforce(newVectorizable);
//                                        ForSyDeHierarchy.GreyBox.enforce(newSYSignal).addContained(ForSyDeHierarchy.VisualizableWithProperties.enforce(newVectorizable));
//                                        visualizableWithProperties.visualizedPropertiesNames(List.of("dimensions"));
//                                        newVectorizable.getViewedVertex().addPort("input");
//                                        newVectorizable.getViewedVertex().addPort("output");
//                                        //Connecting the input and output ports of the newSignal to its hierarchy below
//                                        transformedGraph.connect(newSYSignal, newVectorizable, "producer", "input", ForSyDeHierarchy.EdgeTraits.VisualConnection);
//                                        transformedGraph.connect(newVectorizable, newSYSignal, "output", "consumers", ForSyDeHierarchy.EdgeTraits.VisualConnection);
//                                        //---------------------------------------------------------------------------
//
//                                        root.addContained(ForSyDeHierarchy.Visualizable.enforce(newSYSignal));
//                                        leftSYMap.getViewedVertex().addPort("newSYSignalTransformedOut");
//                                        leftSYMap.outputPorts(List.of("newSYSignalTransformedOut"));
//
//                                        //Connecting the incoming and outgoing edges of the combfunction of the newSYMap which are the children of mapInside
//                                        for (EdgeInfo e: transformedGraph.outgoingEdgesOf(v)){
//                                            final Vertex dstVertex = transformedGraph.getEdgeTarget(e);
//                                            if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.VisualContainment)) continue;
//                                            if (e.getSourcePort().get().equals("combFunctions")) continue;
//                                            if (dstVertex == getOrderedChildren(transformedGraph, ForSyDeHierarchy.SYMap.enforce(transformedGraph, v)).get(0).getViewedVertex()){
//                                                transformedGraph.connect(leftSYMap, getOrderedChildren(transformedGraph, leftSYMap).get(0), "newSYSignalTransformedIn",e.getSourcePort().get(), ForSyDeHierarchy.EdgeTraits.VisualConnection);
//                                            }
//                                        }
//                                        for (EdgeInfo e: transformedGraph.incomingEdgesOf(v)){
//                                            final Vertex srcVertex = transformedGraph.getEdgeSource(e);
//                                            if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.VisualContainment)) continue;
//                                            if (e.getSourcePort().get().equals("combFunctions")) continue;
//                                            if (srcVertex == getOrderedChildren(transformedGraph, ForSyDeHierarchy.SYMap.enforce(transformedGraph, v)).get(0).getViewedVertex()){
//                                                transformedGraph.connect(getOrderedChildren(transformedGraph, leftSYMap).get(0), leftSYMap, e.getSourcePort().get(), "newSYSignalTransformedOut", ForSyDeHierarchy.EdgeTraits.VisualConnection);
//                                            }
//
//                                        }
//
//                                        rightSYMap.getViewedVertex().addPort("newunZipxSYTransformedIn");
//                                        rightSYMap.inputPorts(List.of("newunZipxSYTransformedIn"));
//                                        newSYSignal.producer("newSYSignalTransformedOut", leftSYMap);
//                                        newSYSignal.addConsumers("newunZipxSYTransformedIn", rightSYMap);
//                                        transformedGraph.connect(leftSYMap,newSYSignal,"newSYSignalTransformedOut", "producer", ForSyDeHierarchy.EdgeTraits.VisualConnection);
//                                        transformedGraph.connect(newSYSignal,rightSYMap,"consumers", "newunZipxSYTransformedIn", ForSyDeHierarchy.EdgeTraits.VisualConnection);
//                                    }
//                                    transformedGraph.removeVertex(v);
////                                  deleteHierarchy(transformedGraph, v);
//                                    ParameterizedMapsyReducevTransformedGraphs.add(transformedGraph);
//                                }  else if (rhsCostsByGroupSize.isEmpty()) {
//                                    System.out.println("Skipping MapSY-MapV transformation. No valid C_rhs computed (group sizes may be empty or invalid).");
//                                } else {
//                                    System.out.println("Skipping MapSY-MapV transformation. No improvement (C_lhs = " + C_lhs + ", Best C_rhs = " + bestCost + ")");
//                                }
//                            }
//                        }
//                    }
//                }
//            }
//        }
//        return ParameterizedMapsyReducevTransformedGraphs;
//    }
//}
