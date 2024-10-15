package patternMatch;

import forsyde.io.core.*;
import forsyde.io.lib.hierarchy.ForSyDeHierarchy;
import forsyde.io.lib.hierarchy.behavior.BehaviourEntity;
import forsyde.io.lib.hierarchy.behavior.FunctionLikeEntity;
import forsyde.io.lib.hierarchy.behavior.moc.sy.SYMap;
import forsyde.io.lib.hierarchy.behavior.moc.sy.SYMapViewer;
import forsyde.io.lib.hierarchy.behavior.moc.sy.SYSignal;
import forsyde.io.lib.hierarchy.behavior.moc.sy.SYSignalViewer;
import forsyde.io.lib.hierarchy.behavior.parallel.MapV;
import forsyde.io.lib.hierarchy.behavior.parallel.ReduceV;
import forsyde.io.lib.hierarchy.behavior.parallel.Vectorizable;
import forsyde.io.lib.hierarchy.visualization.GreyBox;
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


public class MapSplitTransformer implements Transformer {
    @Override
    public Set<SystemGraph> applyTransform(SystemGraph systemGraph) {
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
                SYMap syMap = ForSyDeHierarchy.SYMap.tryView(systemGraph, v).get();
                syMaps.add(syMap);

                int inPortNum = syMap.inputPorts().size();
                int outPortNum = syMap.outputPorts().size();

                System.out.println(v.getIdentifier() + ", inputs is " + inPortNum + ", and output is " + outPortNum);

                // mapSY recognition
                if (inPortNum == 1 && outPortNum == 1) {

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
                System.out.println("---------------------Map-Split----------------------------");
                System.out.println(syMap.getIdentifier() + ": ");
                for (FunctionLikeEntity executable : processesAndOrderedExecutables.get(syMap)) {
                    System.out.print(executable.getIdentifier() + "\t");
                }

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
                if (apply.getPlain() == 1) {
                    Random random = new Random();
                    int rand = random.nextInt(1000);
                    final SystemGraph transformedGraph = new NamedSystemGraph(syMap.getIdentifier() + rand + "_T");
                    transformedGraph.mergeInPlace(systemGraph);
                    // this need to be done since the root was viewed in the wrong system graph
                    root = ForSyDeHierarchy.GreyBox.tryView(transformedGraph, root.getViewedVertex()).get();
                    final List<SYMapViewer> newSYMaps = new ArrayList<>();
                    //Generating new SYMaps: mapSY(f), mapSY(g), ...
                    for (FunctionLikeEntity executable : processesAndOrderedExecutables.get(syMap)) {
                        Random random2 = new Random();
                        int rand2 = random2.nextInt(1000);
                        final SYMapViewer newSYMap = ForSyDeHierarchy.SYMap.enforce(transformedGraph, transformedGraph.newVertex(executable.getIdentifier() + rand2 + "TSY"));
                        newSYMap.addCombFunctions(executable);
                        newSYMaps.add(newSYMap);
                        GreyBoxViewer newSYMapGB = ForSyDeHierarchy.GreyBox.enforce(newSYMap);
                        root.addContained(newSYMapGB);
                        newSYMapGB.addContained(ForSyDeHierarchy.Visualizable.enforce(executable));
                    }
                    //Generating signals between the new SYMaps: the signal between mapSY(f) and mapSY(g), ...
                    for (int i = 0; i < newSYMaps.size() - 1; i++) {
                        final SYMapViewer leftSYMap = newSYMaps.get(i);
                        final SYMapViewer rightSYMap = newSYMaps.get(i + 1);
                        final SYSignalViewer newSYSignal = ForSyDeHierarchy.SYSignal.enforce(transformedGraph, transformedGraph.newVertex(leftSYMap.getIdentifier() + rightSYMap.getIdentifier() + "Sig"));
                        root.addContained(ForSyDeHierarchy.Visualizable.enforce(newSYSignal));
                        //working on this part
                        //for (int j = 0; j < processesAndOrderedExecutables.get(syMap).size() - 1; j++) {
                        final FunctionLikeEntity exe1 = processesAndOrderedExecutables.get(syMap).get(i);
                        final FunctionLikeEntity exe2 = processesAndOrderedExecutables.get(syMap).get(i + 1);
                        for (EdgeInfo e1 : systemGraph.outgoingEdgesOf(exe1.getViewedVertex())) {
                            final Vertex v1 = systemGraph.getEdgeTarget(e1);
                            for (EdgeInfo e2 : systemGraph.incomingEdgesOf(exe2.getViewedVertex())) {
                                final Vertex v2 = systemGraph.getEdgeSource(e2);
                                if (v1.getIdentifier() == v2.getIdentifier() && ForSyDeHierarchy.RegisterLike.tryView(systemGraph, v1).isPresent()) {
                                    ForSyDeHierarchy.GreyBox.enforce(newSYSignal).addContained(ForSyDeHierarchy.Visualizable.enforce(systemGraph, v1));
                                    ;
                                    transformedGraph.connect(newSYSignal.getViewedVertex(), v1, "producer", "input", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                    transformedGraph.connect(v1, newSYSignal.getViewedVertex(), "output", "consumers", ForSyDeHierarchy.EdgeTraits.VisualConnection);

                                }
                            }
                        }
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
}
