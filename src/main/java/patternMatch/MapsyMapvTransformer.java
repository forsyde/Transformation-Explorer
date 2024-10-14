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
import forsyde.io.lib.hierarchy.visualization.*;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

public class MapsyMapvTransformer implements Transformer {
    @Override
    public Set<SystemGraph> applyTransform(SystemGraph systemGraph) {
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

                int inPortNum = syMap.inputPorts().size();
                int outPortNum = syMap.outputPorts().size();

                //System.out.println(v.getIdentifier() + ", inputs is " + inPortNum + ", and output is " + outPortNum);

                // mapSY recognition
                if (inPortNum == 1 && outPortNum == 1) {
                    final Set<FunctionLikeEntity> combFunctions = syMap.combFunctions();

                    // MapSY-MapV pattern recognition
                    if (combFunctions.size() == 1) {
                        for (FunctionLikeEntity combs : combFunctions) {
                            ForSyDeHierarchy.MapV.tryView(combs).ifPresent(mapV -> {
                                MapV mapInside = mapV;
                                System.out.println("---------------------MapSY-MapV------------------------");
                                System.out.println(syMap.getIdentifier() + ": \n" + mapV.getIdentifier());
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
                                if (apply.getPlain() == 1) {
                                    // MapSY-MapV transformation application
                                    Random random = new Random();
                                    int rand = random.nextInt(1000);
                                    final SystemGraph transformedGraph = new NamedSystemGraph(syMap.getIdentifier() + rand + "_T");
                                    transformedGraph.mergeInPlace(systemGraph);
                                    final List<SYMapViewer> newSYMaps = new ArrayList<>();
                                    // this need to be done since the root was viewed in the wrong system graph
                                    root = ForSyDeHierarchy.GreyBox.tryView(transformedGraph, root.getViewedVertex()).get();
                                    //Generating new SYMaps: mapSY(f1), mapSY(f2), ...
                                    //Finding the dimensions of the input (which determines the number of mapSYs)
                                    Vertex srcSignal = null;
                                    Vertex dstSignal = null;
                                    List<Integer> sigChildDimentions = new ArrayList<>();
                                    Scanner scanner = new Scanner(System.in);
                                    int groupSize;
                                    for (EdgeInfo e : transformedGraph.incomingEdgesOf(v)) {
                                        if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.SYNetworkEdge)) {
                                            srcSignal = transformedGraph.queryVertex(e.getSource()).get();
                                            for (EdgeInfo ed : transformedGraph.outgoingEdgesOf(srcSignal)) {
                                                if (ed.hasTrait(ForSyDeHierarchy.EdgeTraits.VisualContainment)) {
                                                    final Vertex sigChild = transformedGraph.queryVertex(ed.getTarget()).get();
                                                    if (ForSyDeHierarchy.Vectorizable.tryView(transformedGraph, sigChild).isPresent()) {
                                                        Vectorizable vectorizable = ForSyDeHierarchy.Vectorizable.tryView(transformedGraph, sigChild).get();
                                                        sigChildDimentions = vectorizable.dimensions();
                                                        //The first dimension which is the number of rows in the case of a matrix is the number of mapSYs.
//                                                    System.out.println("Please enter the intended size of each process (the dimension of the input signal is "+ sigChildDimentions.get(0) + "):");
//                                                    groupSize = scanner.nextInt();
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
                                                    }
                                                }
                                            }
                                        }
                                    }
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
