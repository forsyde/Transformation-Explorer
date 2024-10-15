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
import forsyde.io.lib.hierarchy.behavior.moc.sy.SYSignal;
import forsyde.io.lib.hierarchy.behavior.moc.sy.SYSignalViewer;
import forsyde.io.lib.hierarchy.behavior.parallel.MapV;
import forsyde.io.lib.hierarchy.behavior.parallel.ReduceV;
import forsyde.io.lib.hierarchy.behavior.parallel.Vectorizable;
import forsyde.io.lib.hierarchy.behavior.parallel.VectorizableViewer;
import forsyde.io.lib.hierarchy.visualization.GreyBox;
import forsyde.io.lib.hierarchy.visualization.GreyBoxViewer;
import forsyde.io.lib.hierarchy.visualization.VisualizableWithPropertiesViewer;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

public class MapsyReducevTransformer implements Transformer{


    @Override
    public Set<SystemGraph> applyTransform(SystemGraph systemGraph) {
        Set<SystemGraph> mapsyReducevTransformedGraphs= new HashSet<>();

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
        Map<SYMap, List<BehaviourEntity>> processesAndOrderedExecutables = new HashMap<>();
        for (Vertex v : systemGraph.vertexSet()) {

            // SY vertices recognition
            if (ForSyDeHierarchy.SYMap.tryView(systemGraph, v).isPresent()) {
                SYMap syMap = ForSyDeHierarchy.SYMap.tryView(systemGraph, v).get();
                syMaps.add(syMap);

                int inPortNum = syMap.inputPorts().size();
                int outPortNum = syMap.outputPorts().size();

                //System.out.println(v.getIdentifier() + ", inputs is " + inPortNum + ", and output is " + outPortNum);

                // mapSY recognition
                if (inPortNum == 1 && outPortNum == 1) {
                    final Set<FunctionLikeEntity> combFunctions = syMap.combFunctions();

                    // MapSY-ReduceV pattern recognition
                    if (combFunctions.size() == 1) {
                        ReduceV reduceInside = null;
                        for (FunctionLikeEntity combs : combFunctions) {
                            if (ForSyDeHierarchy.ReduceV.tryView(combs).isPresent()) {
                                reduceInside = ForSyDeHierarchy.ReduceV.tryView(combs).get();
                                System.out.println("---------------------MapSY-ReduceV------------------------");
                                System.out.println(ForSyDeHierarchy.SYMap.tryView(systemGraph, v).get().getIdentifier() + ": \n" + reduceInside.getIdentifier());
                                GreyBoxViewer root = null;
                                for (EdgeInfo e : systemGraph.incomingEdgesOf(v)){
                                    if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.VisualContainment)){
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
//                                mapSplitTransformer.applyTransform(SystemGraph, );
                                if (apply.getPlain() == 1) {
                                    // MapSY-ReduceV transformation application
                                    Random random = new Random();
                                    int rand = random.nextInt(1000);
                                    final SystemGraph transformedGraph = new NamedSystemGraph(syMap.getIdentifier() + rand + "_T");
                                    transformedGraph.mergeInPlace(systemGraph);
                                    // this need to be done since the root was viewed in the wrong ssytem graph
                                    root = ForSyDeHierarchy.GreyBox.tryView(transformedGraph, root.getViewedVertex()).get();
                                    final List<SYMapViewer> newSYMaps = new ArrayList<>();
                                    final List<SYSignalViewer> newSYSignals = new ArrayList<>();
                                    final List<SYSignalViewer> newInBetweenSYSignals = new ArrayList<>();
                                    //Generating new SYMaps: mapSY(f1), mapSY(f2), ...
                                    //Finding the dimensions of the input (which determines the number of mapSYs)
                                    Vertex srcSignal = null;
                                    Vertex dstSignal = null;

                                    final Vertex parallelStruct = transformedGraph.newVertex(v.getIdentifier() + reduceInside.getIdentifier());
                                    parallelStruct.addTraits(new OpaqueTrait("ParStruct"));
                                    parallelStruct.addPort("tuplets");

                                    var parList = new ArrayList<String>();
                                    List<Integer> sigChildDimentions = new ArrayList<>();
                                    for (EdgeInfo e : transformedGraph.incomingEdgesOf(v)) {
                                        if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.SYNetworkEdge)) {
                                            srcSignal = transformedGraph.queryVertex(e.getSource()).get();
                                            for (EdgeInfo ed : transformedGraph.outgoingEdgesOf(srcSignal)) {
                                                if (ed.hasTrait(ForSyDeHierarchy.EdgeTraits.VisualContainment)) {
                                                    final Vertex sigChild = transformedGraph.queryVertex(ed.getTarget()).get();
                                                    if (ForSyDeHierarchy.Vectorizable.tryView(transformedGraph, sigChild).isPresent()) {
                                                        sigChildDimentions = ForSyDeHierarchy.Vectorizable.tryView(transformedGraph, sigChild).get().dimensions();
                                                        //The first dimension which is the number of rows in the case of a matrix is the number of mapSYs.
                                                        for (int i = 0; i < sigChildDimentions.get(0) - 1; i++) {
                                                            final SYMapViewer newSYMap = ForSyDeHierarchy.SYMap.enforce(transformedGraph, transformedGraph.newVertex(v.getIdentifier() + "TSY" + i));
                                                            final GreyBoxViewer newSYMapGB = ForSyDeHierarchy.GreyBox.enforce(newSYMap);
                                                            for (FunctionLikeEntity child : getOrderedChildren(transformedGraph, reduceInside)) {
                                                                newSYMap.addCombFunctions(child);
                                                                newSYMapGB.addContained(ForSyDeHierarchy.Visualizable.enforce(child));
                                                            }
                                                            newSYMap.getViewedVertex().addPort("newSYSignalTransformedIn1");
                                                            newSYMap.getViewedVertex().addPort("newSYSignalTransformedIn2");
                                                            newSYMap.inputPorts(List.of("newSYSignalTransformedIn1", "newSYSignalTransformedIn2"));

                                                            newSYMap.getViewedVertex().addPort("newSYMapTransformedOut");
                                                            newSYMap.outputPorts(Collections.singletonList("newSYMapTransformedOut"));

                                                            parList.add(newSYMap.getIdentifier());
                                                            newSYMaps.add(newSYMap);
                                                            root.addContained(newSYMapGB);


                                                        }
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
                                    //Generating the unZipxSY
                                    final SYMapViewer newunZipxSY = ForSyDeHierarchy.SYMap.enforce(transformedGraph, transformedGraph.newVertex(v.getIdentifier() + "unZipxSYtransformedSY"));
                                    //newunZipxSY.insertCombFunctionsPort(transformedGraph, concat);
                                    var unzip = ForSyDeHierarchy.InstrumentedBehaviour.enforce(newunZipxSY);
                                    unzip.computationalRequirements(Map.of("default", Map.of("split", 1L)));
                                    GreyBoxViewer newunZipxSYGB = ForSyDeHierarchy.GreyBox.enforce(newunZipxSY);
                                    root.addContained(newunZipxSYGB);
                                    //newunZipxSYGB.insertContainedPort(transformedGraph, Visualizable.enforce(concat));

                                    //Connecting the input ports of the original mapSY to the newunZipxSY
                                    for (String s : ForSyDeHierarchy.SYMap.tryView(transformedGraph, v).get().inputPorts()) {
                                        newunZipxSY.getViewedVertex().addPort(s);
                                    }
                                    newunZipxSY.inputPorts(ForSyDeHierarchy.SYMap.tryView(transformedGraph, v).get().inputPorts());

                                    if (ForSyDeHierarchy.SYSignal.tryView(transformedGraph, srcSignal).isPresent()) {
                                        ForSyDeHierarchy.SYSignal.tryView(transformedGraph, srcSignal).get().addConsumers(newunZipxSY.inputPorts().get(0), newunZipxSY);
                                    }
                                    transformedGraph.connect(srcSignal, ForSyDeHierarchy.SYMap.tryView(newunZipxSY).get().getViewedVertex(), "consumers", newunZipxSY.inputPorts().get(0), ForSyDeHierarchy.EdgeTraits.VisualConnection);

                                    //Generating signals between newunZipxSY and the newSYMaps
                                    for (int i = 0; i < sigChildDimentions.get(0); i++) {
                                        final SYSignalViewer newSYSignal = ForSyDeHierarchy.SYSignal.enforce(transformedGraph, transformedGraph.newVertex(newunZipxSY.getIdentifier() + v.getIdentifier() + "TSY" + i + "Sig"));
                                        newSYSignals.add(newSYSignal);
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
                                            //newVectorizable.getViewedVertex().getProperties().put("visualizedPropertiesNames", VertexProperty.create("Dimentions"));
                                            // newVectorizable.getViewedVertex()
                                            newVectorizable.getViewedVertex().addPort("input");
                                            newVectorizable.getViewedVertex().addPort("output");
                                            //Connecting the input and output ports of the newSignal to its hierarchy below
                                            transformedGraph.connect(newSYSignal, newVectorizable, "producer", "input", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                            transformedGraph.connect(newVectorizable, newSYSignal, "output", "consumers", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                        }

                                        root.addContained(ForSyDeHierarchy.Visualizable.enforce(newSYSignal));
                                        newunZipxSY.getViewedVertex().addPort("newunZipxSYTransformedOut");
                                        newunZipxSY.outputPorts(List.of("newunZipxSYTransformedOut"));
                                        //--------------------------------------------------------
//

//                                    newSYSignal.setInputPort(transformedGraph, "newunZipxSYTransformedOut", leftSYMap);
//                                    newSYSignal.setOutputPort(transformedGraph, rightSYMap, "newSYSignalTransformedIn");
//                                    transformedGraph.connect(leftSYMap,newSYSignal,"newunZipxSYTransformedOut", "input", ForSyDeHierarchy.EdgeTraits.VisualConnection);
//                                    transformedGraph.connect(newSYSignal,rightSYMap,"output", "newSYSignalTransformedIn", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                    }

                                    //Generating signals between newSYMaps
                                    for (int i = 0; i < newSYMaps.size() - 1; i++) {
                                        final SYMapViewer leftSYMap = newSYMaps.get(i);
                                        final SYMapViewer rightSYMap = newSYMaps.get(i + 1);
                                        final SYSignalViewer newSYSignal = ForSyDeHierarchy.SYSignal.enforce(transformedGraph, transformedGraph.newVertex(leftSYMap.getIdentifier() + rightSYMap.getIdentifier() + "Sig"));
                                        newInBetweenSYSignals.add(newSYSignal);
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
                                            //newVectorizable.getViewedVertex().getProperties().put("visualizedPropertiesNames", VertexProperty.create("Dimentions"));
                                            // newVectorizable.getViewedVertex()
                                            newVectorizable.getViewedVertex().addPort("input");
                                            newVectorizable.getViewedVertex().addPort("output");
                                            //Connecting the input and output ports of the newSignal to its hierarchy below
                                            transformedGraph.connect(newSYSignal, newVectorizable, "producer", "input", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                            transformedGraph.connect(newVectorizable, newSYSignal, "output", "consuemrs", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                        }
                                        root.addContained(ForSyDeHierarchy.Visualizable.enforce(newSYSignal));

                                    }

                                    //Making the connections between newSYMaps and the newInBetweenSYSignals
                                    for (int k = 0; k < newInBetweenSYSignals.size(); k++) {
                                        newInBetweenSYSignals.get(k).producer("newSYMapTransformedOut", newSYMaps.get(k));
                                        transformedGraph.connect(newSYMaps.get(k), newInBetweenSYSignals.get(k), "newSYMapTransformedOut", "producer", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                        newInBetweenSYSignals.get(k).addConsumers(newSYMaps.get(k + 1).inputPorts().get(0), newSYMaps.get(k + 1));
                                        transformedGraph.connect(newInBetweenSYSignals.get(k), newSYMaps.get(k + 1), "consumers", newSYMaps.get(k + 1).inputPorts().get(0), ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                    }

                                    //Making the connections
                                    //The connections of the first mapSY in the chain
                                    newSYSignals.get(0).producer("newunZipxSYTransformedOut", newunZipxSY);
                                    transformedGraph.connect(newunZipxSY, newSYSignals.get(0), "newunZipxSYTransformedOut", "producer", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                    newSYSignals.get(0).addConsumers(newSYMaps.get(0).inputPorts().get(0), newSYMaps.get(0));
                                    transformedGraph.connect(newSYSignals.get(0), newSYMaps.get(0), "consumers", newSYMaps.get(0).inputPorts().get(0), ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                    newSYSignals.get(1).producer("newunZipxSYTransformedOut", newunZipxSY);
                                    transformedGraph.connect(newunZipxSY, newSYSignals.get(1), "newunZipxSYTransformedOut", "producer", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                    newSYSignals.get(1).addConsumers(newSYMaps.get(0).inputPorts().get(1), newSYMaps.get(0));
                                    transformedGraph.connect(newSYSignals.get(1), newSYMaps.get(0), "consumers", newSYMaps.get(0).inputPorts().get(1), ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                    // The connections of the mapSYs except the first one
                                    for (int i = 2; i < newSYSignals.size(); i++) {
                                        newSYSignals.get(i).producer("newunZipxSYTransformedOut", newunZipxSY);
                                        transformedGraph.connect(newunZipxSY, newSYSignals.get(i), "newunZipxSYTransformedOut", "producer", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                        newSYSignals.get(i).addConsumers(newSYMaps.get(i - 1).inputPorts().get(1), newSYMaps.get(i - 1));
                                        transformedGraph.connect(newSYSignals.get(i), newSYMaps.get(i - 1), "consumers", newSYMaps.get(i - 1).inputPorts().get(1), ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                    }
//                                for (int i=1; i < newSYMaps.size(); i++){
//                                    transformedGraph.connect(newSYMaps.get(i-1), newSYMaps.get(i), "newSYMapTransformedOut", newSYMaps.get(i).getInputPorts().get(0), ForSyDeHierarchy.EdgeTraits.VisualConnection);
//                                }

                                    //Connecting the incoming and outgoing edges of the combfunction of the newSYMap which are the children of reduceInside
                                    for (int i = 0; i < newSYMaps.size(); i++) {
                                        final FunctionLikeEntity child = getOrderedChildren(transformedGraph, newSYMaps.get(i)).get(0);
                                        if (ForSyDeHierarchy.HasANSICImplementations.tryView(child).isPresent()) {
                                            final List<String> childInputs = ForSyDeHierarchy.HasANSICImplementations.tryView(child).get().inputArgumentPorts();
                                            final List<String> childOutputs = ForSyDeHierarchy.HasANSICImplementations.tryView(child).get().outputArgumentPorts();
                                            for (int j = 0; j < childInputs.size(); j++) {
                                                transformedGraph.connect(newSYMaps.get(i), child, newSYMaps.get(i).inputPorts().get(j), childInputs.get(j), ForSyDeHierarchy.EdgeTraits.BehaviourCompositionEdge, ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                            }
                                            for (int j = 0; j < childOutputs.size(); j++) {
                                                transformedGraph.connect(child, newSYMaps.get(i), childOutputs.get(j), newSYMaps.get(i).outputPorts().get(j), ForSyDeHierarchy.EdgeTraits.BehaviourCompositionEdge, ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                            }
                                            if (ForSyDeHierarchy.HasANSICImplementations.tryView(child).get().returnPort().isPresent()) {
                                                transformedGraph.connect(child, newSYMaps.get(i), ForSyDeHierarchy.HasANSICImplementations.tryView(child).get().returnPort().get(), newSYMaps.get(i).outputPorts().get(newSYMaps.get(i).outputPorts().size() - 1), ForSyDeHierarchy.EdgeTraits.BehaviourCompositionEdge, ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                            }
                                        } else {
                                            final List<String> portsWithSomeOrder = new ArrayList<>(child.getPorts());
                                            for (int j = 0; j < newSYMaps.get(i).inputPorts().size() && j < portsWithSomeOrder.size(); j++) {
                                                transformedGraph.connect(newSYMaps.get(i), child, newSYMaps.get(i).inputPorts().get(j), portsWithSomeOrder.get(j), ForSyDeHierarchy.EdgeTraits.BehaviourCompositionEdge, ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                            }
                                            for (int j = newSYMaps.get(i).inputPorts().size(); j < portsWithSomeOrder.size(); j++) {
                                                transformedGraph.connect(child, newSYMaps.get(i), newSYMaps.get(i).outputPorts().get(j - newSYMaps.get(i).inputPorts().size()), portsWithSomeOrder.get(j), ForSyDeHierarchy.EdgeTraits.BehaviourCompositionEdge, ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                            }
                                        }
                                    }
                                    //Connecting the output of the last newMapSY to the target signal of the original vertex.
                                    for (EdgeInfo e : transformedGraph.outgoingEdgesOf(v)) {
                                        final Vertex dstVertex = transformedGraph.getEdgeTarget(e);
                                        if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.VisualContainment)) continue;
                                        if (e.getSourcePort().get().equals("combFunctions")) continue;
                                        if (!(dstVertex == getOrderedChildren(transformedGraph, ForSyDeHierarchy.SYMap.enforce(transformedGraph, v)).get(0).getViewedVertex())) {
                                            transformedGraph.connect(newSYMaps.get(newSYMaps.size() - 1).getViewedVertex(), dstVertex, "newSYMapTransformedOut", e.getTargetPort().get(), ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                        }
                                    }

                                    //Removing the original mapSY as well as hierarchy inside it besides the signals.
//                                Vertex childV = getOrderedChildren(SystemGraph, SYMap.tryView(v).get()).get(0).getViewedVertex();
//                                transformedGraph.removeVertex(childV);
//                                transformedGraph.removeVertex(v);
                                    //transformedGraph.removeVertex(reduceInside.getViewedVertex());
                                    transformedGraph.removeVertex(v);
                                    int incomingNonKernels = 0;
                                    for (EdgeInfo e : transformedGraph.incomingEdgesOf(reduceInside)) {
                                        ReduceV finalReduceInside = reduceInside;
                                        boolean isKernel = ForSyDeHierarchy.FunctionLikeEntity.tryView(transformedGraph, transformedGraph.getEdgeSource(e))
                                                .map(functionLikeEntityViewer -> finalReduceInside.kernels().contains(functionLikeEntityViewer))
                                                .orElse(false);
                                        if (!isKernel && e.hasTrait(ForSyDeHierarchy.EdgeTraits.BehaviourCompositionEdge)) {
                                            incomingNonKernels += 1;
                                        }
                                    }
                                    if (incomingNonKernels == 0) {
                                        transformedGraph.removeVertex(reduceInside.getViewedVertex());
                                    }
                                    //deleteHierarchy(transformedGraph, v);
                                    mapsyReducevTransformedGraphs.add(transformedGraph);
                                    //Writing the transformed graphs
//                                forSyDeModelHandler.writeModel(transformedGraph, v.getIdentifier() + "_transformed.fiodl");
//                                forSyDeModelHandler.writeModel(transformedGraph, v.getIdentifier() + "_transformed.kgt");
                                }
                            }
                        }
                    }

                }
            }
        }
        return mapsyReducevTransformedGraphs;
    }
}
