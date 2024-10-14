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
import forsyde.io.lib.hierarchy.visualization.GreyBoxViewer;
import forsyde.io.lib.hierarchy.visualization.VisualizableWithPropertiesViewer;

import java.util.*;

public class ParameterizedMapsyMapvTransformer implements Transformer {

    @Override
    public Set<SystemGraph> applyTransform(SystemGraph systemGraph) {
        int maxNumCore = 6;
        Set<SystemGraph> ParameterizedMapsyMapvTransformedGraphs= new HashSet<>();
        // Synchronous processes recognition
        List<SYMap> syMaps = new ArrayList<>();
        List<MapV> mapVs = new ArrayList<>();
        List<ReduceV> reduceVs = new ArrayList<>();

        // traverse the vertices to find the vectorizables
        final Set<Vectorizable> vectorizables = new HashSet<>();
        for (Vertex v : systemGraph.vertexSet()) {
            if (ForSyDeHierarchy.Vectorizable.tryView(systemGraph, v).isPresent()) {
                vectorizables.add(ForSyDeHierarchy.Vectorizable.tryView(systemGraph, v).get());
            }
        }

        // traverse the vertices
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

                    // MapSY-MapV pattern recognition
                    if (combFunctions.size() == 1) {
                        MapV mapInside = null;
                        for (FunctionLikeEntity combs : combFunctions) {
                            if (ForSyDeHierarchy.MapV.tryView(combs).isPresent()) {
                                mapInside = ForSyDeHierarchy.MapV.tryView(combs).get();
                                System.out.println("---------------------MapSY-MapV------------------------");
                                System.out.println(ForSyDeHierarchy.SYMap.tryView(systemGraph, v).get().getIdentifier() + ": \n" + mapInside.getIdentifier());
                                GreyBoxViewer root = null;
                                for (EdgeInfo e : systemGraph.incomingEdgesOf(v)) {
                                    if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.VisualContainment)) {
                                        final Vertex rootV = systemGraph.getEdgeSource(e);
                                        root = ForSyDeHierarchy.GreyBox.tryView(systemGraph, rootV).get();
                                    }
                                }
                                System.out.println();
                                // MapSY-MapV transformation application

                                //Generating new SYMaps: mapSY(f1), mapSY(f2), ...
                                //Finding the dimensions of the input (which determines the number of mapSYs)
                                Vertex srcSignal = null;
                                Vertex dstSignal = null;
                                List<Integer> srcSigChildDimentions = new ArrayList<>();
                                List<Integer> dstSigChildDimentions = new ArrayList<>();
                                Scanner scanner = new Scanner(System.in);
                                List<Integer> groupSize = new ArrayList<>();
                                for (EdgeInfo e : systemGraph.incomingEdgesOf(v)) {
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
                                for(int i = 2; i< srcSigChildDimentions.get(0); i++) {
                                    if(srcSigChildDimentions.get(0) % i == 0 && (srcSigChildDimentions.get(0)/i)<= maxNumCore) {
                                        groupSize.add(i);
                                    }
                                }
                                for (int j = 0; j < groupSize.size(); j++){
                                    Random random = new Random();
                                    int rand = random.nextInt(1000);
                                    final SystemGraph transformedGraph = new NamedSystemGraph(syMap.getIdentifier()+ rand + "_T");
                                    transformedGraph.mergeInPlace(systemGraph);
                                    // this need to be done since the root was viewed in the wrong ssytem graph
                                    root = ForSyDeHierarchy.GreyBox.tryView(transformedGraph, root.getViewedVertex()).get();
                                    final List<SYMapViewer> newSYMaps = new ArrayList<>();
                                    for (int i=0; i < srcSigChildDimentions.get(0)/groupSize.get(j); i++){
                                        final Vertex parallelStruct = transformedGraph.newVertex("parallelStruct");
                                        parallelStruct.addTraits(new OpaqueTrait("ParStruct"));
                                        parallelStruct.addPort("tuplets");

                                        final SYMapViewer newSYMap = ForSyDeHierarchy.SYMap.enforce(transformedGraph, transformedGraph.newVertex(v.getIdentifier() + "TSY" + i));
                                        final GreyBoxViewer newSYMapGB = ForSyDeHierarchy.GreyBox.enforce(newSYMap);
                                        newSYMap.addCombFunctions(mapInside);
                                        newSYMapGB.addContained(ForSyDeHierarchy.Visualizable.enforce(mapInside));
                                        newSYMapGB.addContained(ForSyDeHierarchy.Visualizable.enforce(mapInside));

                                        transformedGraph.connect(parallelStruct, newSYMap.getViewedVertex(), "tuplets", ForSyDeHierarchy.EdgeTraits.BehaviourCompositionEdge);
                                        //Making the connections between newSYMaps and their hierarchy below
                                        newSYMaps.add(newSYMap);
                                        root.addContained(newSYMapGB);

                                    }
                                    for (EdgeInfo e : transformedGraph.outgoingEdgesOf(v)) {
                                        if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.SYNetworkEdge)) {
                                            dstSignal = transformedGraph.queryVertex(e.getTarget()).get();
                                            for (EdgeInfo ed : transformedGraph.outgoingEdgesOf(dstSignal)) {
                                                if (ed.hasTrait(ForSyDeHierarchy.EdgeTraits.VisualContainment)){
                                                    final Vertex sigChild = transformedGraph.queryVertex(ed.getTarget()).get();
                                                    if (ForSyDeHierarchy.Vectorizable.tryView(transformedGraph, sigChild).isPresent()){
                                                        dstSigChildDimentions = ForSyDeHierarchy.Vectorizable.tryView(transformedGraph, sigChild).get().dimensions();
                                                    }
                                                }
                                            }
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
                                    GreyBoxViewer newunZipxSYGB = ForSyDeHierarchy.GreyBox.enforce(newunZipxSY);
                                    root.addContained(newunZipxSYGB);
                                    //newunZipxSYGB.insertContainedPort(transformedGraph, Visualizable.enforce(concat));

                                    //Connecting the input ports of the original mapSY to the newunZipxSY and the output ports to the new zipxSY
                                    for (String s : ForSyDeHierarchy.SYMap.tryView(transformedGraph, v).get().inputPorts()) {
                                        newZipxSY.getViewedVertex().addPort(s);
                                    }
                                    for (String s : ForSyDeHierarchy.SYMap.tryView(transformedGraph, v).get().outputPorts()) {
                                        newunZipxSY.getViewedVertex().addPort(s);
                                     }
                                    newZipxSY.inputPorts(ForSyDeHierarchy.SYMap.tryView(transformedGraph, v).get().inputPorts());
                                    newunZipxSY.outputPorts(ForSyDeHierarchy.SYMap.tryView(transformedGraph, v).get().outputPorts());

                                    if (ForSyDeHierarchy.SYSignal.tryView(transformedGraph, srcSignal).isPresent()) {
                                        ForSyDeHierarchy.SYSignal.tryView(transformedGraph, srcSignal).get().addConsumers(newZipxSY.inputPorts().get(0), newZipxSY);
                                    }
                                    transformedGraph.connect(srcSignal, ForSyDeHierarchy.SYMap.tryView(newZipxSY).get().getViewedVertex(), "consumers", newZipxSY.inputPorts().get(0), ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                    if (ForSyDeHierarchy.SYSignal.tryView(transformedGraph, dstSignal).isPresent()) {
                                        ForSyDeHierarchy.SYSignal.tryView(transformedGraph, dstSignal).get().producer(newunZipxSY.outputPorts().get(0), newunZipxSY);
                                    }
                                    transformedGraph.connect(ForSyDeHierarchy.SYMap.tryView(newunZipxSY).get().getViewedVertex(), dstSignal, newunZipxSY.outputPorts().get(0), "producer", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                    //Generating signals between newZipxSY and the newSYMaps as well as signals between newSYMaps and newunZipxSY
                                    for (int i = 0; i < newSYMaps.size(); i++) {
                                        final SYMapViewer leftSYMap = newZipxSY;
                                        final SYMapViewer rightSYMap = newSYMaps.get(i);
                                        final SYSignalViewer newSYSignal = ForSyDeHierarchy.SYSignal.enforce(transformedGraph, transformedGraph.newVertex(leftSYMap.getIdentifier() + rightSYMap.getIdentifier() + "Sig"));
                                        //Inserting hierarchy below for the newSYSignals
                                        final VectorizableViewer newVectorizable = ForSyDeHierarchy.Vectorizable.enforce(transformedGraph, transformedGraph.newVertex(newSYSignal.getIdentifier()+"Vec"));
                                        List<Integer> newVectorizableDimentions = new ArrayList<>();
                                        newVectorizableDimentions.add(groupSize.get(j));
                                        for (int k=1; k < srcSigChildDimentions.size(); k++){
                                            newVectorizableDimentions.add(srcSigChildDimentions.get(k));
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
                                        //--------------------------------------------------------------

                                        root.addContained(ForSyDeHierarchy.Visualizable.enforce(newSYSignal));
                                        leftSYMap.getViewedVertex().addPort("newZipxSYTransformedOut");
                                        leftSYMap.outputPorts(List.of("newZipxSYTransformedOut"));
                                        //--------------------------------------------------------
                                        rightSYMap.getViewedVertex().addPort("newSYSignalTransformedIn");
                                        rightSYMap.inputPorts(List.of("newSYSignalTransformedIn"));
                                        newSYSignal.producer("newZipxSYTransformedOut", leftSYMap);
                                        newSYSignal.addConsumers("newSYSignalTransformedIn", rightSYMap);
                                        transformedGraph.connect(leftSYMap,newSYSignal,"newZipxSYTransformedOut", "producer", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                        transformedGraph.connect(newSYSignal,rightSYMap,"consumers", "newSYSignalTransformedIn", ForSyDeHierarchy.EdgeTraits.VisualConnection);

                                    }
                                    for (int i = 0; i < newSYMaps.size(); i++) {
                                        final SYMapViewer leftSYMap = newSYMaps.get(i);
                                        final SYMapViewer rightSYMap = newunZipxSY;
                                        final SYSignalViewer newSYSignal = ForSyDeHierarchy.SYSignal.enforce(transformedGraph, transformedGraph.newVertex(leftSYMap.getIdentifier() + rightSYMap.getIdentifier() + "Sig"));
                                        //Inserting hierarchy below for the newSYSignals
                                        final VectorizableViewer newVectorizable = ForSyDeHierarchy.Vectorizable.enforce(transformedGraph, transformedGraph.newVertex(newSYSignal.getIdentifier()+"Vec"));
                                        List<Integer> newVectorizableDimentions = new ArrayList<>();
                                        newVectorizableDimentions.add(groupSize.get(j));
                                        for (int k=1; k < dstSigChildDimentions.size(); k++){
                                            newVectorizableDimentions.add(dstSigChildDimentions.get(k));
                                        }
                                        newVectorizable.dimensions(newVectorizableDimentions);
                                        final VisualizableWithPropertiesViewer visualizableWithProperties = ForSyDeHierarchy.VisualizableWithProperties.enforce(newVectorizable);
                                        ForSyDeHierarchy.GreyBox.enforce(newSYSignal).addContained(ForSyDeHierarchy.VisualizableWithProperties.enforce(newVectorizable));
                                        visualizableWithProperties.visualizedPropertiesNames(List.of("dimensions"));
                                        newVectorizable.getViewedVertex().addPort("input");
                                        newVectorizable.getViewedVertex().addPort("output");
                                        //Connecting the input and output ports of the newSignal to its hierarchy below
                                        transformedGraph.connect(newSYSignal, newVectorizable, "input", "producer", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                        transformedGraph.connect(newVectorizable, newSYSignal, "consumers", "output", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                        //---------------------------------------------------------------------------

                                        root.addContained(ForSyDeHierarchy.Visualizable.enforce(newSYSignal));
                                        leftSYMap.getViewedVertex().addPort("newSYSignalTransformedOut");
                                        leftSYMap.outputPorts(List.of("newSYSignalTransformedOut"));

                                        //Connecting the incoming and outgoing edges of the combfunction of the newSYMap which are the children of mapInside
                                        for (EdgeInfo e: transformedGraph.outgoingEdgesOf(v)){
                                            final Vertex dstVertex = transformedGraph.getEdgeTarget(e);
                                            if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.VisualContainment)) continue;
                                            if (e.getSourcePort().get().equals("combFunctions")) continue;
                                            if (dstVertex == getOrderedChildren(transformedGraph, ForSyDeHierarchy.SYMap.enforce(transformedGraph, v)).get(0).getViewedVertex()){
                                                transformedGraph.connect(leftSYMap, getOrderedChildren(transformedGraph, leftSYMap).get(0), "newSYSignalTransformedIn",e.getSourcePort().get(), ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                            }
                                        }
                                        for (EdgeInfo e: transformedGraph.incomingEdgesOf(v)){
                                            final Vertex srcVertex = transformedGraph.getEdgeSource(e);
                                            if (e.hasTrait(ForSyDeHierarchy.EdgeTraits.VisualContainment)) continue;
                                            if (e.getSourcePort().get().equals("combFunctions")) continue;
                                            if (srcVertex == getOrderedChildren(transformedGraph, ForSyDeHierarchy.SYMap.enforce(transformedGraph, v)).get(0).getViewedVertex()){
                                                transformedGraph.connect(getOrderedChildren(transformedGraph, leftSYMap).get(0), leftSYMap, e.getSourcePort().get(), "newSYSignalTransformedOut", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                            }

                                        }

                                        rightSYMap.getViewedVertex().addPort("newunZipxSYTransformedIn");
                                        rightSYMap.inputPorts(List.of("newunZipxSYTransformedIn"));
                                        newSYSignal.producer("newSYSignalTransformedOut", leftSYMap);
                                        newSYSignal.addConsumers("newunZipxSYTransformedIn", rightSYMap);
                                        transformedGraph.connect(leftSYMap,newSYSignal,"newSYSignalTransformedOut", "producer", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                        transformedGraph.connect(newSYSignal,rightSYMap,"consumers", "newunZipxSYTransformedIn", ForSyDeHierarchy.EdgeTraits.VisualConnection);
                                    }
                                    transformedGraph.removeVertex(v);
//                                  deleteHierarchy(transformedGraph, v);
                                    ParameterizedMapsyMapvTransformedGraphs.add(transformedGraph);
                                }
                            }
                        }
                    }
                }
            }
        }
        return ParameterizedMapsyMapvTransformedGraphs;
    }
}
