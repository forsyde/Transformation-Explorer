package patternMatch;

import forsyde.io.java.core.*;
import forsyde.io.java.drivers.ForSyDeModelHandler;
import forsyde.io.java.kgt.drivers.ForSyDeKGTDriver;
import forsyde.io.java.typed.viewers.impl.ANSICBlackBoxExecutable;
import forsyde.io.java.typed.viewers.impl.DataBlock;
import forsyde.io.java.typed.viewers.impl.Executable;
import forsyde.io.java.typed.viewers.moc.sy.SYMap;
import forsyde.io.java.typed.viewers.moc.sy.SYSignal;
import forsyde.io.java.typed.viewers.parallel.MapV;
import forsyde.io.java.typed.viewers.parallel.ParallelSkeleton;
import forsyde.io.java.typed.viewers.parallel.ReduceV;
import forsyde.io.java.typed.viewers.parallel.Vectorizable;
import forsyde.io.java.typed.viewers.visualization.GreyBox;
import forsyde.io.java.typed.viewers.visualization.Visualizable;
import forsyde.io.java.typed.viewers.visualization.VisualizableWithProperties;
import org.jgrapht.Graph;
import org.jgrapht.GraphPath;
import org.jgrapht.alg.interfaces.ShortestPathAlgorithm;
import org.jgrapht.alg.shortestpath.DijkstraShortestPath;
import org.jgrapht.graph.AsSubgraph;

import java.util.Scanner;

import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;

public class PatternMatcher {

    public static void main(String[] args) throws Exception {
        System.out.println("Matched patterns:");

        ForSyDeModelHandler forSyDeModelHandler = new ForSyDeModelHandler().registerDriver(new ForSyDeKGTDriver());
        ForSyDeSystemGraph forSyDeSystemGraph = forSyDeModelHandler.loadModel(Paths.get("imageProcessingSY.fiodl"));
        //ForSyDeSystemGraph forSyDeSystemGraph = forSyDeModelHandler.loadModel(Paths.get("maxBrightnessSY_transformed.fiodl"));
        //ForSyDeSystemGraph forSyDeSystemGraph = forSyDeModelHandler.loadModel(Paths.get("maxBrightnessMapVtransformedSY_transformed.fiodl"));

        // Synchronous processes recognition
        List<SYMap> syMaps = new ArrayList<>();
        List<MapV> mapVs = new ArrayList<>();
        List<ReduceV> reduceVs = new ArrayList<>();

        // traverse the vertices to find the vectorizables
        final Set<Vectorizable> vectorizables = new HashSet<>();
        for (Vertex v : forSyDeSystemGraph.vertexSet()) {
            if (Vectorizable.conforms(v)) {
                vectorizables.add(Vectorizable.safeCast(v).get());
            }
        }

        // traverse the vertices
        Map<SYMap, List<Executable>> processesAndOrderedExecutables = new HashMap<>();
        for (Vertex v : forSyDeSystemGraph.vertexSet()) {

            // SY vertices recognition
            if (SYMap.conforms(v)) {
                syMaps.add(SYMap.safeCast(v).get());

                int inPortNum = 0;
                for (EdgeInfo e : forSyDeSystemGraph.incomingEdgesOf(v)) {
                    if (e.hasTrait(EdgeTrait.MOC_SY_SYDATAEDGE)) {
                        inPortNum += 1;
                    }
                }
                int outPortNum = 0;
                for (EdgeInfo e : forSyDeSystemGraph.outgoingEdgesOf(v)) {
                    if (e.hasTrait(EdgeTrait.MOC_SY_SYDATAEDGE)) {
                        outPortNum += 1;
                    }
                }

                //System.out.println(v.getIdentifier() + ", inputs is " + inPortNum + ", and output is " + outPortNum);

                // mapSY recognition
                if (inPortNum == 1 && outPortNum == 1) {
                    SYMap syMap = SYMap.safeCast(v).get();

                    final Set<Executable> combFunctions = syMap.getCombFunctionsPort(forSyDeSystemGraph);

                    // MapSY-ReduceV pattern recognition
                    if (combFunctions.size() == 1) {
                        ReduceV reduceInside = null;
                        for (Executable combs : combFunctions) {
                            if (ReduceV.conforms(combs)) {
                                reduceInside = ReduceV.safeCast(combs).get();
                                System.out.println("---------------------MapSY-ReduceV------------------------");
                                System.out.println(SYMap.safeCast(v).get().getIdentifier() + ": \n" + reduceInside.getIdentifier());
                                GreyBox root = null;
                                for (EdgeInfo e : forSyDeSystemGraph.incomingEdgesOf(v)){
                                    if (e.hasTrait(EdgeTrait.VISUALIZATION_VISUALCONTAINMENT)){
                                        final Vertex rootV = forSyDeSystemGraph.getEdgeSource(e);
                                        root = GreyBox.safeCast(rootV).get();
                                    }
                                }
                                System.out.println();
                                // MapSY-ReduceV transformation application
                                final ForSyDeSystemGraph transformedGraph = new ForSyDeSystemGraph().merge(forSyDeSystemGraph);
                                final List<SYMap> newSYMaps = new ArrayList<>();
                                final List<SYSignal> newSYSignals = new ArrayList<>();
                                //Generating new SYMaps: mapSY(f1), mapSY(f2), ...
                                //Finding the dimensions of the input (which determines the number of mapSYs)
                                Vertex srcSignal = null;
                                Vertex dstSignal = null;
                                List<Integer> sigChildDimentions = new ArrayList<>();
                                for (EdgeInfo e : forSyDeSystemGraph.incomingEdgesOf(v)) {
                                    if (e.hasTrait(EdgeTrait.MOC_SY_SYDATAEDGE)) {
                                        srcSignal = forSyDeSystemGraph.queryVertex(e.getSource()).get();
                                        for (EdgeInfo ed : forSyDeSystemGraph.outgoingEdgesOf(srcSignal)) {
                                            if (ed.hasTrait(EdgeTrait.VISUALIZATION_VISUALCONTAINMENT)){
                                                final Vertex sigChild = forSyDeSystemGraph.queryVertex(ed.getTarget()).get();
                                                if (Vectorizable.conforms(sigChild)){
                                                    sigChildDimentions = Vectorizable.safeCast(sigChild).get().getDimensions();
                                                    //The first dimension which is the number of rows in the case of a matrix is the number of mapSYs.
                                                    for (int i=0; i < sigChildDimentions.get(0)-1; i++){
                                                        final SYMap newSYMap = SYMap.enforce(transformedGraph.newVertex(v.getIdentifier() + "transformedSY" + i));
                                                        final GreyBox newSYMapGB = GreyBox.enforce(newSYMap);
                                                        for (Executable child: getOrderedChildren(forSyDeSystemGraph, reduceInside)){
                                                            newSYMap.insertCombFunctionsPort(transformedGraph, child);
                                                            newSYMapGB.insertContainedPort(transformedGraph, Visualizable.enforce(child));
                                                        }

                                                        newSYMap.getViewedVertex().addPort("newSYSignalTransformedIn1");
                                                        newSYMap.getViewedVertex().addPort("newSYSignalTransformedIn2");
                                                        newSYMap.setInputPorts(List.of("newSYSignalTransformedIn1", "newSYSignalTransformedIn2"));

                                                        newSYMap.getViewedVertex().addPort("newSYMapTransformedOut");
                                                        newSYMap.setOutputPorts(Collections.singletonList("newSYMapTransformedOut"));

                                                        newSYMaps.add(newSYMap);
                                                        root.insertContainedPort(transformedGraph, newSYMapGB);



                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                                for (EdgeInfo e : forSyDeSystemGraph.outgoingEdgesOf(v)) {
                                    if (e.hasTrait(EdgeTrait.MOC_SY_SYDATAEDGE)) {
                                        dstSignal = forSyDeSystemGraph.queryVertex(e.getTarget()).get();
                                    }
                                }
                                //Generating the unZipxSY
                                final SYMap newunZipxSY = SYMap.enforce(transformedGraph.newVertex(v.getIdentifier() + "unZipxSYtransformedSY"));
                                //newunZipxSY.insertCombFunctionsPort(transformedGraph, concat);
                                GreyBox newunZipxSYGB = GreyBox.enforce(newunZipxSY);
                                root.insertContainedPort(transformedGraph, newunZipxSYGB);
                                //newunZipxSYGB.insertContainedPort(transformedGraph, Visualizable.enforce(concat));

                                //Connecting the input ports of the original mapSY to the newunZipxSY
                                for (String s : SYMap.safeCast(v).get().getInputPorts()) {
                                    newunZipxSY.getViewedVertex().addPort(s);
                                }
                                newunZipxSY.setInputPorts(SYMap.safeCast(v).get().getInputPorts());

                                if (SYSignal.conforms(srcSignal)) {
                                    SYSignal.safeCast(srcSignal).get().setOutputPort(transformedGraph, newunZipxSY, newunZipxSY.getInputPorts().get(0));
                                }
                                transformedGraph.connect(srcSignal, SYMap.safeCast(newunZipxSY).get().getViewedVertex(), "output", newunZipxSY.getInputPorts().get(0), EdgeTrait.VISUALIZATION_VISUALCONNECTION);

                                //Generating signals between newunZipxSY and the newSYMaps
                                for (int i = 0; i < sigChildDimentions.get(0); i++) {
                                    final SYSignal newSYSignal = SYSignal.enforce(transformedGraph.newVertex(newunZipxSY.getIdentifier() + v.getIdentifier() + "transformedSY" + i + "Sig"));
                                    newSYSignals.add(newSYSignal);
                                    //Inserting hierarchy below for the newSYSignals
                                    if (sigChildDimentions.size() > 1) {
                                        final Vectorizable newVectorizable = Vectorizable.enforce(transformedGraph.newVertex(newSYSignal.getIdentifier()+"Vec"));
                                        List<Integer> newVectorizableDimentions = new ArrayList<>();
                                        for (int j=1; j < sigChildDimentions.size(); j++){
                                            newVectorizableDimentions.add(sigChildDimentions.get(j));
                                        }
                                        newVectorizable.setDimensions(newVectorizableDimentions);
                                        final VisualizableWithProperties visualizableWithProperties = VisualizableWithProperties.enforce(newVectorizable);
                                        GreyBox.enforce(newSYSignal).insertContainedPort(transformedGraph, visualizableWithProperties);
                                        visualizableWithProperties.setVisualizedPropertiesNames(List.of("dimensions"));
                                        //newVectorizable.getViewedVertex().getProperties().put("visualizedPropertiesNames", VertexProperty.create("Dimentions"));
                                        // newVectorizable.getViewedVertex()
                                        newVectorizable.getViewedVertex().addPort("input");
                                        newVectorizable.getViewedVertex().addPort("output");
                                        //Connecting the input and output ports of the newSignal to its hierarchy below
                                        transformedGraph.connect(newSYSignal, newVectorizable, "input", "input", EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                        transformedGraph.connect(newVectorizable, newSYSignal, "output", "output", EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                    }

                                    root.insertContainedPort(transformedGraph, Visualizable.enforce(newSYSignal));
                                    newunZipxSY.getViewedVertex().addPort("newunZipxSYTransformedOut");
                                    newunZipxSY.setOutputPorts(List.of("newunZipxSYTransformedOut"));
                                    //--------------------------------------------------------
//

//                                    newSYSignal.setInputPort(transformedGraph, "newunZipxSYTransformedOut", leftSYMap);
//                                    newSYSignal.setOutputPort(transformedGraph, rightSYMap, "newSYSignalTransformedIn");
//                                    transformedGraph.connect(leftSYMap,newSYSignal,"newunZipxSYTransformedOut", "input", EdgeTrait.VISUALIZATION_VISUALCONNECTION);
//                                    transformedGraph.connect(newSYSignal,rightSYMap,"output", "newSYSignalTransformedIn", EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                }
                                //Making the connections
                                //The connections of the first mapSY in the chain
                                newSYSignals.get(0).setInputPort(transformedGraph, "newunZipxSYTransformedOut", newunZipxSY);
                                transformedGraph.connect(newunZipxSY, newSYSignals.get(0), "newunZipxSYTransformedOut", "input", EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                newSYSignals.get(0).setOutputPort(transformedGraph, newSYMaps.get(0), newSYMaps.get(0).getInputPorts().get(0));
                                transformedGraph.connect(newSYSignals.get(0), newSYMaps.get(0), "output", newSYMaps.get(0).getInputPorts().get(0), EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                newSYSignals.get(1).setInputPort(transformedGraph, "newunZipxSYTransformedOut", newunZipxSY);
                                transformedGraph.connect(newunZipxSY, newSYSignals.get(1), "newunZipxSYTransformedOut", "input", EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                newSYSignals.get(1).setOutputPort(transformedGraph, newSYMaps.get(0), newSYMaps.get(0).getInputPorts().get(1));
                                transformedGraph.connect(newSYSignals.get(1), newSYMaps.get(0), "output", newSYMaps.get(0).getInputPorts().get(1), EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                // The connections of the mapSYs except the first one
                                for (int i=2; i < newSYSignals.size(); i++){
                                    newSYSignals.get(i).setInputPort(transformedGraph, "newunZipxSYTransformedOut", newunZipxSY);
                                    transformedGraph.connect(newunZipxSY, newSYSignals.get(i), "newunZipxSYTransformedOut", "input", EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                    newSYSignals.get(i).setOutputPort(transformedGraph, newSYMaps.get(i-1), newSYMaps.get(i-1).getInputPorts().get(1));
                                    transformedGraph.connect(newSYSignals.get(i), newSYMaps.get(i-1), "output", newSYMaps.get(i-1).getInputPorts().get(1), EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                }
                                for (int i=1; i < newSYMaps.size(); i++){
                                    transformedGraph.connect(newSYMaps.get(i-1), newSYMaps.get(i), "newSYMapTransformedOut", newSYMaps.get(i).getInputPorts().get(0), EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                }

                                //Connecting the incoming and outgoing edges of the combfunction of the newSYMap which are the children of reduceInside
                                for (int i=0; i < newSYMaps.size(); i++){
                                    final Executable child = getOrderedChildren(transformedGraph, newSYMaps.get(i)).get(0);
                                    if (ANSICBlackBoxExecutable.conforms(child)) {
                                        final List<String> childInputs = ANSICBlackBoxExecutable.safeCast(child).get().getInputArgumentPorts();
                                        final List<String> childOutputs = ANSICBlackBoxExecutable.safeCast(child).get().getOutputArgumentPorts();
                                        for (int j = 0; j < childInputs.size(); j++) {
                                            transformedGraph.connect(newSYMaps.get(i), child, newSYMaps.get(i).getInputPorts().get(j), childInputs.get(j), EdgeTrait.MOC_ABSTRACTIONEDGE, EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                        }
                                        for (int j = 0; j < childOutputs.size(); j++) {
                                            transformedGraph.connect(child, newSYMaps.get(i), childOutputs.get(j), newSYMaps.get(i).getOutputPorts().get(j), EdgeTrait.MOC_ABSTRACTIONEDGE, EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                        }
                                        transformedGraph.connect(child, newSYMaps.get(i), ANSICBlackBoxExecutable.safeCast(child).get().getReturnPort(), newSYMaps.get(i).getOutputPorts().get(newSYMaps.get(i).getOutputPorts().size() - 1), EdgeTrait.MOC_ABSTRACTIONEDGE, EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                    } else {
                                        final List<String> portsWithSomeOrder = new ArrayList<>(child.getPorts());
                                        for (int j = 0; j < newSYMaps.get(i).getInputPorts().size() && j < portsWithSomeOrder.size(); j++) {
                                            transformedGraph.connect(newSYMaps.get(i), child, newSYMaps.get(i).getInputPorts().get(j), portsWithSomeOrder.get(j), EdgeTrait.MOC_ABSTRACTIONEDGE, EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                        }
                                        for (int j = newSYMaps.get(i).getInputPorts().size(); j < portsWithSomeOrder.size(); j++) {
                                            transformedGraph.connect(child, newSYMaps.get(i), newSYMaps.get(i).getOutputPorts().get(j - newSYMaps.get(i).getInputPorts().size()), portsWithSomeOrder.get(j), EdgeTrait.MOC_ABSTRACTIONEDGE, EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                        }
                                    }
                                }
                                //Connecting the output of the last newMapSY to the target signal of the original vertex.
                                for (EdgeInfo e: forSyDeSystemGraph.outgoingEdgesOf(v)){
                                    final Vertex dstVertex = forSyDeSystemGraph.getEdgeTarget(e);
                                    if (e.hasTrait(EdgeTrait.VISUALIZATION_VISUALCONTAINMENT)) continue;
                                    if (e.getSourcePort().get().equals("combFunctions")) continue;
                                    if (!(dstVertex == getOrderedChildren(forSyDeSystemGraph, SYMap.enforce(v)).get(0).getViewedVertex())){
                                        transformedGraph.connect(newSYMaps.get(newSYMaps.size()-1).getViewedVertex(), dstVertex, "newSYMapTransformedOut", e.getTargetPort().get(), EdgeTrait.VISUALIZATION_VISUALCONNECTION);                                    }
                                }

                                //Removing the original mapSY as well as hierarchy inside it besides the signals.
                                Vertex childV = getOrderedChildren(forSyDeSystemGraph, SYMap.enforce(v)).get(0).getViewedVertex();
//                                transformedGraph.removeVertex(childV);
//                                transformedGraph.removeVertex(v);
                                //Writing the transformed graphs
                                forSyDeModelHandler.writeModel(transformedGraph, v.getIdentifier() + "_transformed.fiodl");
                                forSyDeModelHandler.writeModel(transformedGraph, v.getIdentifier() + "_transformed.kgt");


                            }
                        }
                    }

                    // MapSY-MapV pattern recognition
                    if (combFunctions.size() == 1) {
                        MapV mapInside = null;
                        for (Executable combs : combFunctions) {
                            if (MapV.conforms(combs)) {
                                mapInside = MapV.safeCast(combs).get();
                                System.out.println("---------------------MapSY-MapV------------------------");
                                System.out.println(SYMap.safeCast(v).get().getIdentifier() + ": \n" + mapInside.getIdentifier());
                                GreyBox root = null;
                                for (EdgeInfo e : forSyDeSystemGraph.incomingEdgesOf(v)){
                                    if (e.hasTrait(EdgeTrait.VISUALIZATION_VISUALCONTAINMENT)){
                                        final Vertex rootV = forSyDeSystemGraph.getEdgeSource(e);
                                        root = GreyBox.safeCast(rootV).get();
                                    }
                                }
                                System.out.println();
                                // MapSY-MapV transformation application
                                final ForSyDeSystemGraph transformedGraph = new ForSyDeSystemGraph().merge(forSyDeSystemGraph);
                                final List<SYMap> newSYMaps = new ArrayList<>();
                                //Generating new SYMaps: mapSY(f1), mapSY(f2), ...
                                //Finding the dimensions of the input (which determines the number of mapSYs)
                                Vertex srcSignal = null;
                                Vertex dstSignal = null;
                                List<Integer> sigChildDimentions = new ArrayList<>();
                                for (EdgeInfo e : forSyDeSystemGraph.incomingEdgesOf(v)) {
                                    if (e.hasTrait(EdgeTrait.MOC_SY_SYDATAEDGE)) {
                                        srcSignal = forSyDeSystemGraph.queryVertex(e.getSource()).get();
                                        for (EdgeInfo ed : forSyDeSystemGraph.outgoingEdgesOf(srcSignal)) {
                                            if (ed.hasTrait(EdgeTrait.VISUALIZATION_VISUALCONTAINMENT)){
                                                final Vertex sigChild = forSyDeSystemGraph.queryVertex(ed.getTarget()).get();
                                                if (Vectorizable.conforms(sigChild)){
                                                    sigChildDimentions = Vectorizable.safeCast(sigChild).get().getDimensions();
                                                    //The first dimension which is the number of rows in the case of a matrix is the number of mapSYs.
                                                    for (int i=0; i < sigChildDimentions.get(0); i++){
                                                        final SYMap newSYMap = SYMap.enforce(transformedGraph.newVertex(v.getIdentifier() + "transformedSY" + i));
                                                        final GreyBox newSYMapGB = GreyBox.enforce(newSYMap);
                                                        for (Executable child: getOrderedChildren(forSyDeSystemGraph, mapInside)){
                                                            newSYMap.insertCombFunctionsPort(transformedGraph, child);
                                                            newSYMapGB.insertContainedPort(transformedGraph, Visualizable.enforce(child));
                                                        }
                                                        //Making the connections between newSYMaps and their hierarchy below
                                                        newSYMaps.add(newSYMap);
                                                        root.insertContainedPort(transformedGraph, newSYMapGB);

                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                                for (EdgeInfo e : forSyDeSystemGraph.outgoingEdgesOf(v)) {
                                    if (e.hasTrait(EdgeTrait.MOC_SY_SYDATAEDGE)) {
                                        dstSignal = forSyDeSystemGraph.queryVertex(e.getTarget()).get();
                                    }
                                }
                                //Generating the zipxSY and unZipxSY
                                final SYMap newZipxSY = SYMap.enforce(transformedGraph.newVertex(v.getIdentifier() + "zipxSYtransformedSY"));
                                //newZipxSY.insertCombFunctionsPort(transformedGraph, concat);
                                GreyBox newZipxSYGB = GreyBox.enforce(newZipxSY);
                                root.insertContainedPort(transformedGraph, newZipxSYGB);
                                //newZipxSYGB.insertContainedPort(transformedGraph, Visualizable.enforce(concat));
                                final SYMap newunZipxSY = SYMap.enforce(transformedGraph.newVertex(v.getIdentifier() + "unZipxSYtransformedSY"));
                                //newunZipxSY.insertCombFunctionsPort(transformedGraph, concat);
                                GreyBox newunZipxSYGB = GreyBox.enforce(newunZipxSY);
                                root.insertContainedPort(transformedGraph, newunZipxSYGB);
                                //newunZipxSYGB.insertContainedPort(transformedGraph, Visualizable.enforce(concat));

                                //Connecting the input ports of the original mapSY to the newunZipxSY and the output ports to the new zipxSY
                                for (String s : SYMap.safeCast(v).get().getInputPorts()) {
                                    newunZipxSY.getViewedVertex().addPort(s);
                                }
                                for (String s : SYMap.safeCast(v).get().getOutputPorts()) {
                                    newZipxSY.getViewedVertex().addPort(s);
                                }
                                newunZipxSY.setInputPorts(SYMap.safeCast(v).get().getInputPorts());
                                newZipxSY.setOutputPorts(SYMap.safeCast(v).get().getOutputPorts());

                                if (SYSignal.conforms(srcSignal)) {
                                    SYSignal.safeCast(srcSignal).get().setOutputPort(transformedGraph, newunZipxSY, newunZipxSY.getInputPorts().get(0));
                                }
                                transformedGraph.connect(srcSignal, SYMap.safeCast(newunZipxSY).get().getViewedVertex(), "output", newunZipxSY.getInputPorts().get(0), EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                if (SYSignal.conforms(dstSignal)) {
                                    SYSignal.safeCast(dstSignal).get().setInputPort(transformedGraph, newZipxSY.getOutputPorts().get(0), newZipxSY);
                                }
                                transformedGraph.connect(SYMap.safeCast(newZipxSY).get().getViewedVertex(), dstSignal, newZipxSY.getOutputPorts().get(0), "input", EdgeTrait.VISUALIZATION_VISUALCONNECTION);

                                //Generating signals between newZipxSY and the newSYMaps as well as signals between newSYMaps and newunZipxSY
                                for (int i = 0; i < newSYMaps.size(); i++) {
                                    final SYMap leftSYMap = newunZipxSY;
                                    final SYMap rightSYMap = newSYMaps.get(i);
                                    final SYSignal newSYSignal = SYSignal.enforce(transformedGraph.newVertex(leftSYMap.getIdentifier() + rightSYMap.getIdentifier() + "Sig"));
                                    //Inserting hierarchy below for the newSYSignals
                                    if (sigChildDimentions.size() > 1){
                                        final Vectorizable newVectorizable = Vectorizable.enforce(transformedGraph.newVertex(newSYSignal.getIdentifier()+"Vec"));
                                        List<Integer> newVectorizableDimentions = new ArrayList<>();
                                        for (int j=1; j < sigChildDimentions.size(); j++){
                                            newVectorizableDimentions.add(sigChildDimentions.get(j));
                                        }
                                        newVectorizable.setDimensions(newVectorizableDimentions);
                                        final VisualizableWithProperties visualizableWithProperties = VisualizableWithProperties.enforce(newVectorizable);
                                        GreyBox.enforce(newSYSignal).insertContainedPort(transformedGraph, visualizableWithProperties);
                                        visualizableWithProperties.setVisualizedPropertiesNames(List.of("dimensions"));
                                        //newVectorizable.getViewedVertex().getProperties().put("visualizedPropertiesNames", VertexProperty.create("Dimentions"));
                                        // newVectorizable.getViewedVertex()
                                        newVectorizable.getViewedVertex().addPort("input");
                                        newVectorizable.getViewedVertex().addPort("output");
                                        //Connecting the input and output ports of the newSignal to its hierarchy below
                                        transformedGraph.connect(newSYSignal, newVectorizable, "input", "input", EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                        transformedGraph.connect(newVectorizable, newSYSignal, "output", "output", EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                    }
                                    root.insertContainedPort(transformedGraph, Visualizable.enforce(newSYSignal));
                                    leftSYMap.getViewedVertex().addPort("newunZipxSYTransformedOut");
                                    leftSYMap.setOutputPorts(List.of("newunZipxSYTransformedOut"));
                                    //--------------------------------------------------------
                                    rightSYMap.getViewedVertex().addPort("newSYSignalTransformedIn");
                                    rightSYMap.setInputPorts(List.of("newSYSignalTransformedIn"));
                                    newSYSignal.setInputPort(transformedGraph, "newunZipxSYTransformedOut", leftSYMap);
                                    newSYSignal.setOutputPort(transformedGraph, rightSYMap, "newSYSignalTransformedIn");
                                    transformedGraph.connect(leftSYMap,newSYSignal,"newunZipxSYTransformedOut", "input", EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                    transformedGraph.connect(newSYSignal,rightSYMap,"output", "newSYSignalTransformedIn", EdgeTrait.VISUALIZATION_VISUALCONNECTION);

                                }
                                for (int i = 0; i < newSYMaps.size(); i++) {
                                    final SYMap leftSYMap = newSYMaps.get(i);
                                    final SYMap rightSYMap = newZipxSY;
                                    final SYSignal newSYSignal = SYSignal.enforce(transformedGraph.newVertex(leftSYMap.getIdentifier() + rightSYMap.getIdentifier() + "Sig"));
                                    //Inserting hierarchy below for the newSYSignals
                                    if (sigChildDimentions.size() > 1){
                                        final Vectorizable newVectorizable = Vectorizable.enforce(transformedGraph.newVertex(newSYSignal.getIdentifier()+"Vec"));
                                        List<Integer> newVectorizableDimentions = new ArrayList<>();
                                        for (int j=1; j < sigChildDimentions.size(); j++){
                                            newVectorizableDimentions.add(sigChildDimentions.get(j));
                                        }
                                        newVectorizable.setDimensions(newVectorizableDimentions);
                                        final VisualizableWithProperties visualizableWithProperties = VisualizableWithProperties.enforce(newVectorizable);
                                        GreyBox.enforce(newSYSignal).insertContainedPort(transformedGraph, VisualizableWithProperties.enforce(newVectorizable));
                                        visualizableWithProperties.setVisualizedPropertiesNames(List.of("dimensions"));
                                        newVectorizable.getViewedVertex().addPort("input");
                                        newVectorizable.getViewedVertex().addPort("output");
                                        //Connecting the input and output ports of the newSignal to its hierarchy below
                                        transformedGraph.connect(newSYSignal, newVectorizable, "input", "input", EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                        transformedGraph.connect(newVectorizable, newSYSignal, "output", "output", EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                    }
                                    root.insertContainedPort(transformedGraph, Visualizable.enforce(newSYSignal));
                                    leftSYMap.getViewedVertex().addPort("newSYSignalTransformedOut");
                                    leftSYMap.setOutputPorts(List.of("newSYSignalTransformedOut"));

                                    //Connecting the incoming and outgoing edges of the combfunction of the newSYMap which are the children of mapInside
                                    for (EdgeInfo e: forSyDeSystemGraph.outgoingEdgesOf(v)){
                                        final Vertex dstVertex = forSyDeSystemGraph.getEdgeTarget(e);
                                        if (e.hasTrait(EdgeTrait.VISUALIZATION_VISUALCONTAINMENT)) continue;
                                        if (e.getSourcePort().get().equals("combFunctions")) continue;
                                        if (dstVertex == getOrderedChildren(forSyDeSystemGraph, SYMap.enforce(v)).get(0).getViewedVertex()){
                                            transformedGraph.connect(leftSYMap, getOrderedChildren(transformedGraph, leftSYMap).get(0), "newSYSignalTransformedIn",e.getSourcePort().get(), EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                        }
                                    }
                                    for (EdgeInfo e: forSyDeSystemGraph.incomingEdgesOf(v)){
                                        final Vertex srcVertex = forSyDeSystemGraph.getEdgeSource(e);
                                        if (e.hasTrait(EdgeTrait.VISUALIZATION_VISUALCONTAINMENT)) continue;
                                        if (e.getSourcePort().get().equals("combFunctions")) continue;
                                        if (srcVertex == getOrderedChildren(forSyDeSystemGraph, SYMap.enforce(v)).get(0).getViewedVertex()){
                                            transformedGraph.connect(getOrderedChildren(transformedGraph, leftSYMap).get(0), leftSYMap, e.getSourcePort().get(), "newSYSignalTransformedOut", EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                        }

                                    }
                                    rightSYMap.getViewedVertex().addPort("newZipxSYTransformedIn");
                                    rightSYMap.setInputPorts(List.of("newZipxSYTransformedIn"));
                                    newSYSignal.setInputPort(transformedGraph, "newSYSignalTransformedOut", leftSYMap);
                                    newSYSignal.setOutputPort(transformedGraph, rightSYMap, "newZipxSYTransformedIn");
                                    transformedGraph.connect(leftSYMap,newSYSignal,"newSYSignalTransformedOut", "input", EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                    transformedGraph.connect(newSYSignal,rightSYMap,"output", "newZipxSYTransformedIn", EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                }
                                //---------------------------------------------------------------------------
                                //Removing the original mapSY as well as hierarchy inside it besides the signals.
                                Vertex childV = getOrderedChildren(forSyDeSystemGraph, SYMap.safeCast(v).get()).get(0).getViewedVertex();
                                transformedGraph.removeVertex(childV);
                                transformedGraph.removeVertex(v);
                                //Writing the transformed graphs
                                forSyDeModelHandler.writeModel(transformedGraph, v.getIdentifier() + "_transformed.fiodl");
                                forSyDeModelHandler.writeModel(transformedGraph, v.getIdentifier() + "_transformed.kgt");
                            }
                        }
                    }

                    // zipxSY recognition
                    /*if (inPortNum > 1 && outPortNum ==1 && ){
                        if (combFunctions.size() == 1){
                            SYMap concatInside = null;
                            for (Executable combs: combFunctions) {
                                if (concat.conforms(combs)) {
                                    concatInside = SYMap.safeCast(combs).get();
                                    System.out.println("zipxSY matched:" + SYMap.safeCast(v).get().getIdentifier() + " with " + concatInside.getIdentifier());
                                }
                            }
                        }
                    }*/

                    // unzipxSY recognition
                    /*if (outPortNum > 1 && inPortNum ==1 && ){
                        if (combFunctions.size() == 1){
                            SYMap unconcatInside = null;
                            for (Executable combs: combFunctions) {
                                if (unconcat.conforms(combs)) {
                                    unconcatInside = SYMap.safeCast(combs).get();
                                    System.out.println("unzipxSY matched:" + SYMap.safeCast(v).get().getIdentifier() + " with " + unconcatInside.getIdentifier());
                                }
                            }
                        }
                    }*/

                    final Set<EdgeInfo> parallelComputationEdges = new HashSet<>();
                    for (EdgeInfo e : forSyDeSystemGraph.edgeSet()) {
                        if (e.hasTrait(EdgeTrait.PARALLEL_PARALLELCOMPUTATIONEDGE)) {
                            parallelComputationEdges.add(e);
                        }
                    }
                    // Finding the order of the combination functions inside mapSY (for the sake of Map-Split transformation)
                    final Graph<Vertex, EdgeInfo> combFunctionsGraph = new AsSubgraph<>(forSyDeSystemGraph, forSyDeSystemGraph.vertexSet(), parallelComputationEdges);
                    final ShortestPathAlgorithm<Vertex, EdgeInfo> shortestPathAlgorithm = new DijkstraShortestPath<>(combFunctionsGraph);
                    List<Executable> combFunctionsHaskellSorted = new ArrayList<>(combFunctions);
                    combFunctionsHaskellSorted.sort((exe, exe2) -> {
                        final GraphPath<Vertex, EdgeInfo> path = shortestPathAlgorithm.getPath(exe.getViewedVertex(), exe2.getViewedVertex());
                        if (exe == exe2) {
                            return 0;
                        } else if (path == null) {
                            return 1;
                        } else {
                            return -1;
                        }
                    });
                    processesAndOrderedExecutables.put(SYMap.safeCast(v).get(), combFunctionsHaskellSorted);


//                System.out.println(v.getIdentifier());
//                System.out.println(combFunctions.stream().map(VertexViewer::getIdentifier).collect(Collectors.joining(", ")));
//                System.out.println(combFunctionsHaskellSorted.stream().map(VertexViewer::getIdentifier).collect(Collectors.joining(", ")));
                    while (combFunctionsHaskellSorted.size() < combFunctions.size()) {
                        boolean isNext = false;
                        final Executable exe = combFunctionsHaskellSorted.get(combFunctionsHaskellSorted.size() - 1);
                        for (Executable exe2 : combFunctions) {
                            if (!combFunctionsHaskellSorted.contains(exe2)) {
                                for (EdgeInfo e : forSyDeSystemGraph.getAllEdges(exe.getViewedVertex(), exe2.getViewedVertex())) {
                                    if (e.hasTrait(EdgeTrait.PARALLEL_PARALLELCOMPUTATIONEDGE)) {
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

            // Balanced-Tree (reduceV) pattern recognition
            if (ReduceV.conforms(v)) {
                System.out.println("---------------------Balanced-Tree------------------------");
                System.out.println(v.getIdentifier());
                // System.out.println(v.getIdentifier());
            }


        }

        for (SYMap syMap : processesAndOrderedExecutables.keySet()) {
            GreyBox root = null;
            for (EdgeInfo e : forSyDeSystemGraph.incomingEdgesOf(syMap.getViewedVertex())){
                if (e.hasTrait(EdgeTrait.VISUALIZATION_VISUALCONTAINMENT)){
                   final Vertex rootV = forSyDeSystemGraph.getEdgeSource(e);
                   root = GreyBox.safeCast(rootV).get();
                }
            }
            if (!processesAndOrderedExecutables.get(syMap).isEmpty() && processesAndOrderedExecutables.get(syMap).size() > 1) {
                System.out.println("---------------------Map-Split----------------------------");
                System.out.println(syMap.getIdentifier() + ": ");
                for (Executable executable : processesAndOrderedExecutables.get(syMap)) {
                    System.out.print(executable.getIdentifier() + "\t");
                }
                System.out.println();
                // Map-Split transformation application
                final ForSyDeSystemGraph transformedGraph = new ForSyDeSystemGraph().merge(forSyDeSystemGraph);
                final List<SYMap> newSYMaps = new ArrayList<>();
                //Generating new SYMaps: mapSY(f), mapSY(g), ...
                for (Executable executable :
                        processesAndOrderedExecutables.get(syMap)) {
                    final SYMap newSYMap = SYMap.enforce(transformedGraph.newVertex(executable.getIdentifier() + "transformedSY"));
                    newSYMap.insertCombFunctionsPort(transformedGraph, executable);
                    newSYMaps.add(newSYMap);
                    GreyBox newSYMapGB = GreyBox.enforce(newSYMap);
                    root.insertContainedPort(transformedGraph, newSYMapGB);
                    newSYMapGB.insertContainedPort(transformedGraph, Visualizable.enforce(executable));
                }
                //Generating signals between the new SYMaps: the signal between mapSY(f) and mapSY(g), ...
                for (int i = 0; i < newSYMaps.size() - 1; i++) {
                    final SYMap leftSYMap = newSYMaps.get(i);
                    final SYMap rightSYMap = newSYMaps.get(i + 1);
                    final SYSignal newSYSignal = SYSignal.enforce(transformedGraph.newVertex(leftSYMap.getIdentifier() + rightSYMap.getIdentifier() + "Sig"));
                    root.insertContainedPort(transformedGraph, Visualizable.enforce(newSYSignal));
                    //working on this part
                    //for (int j = 0; j < processesAndOrderedExecutables.get(syMap).size() - 1; j++) {
                        final Executable exe1 = processesAndOrderedExecutables.get(syMap).get(i);
                        final Executable exe2 = processesAndOrderedExecutables.get(syMap).get(i + 1);
                        for (EdgeInfo e1 : forSyDeSystemGraph.outgoingEdgesOf(exe1.getViewedVertex())) {
                            final Vertex v1 = forSyDeSystemGraph.getEdgeTarget(e1);
                            for (EdgeInfo e2 : forSyDeSystemGraph.incomingEdgesOf(exe2.getViewedVertex())) {
                                final Vertex v2 = forSyDeSystemGraph.getEdgeSource(e2);
                                if (v1.getIdentifier() == v2.getIdentifier() && DataBlock.conforms(v1)) {
                                    GreyBox.enforce(newSYSignal).insertContainedPort(transformedGraph, Visualizable.enforce(v1));;
                                    transformedGraph.connect(newSYSignal.getViewedVertex(), v1, "input", "input", EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                                    transformedGraph.connect(v1, newSYSignal.getViewedVertex(), "output", "output", EdgeTrait.VISUALIZATION_VISUALCONNECTION);

                                }
                            }
                        }
                    //}
                    //-----------------------------------------------------------------------------------

                    leftSYMap.getViewedVertex().addPort("mapSplitTransformedOut");
                    leftSYMap.setOutputPorts(List.of("mapSplitTransformedOut"));
                    //Connecting the incoming and outgoing edges of the combfunction of the newSYMap which is the executable
                    for (EdgeInfo e : forSyDeSystemGraph.outgoingEdgesOf(getOrderedChildren(transformedGraph, leftSYMap).get(0).getViewedVertex())){
                        final Vertex dstVertex = transformedGraph.getEdgeTarget(e);
                        if (e.hasTrait(EdgeTrait.VISUALIZATION_VISUALCONTAINMENT)) continue;
                        if (MapV.safeCast(getOrderedChildren(transformedGraph, leftSYMap).get(0).getViewedVertex()).isPresent()) {
                            if (!(dstVertex == getOrderedChildren(forSyDeSystemGraph, MapV.safeCast(getOrderedChildren(transformedGraph, leftSYMap).get(0).getViewedVertex()).get()).get(0).getViewedVertex())) {
                                transformedGraph.connect(getOrderedChildren(transformedGraph, leftSYMap).get(0), leftSYMap, e.getSourcePort().get(), "mapSplitTransformedOut", EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                            }
                        }
                        if (ReduceV.safeCast(getOrderedChildren(transformedGraph, leftSYMap).get(0).getViewedVertex()).isPresent()) {
                            if (!(dstVertex == getOrderedChildren(forSyDeSystemGraph, ReduceV.safeCast(getOrderedChildren(transformedGraph, leftSYMap).get(0).getViewedVertex()).get()).get(0).getViewedVertex())) {
                                transformedGraph.connect(getOrderedChildren(transformedGraph, leftSYMap).get(0), leftSYMap, e.getSourcePort().get(), "mapSplitTransformedOut", EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                            }
                        }
                        if (SYMap.safeCast(getOrderedChildren(transformedGraph, leftSYMap).get(0).getViewedVertex()).isPresent()) {
                            if (!(dstVertex == getOrderedChildren(forSyDeSystemGraph, SYMap.safeCast(getOrderedChildren(transformedGraph, leftSYMap).get(0).getViewedVertex()).get()).get(0).getViewedVertex())) {
                                transformedGraph.connect(getOrderedChildren(transformedGraph, leftSYMap).get(0), leftSYMap, e.getSourcePort().get(), "mapSplitTransformedOut", EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                            }
                        }
                    }
                    //--------------------------------------------------------
                    rightSYMap.getViewedVertex().addPort("mapSplitTransformedIn");
                    rightSYMap.setInputPorts(List.of("mapSplitTransformedIn"));
                    //Connecting the incoming and incomming edges of the combfunction of the newSYMap which is the executable
                    for (EdgeInfo e : forSyDeSystemGraph.incomingEdgesOf(getOrderedChildren(transformedGraph, rightSYMap).get(0).getViewedVertex())){
                        final Vertex srcVertex = transformedGraph.getEdgeTarget(e);
                        if (e.hasTrait(EdgeTrait.VISUALIZATION_VISUALCONTAINMENT)) continue;
                        if (e.getSourcePort().get().equals("combFunctions")) continue;
                        if (MapV.safeCast(getOrderedChildren(transformedGraph, rightSYMap).get(0).getViewedVertex()).isPresent()) {
                            if (!(srcVertex == getOrderedChildren(forSyDeSystemGraph, MapV.safeCast(getOrderedChildren(transformedGraph, rightSYMap).get(0).getViewedVertex()).get()).get(0).getViewedVertex())) {
                               transformedGraph.connect(rightSYMap, getOrderedChildren(transformedGraph, rightSYMap).get(0), "mapSplitTransformedIn", e.getTargetPort().get(), EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                            }
                        }
                        if (ReduceV.safeCast(getOrderedChildren(transformedGraph, rightSYMap).get(0).getViewedVertex()).isPresent()) {
                            if (!(srcVertex == getOrderedChildren(forSyDeSystemGraph, ReduceV.safeCast(getOrderedChildren(transformedGraph, rightSYMap).get(0).getViewedVertex()).get()).get(0).getViewedVertex())) {
                               transformedGraph.connect(rightSYMap, getOrderedChildren(transformedGraph, rightSYMap).get(0), "mapSplitTransformedIn", e.getTargetPort().get(), EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                            }
                        }
                        if (SYMap.safeCast(getOrderedChildren(transformedGraph, rightSYMap).get(0).getViewedVertex()).isPresent()) {
                            if (!(srcVertex == getOrderedChildren(forSyDeSystemGraph, SYMap.safeCast(getOrderedChildren(transformedGraph, rightSYMap).get(0).getViewedVertex()).get()).get(0).getViewedVertex())) {
                               transformedGraph.connect(rightSYMap, getOrderedChildren(transformedGraph, rightSYMap).get(0), "mapSplitTransformedIn", e.getTargetPort().get(), EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                            }
                        }
                    }
                    newSYSignal.setInputPort(transformedGraph, "mapSplitTransformedOut", leftSYMap);
                    newSYSignal.setOutputPort(transformedGraph, rightSYMap, "mapSplitTransformedIn");
                    transformedGraph.connect(leftSYMap,newSYSignal,"mapSplitTransformedOut", "input", EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                    transformedGraph.connect(newSYSignal,rightSYMap,"output", "mapSplitTransformedIn", EdgeTrait.VISUALIZATION_VISUALCONNECTION);
                }
                //Connecting the incoming edges to the original vertex to the first mapSY inside the Map-Split chain,
                //and the outgoing edges to the original vertex to the last one in the chain
                final SYMap firstSyMap = newSYMaps.get(0);
                final SYMap lastSYMap = newSYMaps.get(newSYMaps.size() - 1);
                for (String s : syMap.getInputPorts()) {
                    firstSyMap.getViewedVertex().addPort(s);
                }
                for (String s : syMap.getOutputPorts()) {
                    lastSYMap.getViewedVertex().addPort(s);
                }
                firstSyMap.setInputPorts(syMap.getInputPorts());
                lastSYMap.setOutputPorts(syMap.getOutputPorts());



                outerloop1:
                for (EdgeInfo e : transformedGraph.incomingEdgesOf(syMap.getViewedVertex())) {
                    if (e.hasTrait(EdgeTrait.MOC_ABSTRACTIONEDGE)) continue;
                    final Vertex srcVertex = transformedGraph.getEdgeSource(e);
                    if (e.getTargetPort().isPresent()) { //&& (!e.hasTrait(EdgeTrait.MOC_SY_SYDATAEDGE) || syMap.getInputPorts().contains(e.getTargetPort().get()))
//                        if (processesAndOrderedExecutables.get(syMap).get(0))
                        firstSyMap.getViewedVertex().addPort(e.getTargetPort().get());
                        if (e.getSourcePort().isPresent()) {
                            for (Executable executable: processesAndOrderedExecutables.get(syMap)){
                                if (srcVertex == executable.getViewedVertex()){
                                    transformedGraph.connect(getOrderedChildren(transformedGraph, lastSYMap).get(0).getViewedVertex(), lastSYMap.getViewedVertex(), e.getSourcePort().get(),e.getTargetPort().get(), EdgeTrait.VISUALIZATION_VISUALCONNECTION);
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
                    if (e.hasTrait(EdgeTrait.VISUALIZATION_VISUALCONTAINMENT)) continue;
                    if (e.hasTrait(EdgeTrait.MOC_ABSTRACTIONEDGE)) continue;
                    final Vertex dstVertex = transformedGraph.getEdgeTarget(e);
                    if (e.getSourcePort().isPresent()) {
                        lastSYMap.getViewedVertex().addPort(e.getSourcePort().get());
                        if (e.getTargetPort().isPresent()) {
                            for (Executable executable: processesAndOrderedExecutables.get(syMap)){
                                if (dstVertex == executable.getViewedVertex()){
                                    transformedGraph.connect(firstSyMap.getViewedVertex(),getOrderedChildren(transformedGraph, firstSyMap).get(0).getViewedVertex(), e.getSourcePort().get(),e.getTargetPort().get(), EdgeTrait.VISUALIZATION_VISUALCONNECTION);
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
//                    for (EdgeInfo e1 : forSyDeSystemGraph.outgoingEdgesOf(exe1.getViewedVertex())) {
//                        final Vertex v1 = forSyDeSystemGraph.getEdgeTarget(e1);
//                        for (EdgeInfo e2 : forSyDeSystemGraph.incomingEdgesOf(exe2.getViewedVertex())) {
//                            final Vertex v2 = forSyDeSystemGraph.getEdgeSource(e2);
//                            if (v1.getIdentifier() == v2.getIdentifier() && DataBlock.conforms(v1)) {
//                                transformedGraph.removeVertex(v1);
//                            }
//                        }
//                    }
//                }
                //Removing the original mapSY as well as hierarchy inside it besides the signals.
                transformedGraph.removeVertex(syMap.getViewedVertex());
                //Writing the transformed graphs
                forSyDeModelHandler.writeModel(transformedGraph, syMap.getIdentifier() + "_transformed.fiodl");
                forSyDeModelHandler.writeModel(transformedGraph, syMap.getIdentifier() + "_transformed.kgt");

            }

        }
        forSyDeModelHandler.writeModel(forSyDeSystemGraph, "visual.kgt");
    }

    //Removing the original mapSY as well as hierarchy inside it besides the signals.
    static protected void deleteHierarchy(ForSyDeSystemGraph m, Vertex v) {
        MapV.safeCast(v).ifPresent(mapV -> {
            for (Executable child : mapV.getKernelsPort(m)) {
                deleteHierarchy(m, child.getViewedVertex());
            }
        });
        ReduceV.safeCast(v).ifPresent(reduceV -> {
            for (Executable child : reduceV.getKernelsPort(m)) {
                deleteHierarchy(m, child.getViewedVertex());
            }
        });
        SYMap.safeCast(v).ifPresent(syMap -> {
            for (Executable child : syMap.getCombFunctionsPort(m)) {
                deleteHierarchy(m, child.getViewedVertex());
            }
        });
        int i=0;
        for (EdgeInfo e : m.incomingEdgesOf(v)){
            if(e.getSourcePort().isPresent()) {
                if (e.getSourcePort().get().equals("kernels") ||e.getSourcePort().get().equals("combFunctions"))
                    i+=1;
            }
        }
        if (i<=1)
            m.removeVertex(v);
    }

    static int countParents(ForSyDeSystemGraph m , Vertex v) {
        int i = 0;
        for (EdgeInfo e : m.incomingEdgesOf(v)){
            if(e.getSourcePort().isPresent()) {
                if (e.getSourcePort().get().equals("kernels") ||e.getSourcePort().get().equals("combFunctions")) {
                    final int numParents = countParents(m, m.getEdgeSource(e));
                }
            }
        }
        return i;
    }

    static List<Executable> getOrderedChildren(ForSyDeSystemGraph m, SYMap v) {
        Set<Vertex> children = new HashSet<>();
        for (Executable e : v.getCombFunctionsPort(m)) {
            children.add(e.getViewedVertex());
        }
        final Graph<Vertex, EdgeInfo> combFunctionsGraph = new AsSubgraph<>(m, children);
        final ShortestPathAlgorithm<Vertex, EdgeInfo> shortestPathAlgorithm = new DijkstraShortestPath<>(combFunctionsGraph);
        List<Executable> childrenSorted = new ArrayList<>(v.getCombFunctionsPort(m));
        childrenSorted.sort((exe, exe2) -> {
            final GraphPath<Vertex, EdgeInfo> path = shortestPathAlgorithm.getPath(exe.getViewedVertex(), exe2.getViewedVertex());
            if (exe == exe2) {
                return 0;
            } else if (path == null) {
                return 1;
            } else {
                return -1;
            }
        });
        return childrenSorted;
    }

    static List<Executable> getOrderedChildren(ForSyDeSystemGraph m, MapV v) {
        Set<Vertex> children = new HashSet<>();
        for (Executable e : v.getKernelsPort(m)) {
            children.add(e.getViewedVertex());
        }
        final Graph<Vertex, EdgeInfo> combFunctionsGraph = new AsSubgraph<>(m, children);
        final ShortestPathAlgorithm<Vertex, EdgeInfo> shortestPathAlgorithm = new DijkstraShortestPath<>(combFunctionsGraph);
        List<Executable> childrenSorted = new ArrayList<>(v.getKernelsPort(m));
        childrenSorted.sort((exe, exe2) -> {
            final GraphPath<Vertex, EdgeInfo> path = shortestPathAlgorithm.getPath(exe.getViewedVertex(), exe2.getViewedVertex());
            if (exe == exe2) {
                return 0;
            } else if (path == null) {
                return 1;
            } else {
                return -1;
            }
        });
        return childrenSorted;
    }

    static List<Executable> getOrderedChildren(ForSyDeSystemGraph m, ReduceV v) {
        Set<Vertex> children = new HashSet<>();
        for (Executable e : v.getKernelsPort(m)) {
            children.add(e.getViewedVertex());
        }
        final Graph<Vertex, EdgeInfo> combFunctionsGraph = new AsSubgraph<>(m, children);
        final ShortestPathAlgorithm<Vertex, EdgeInfo> shortestPathAlgorithm = new DijkstraShortestPath<>(combFunctionsGraph);
        List<Executable> childrenSorted = new ArrayList<>(v.getKernelsPort(m));
        childrenSorted.sort((exe, exe2) -> {
            final GraphPath<Vertex, EdgeInfo> path = shortestPathAlgorithm.getPath(exe.getViewedVertex(), exe2.getViewedVertex());
            if (exe == exe2) {
                return 0;
            } else if (path == null) {
                return 1;
            } else {
                return -1;
            }
        });
        return childrenSorted;
    }
}

