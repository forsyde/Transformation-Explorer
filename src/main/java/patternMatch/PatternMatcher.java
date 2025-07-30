package patternMatch;

import forsyde.io.core.EdgeInfo;
import forsyde.io.core.SystemGraph;
import forsyde.io.core.ModelHandler;
import forsyde.io.core.Vertex;
import forsyde.io.lib.LibForSyDeModelHandler;
import forsyde.io.lib.hierarchy.ForSyDeHierarchy;
import forsyde.io.lib.TraitNamesFrom0_6To0_7;
import forsyde.io.lib.hierarchy.platform.hardware.InstrumentedCommunicationModule;
import forsyde.io.lib.hierarchy.platform.hardware.InstrumentedProcessingModule;
import forsyde.io.visual.kgt.drivers.KGTDriver;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.util.Scanner;

import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;

public class PatternMatcher {

    public static void main(String[] args) throws Exception {
        System.out.println("Matched patterns:");
        Set<SystemGraph> transformedGraphs = new HashSet<>();
        Deque<SystemGraph> transformedGraphsSearchTree = new ArrayDeque<>();


        //ModelHandler modelHandler = LibForSyDeModelHandler.registerLibForSyDe(new ModelHandler());
        ModelHandler modelHandler = new ModelHandler().registerTraitHierarchy(new ForSyDeHierarchy()).registerDriver(new KGTDriver()).registerSystemGraphMigrator(new TraitNamesFrom0_6To0_7());
        SystemGraph SystemGraph = modelHandler.loadModel(Paths.get("imageProcessingSY-gray.fiodl"));
        SystemGraph PlatformModel = modelHandler.loadModel(Paths.get("bus_small_with_hwacc_cloud_detection.fiodl"));
        //SystemGraph SystemGraph = modelHandler.loadModel(Paths.get("A1v0_ForSyDeIO.fiodl"));
        //SystemGraph SystemGraph = modelHandler.loadModel(Paths.get("graySY609_T.fiodl"));
        //SystemGraph SystemGraph = modelHandler.loadModel(Paths.get("maxBrightness.fiodl"));
        //SystemGraph SystemGraph = modelHandler.loadModel(Paths.get("imageProcessing_migrated.fiodl"));
        //SystemGraph SystemGraph = modelHandler.loadModel(Paths.get("imageProcessing_migrated_gray_hierarchy.fiodl"));
        //SystemGraph SystemGraph = ModelHandler.loadModel(Paths.get("maxBrightnessSY_transformed.fiodl"));
        //SystemGraph SystemGraph = ModelHandler.loadModel(Paths.get("maxBrightnessMapVtransformedSY_transformed.fiodl"));
        //SystemGraph SystemGraph = ModelHandler.loadModel(Paths.get("maxBrightnessMapVtransformedSYtransformedSY0_transformed.fiodl"));
        transformedGraphsSearchTree.add(SystemGraph);

        // Map of operation name to cycle count
        Map<String, Double> operationCycles = new HashMap<>();
        int tileCount = 0;
        int minBandwidth = Integer.MAX_VALUE;
        int hopBusTransfer = 0;
        int maxNumHopBus = 0;

        for (Vertex v : PlatformModel.vertexSet()) {
            // Count tiles: vertices with Structure trait and containedModules port
            var maybeStructure = ForSyDeHierarchy.Structure.tryView(PlatformModel, v);
            if (maybeStructure.isPresent() && maybeStructure.get().containedModules() != null) {
                tileCount++;
            }

            // Extract processing module cycle counts
            var maybeProc = ForSyDeHierarchy.InstrumentedProcessingModule.tryView(PlatformModel, v);
            if (maybeProc.isPresent()) {
                InstrumentedProcessingModule proc = maybeProc.get();
                Map<String, Map<String, Double>> modalInstructions = proc.modalInstructionsPerCycle();

                if (modalInstructions.containsKey("generic")) {
                    Map<String, Double> genericOps = modalInstructions.get("generic");

                    for (Map.Entry<String, Double> entry : genericOps.entrySet()) {
                        String operation = entry.getKey();
                        Double cycles = entry.getValue();

                        operationCycles.merge(operation, cycles, (existing, newVal) -> {
                            if (!existing.equals(newVal)) {
                                System.out.println("⚠️ Warning: Operation '" + operation +
                                        "' has different cycle counts: " + existing + " vs " + newVal);
                            }
                            return existing; // keep first
                        });
                    }
                }
            }

            // Extract bus characteristics
            var maybeComm = ForSyDeHierarchy.InstrumentedCommunicationModule.tryView(PlatformModel, v);
            if (maybeComm.isPresent()) {
                InstrumentedCommunicationModule comm = maybeComm.get();

                // Update minBandwidth
                long flitSizeBits = comm.flitSizeInBits();
                int flitSizeBytes = (int) (flitSizeBits / 8);
                minBandwidth = Math.min(minBandwidth, flitSizeBytes);

                // Extract hop/bus delay
                hopBusTransfer = Math.max(hopBusTransfer, comm.maxCyclesPerFlit());

                // Check if this communication module connects to any tile
                boolean connectsToTile = false;

                for (EdgeInfo edge : PlatformModel.incomingEdgesOf(v)) {
                    Vertex source = PlatformModel.getEdgeSource(edge);
                    if (edge.hasTrait(ForSyDeHierarchy.EdgeTraits.PhysicalConnection)) {
                        var maybeTile = ForSyDeHierarchy.Structure.tryView(PlatformModel, source);
                        if (maybeTile.isPresent()) {
                            connectsToTile = true;
                            break;
                        }
                    }
                }
                for (EdgeInfo edge : PlatformModel.outgoingEdgesOf(v)) {
                    Vertex target = PlatformModel.getEdgeTarget(edge);
                    if (edge.hasTrait(ForSyDeHierarchy.EdgeTraits.PhysicalConnection) && connectsToTile == true) {
                        connectsToTile = false;
                        var maybeTile = ForSyDeHierarchy.Structure.tryView(PlatformModel, target);
                        if (maybeTile.isPresent()) {
                            connectsToTile = true;
                            break;
                        }
                    }
                }

                if (connectsToTile) {
                    maxNumHopBus++;
                }
            }
        }

// Output
        System.out.println("Total number of tiles: " + tileCount);
        System.out.println("Operation cycles: " + operationCycles);
        System.out.println("minBandwidth (bytes): " + minBandwidth);
        System.out.println("hop_bus_Transfer (cycles): " + hopBusTransfer);
        System.out.println("max_num_hop_bus: " + maxNumHopBus);


        MapSplitTransformer mapSplitTransformer = new MapSplitTransformer(operationCycles);
        MapsyMapvTransformer mapsyMapvTransformer = new MapsyMapvTransformer(operationCycles);
        MapsyReducevTransformer mapsyReducevTransformer = new MapsyReducevTransformer();
        //ParameterizedMapsyReducevTransformer parameterizedMapsyReducevTransformer = new ParameterizedMapsyReducevTransformer(operationCycles);
        ParameterizedMapSplitTransformer parameterizedMapSplitTransformer = new ParameterizedMapSplitTransformer(operationCycles);
        ParameterizedMapsyMapvTransformer parameterizedMapsyMapvTransformer = new ParameterizedMapsyMapvTransformer(operationCycles);



        //Making the transformation tree with DFS (depth-first search) algorithm
        //Making the transformation tree with Breadth First Search algorithm
        int round = 0;
        Files.createDirectories(Paths.get("out"));
        while (!(transformedGraphsSearchTree.isEmpty())){
            SystemGraph graph = transformedGraphsSearchTree.poll();
            int transformedGraphsSearchTreeSize = transformedGraphsSearchTree.size();
            if (graph instanceof NamedSystemGraph) {
                modelHandler.writeModel(graph, "out/" + ((NamedSystemGraph) graph).name + ".fiodl");
                modelHandler.writeModel(graph, "out/" + ((NamedSystemGraph) graph).name + ".kgt");

                // ------------------------------Running IDeSyDe--------working-----------------------------------------------
//                String runPath = ((NamedSystemGraph) graph).name;
//
//                Process process = new ProcessBuilder("./idesyde","-v","DEBUG","--x-improvement-time-out","60","--run-path","DSE-Results/" +runPath, "out/" + ((NamedSystemGraph) graph).name + ".fiodl", "bus_small_with_hwacc_cloud_detection.fiodl").start();
//                //--x-total-time-out
//                //--x-improvement-time-out
//                InputStream is = process.getInputStream();
//                InputStreamReader isr = new InputStreamReader(is);
//                BufferedReader br = new BufferedReader(isr);
//                String line;
//                while ((line = br.readLine()) != null) {
//                    System.out.println(line);
//                }
//                process.waitFor();
                //--------------------------------------------------------------------------------------------------------
            }
            if (!transformedGraphs.contains(graph)) {
                boolean greedyMatched = false;
                if (!greedyMatched) {
                    for (SystemGraph g: mapSplitTransformer.applyTransform(graph)){
                        if (!transformedGraphs.contains(g) && !greedyMatched) {
                            transformedGraphsSearchTree.add(g);
                            transformedGraphs.add(graph);
                            greedyMatched = true;
                        }
                    }
                }

//                if (!greedyMatched) {
//                    for (SystemGraph g: parameterizedMapSplitTransformer.applyTransform(graph)){
//                        if (!transformedGraphs.contains(g) && !greedyMatched) {
//                            transformedGraphsSearchTree.add(g);
//                            transformedGraphs.add(graph);
//                            greedyMatched = true;
//                        }
//                    }
//                }

                if (!greedyMatched) {
                    for (SystemGraph g : mapsyMapvTransformer.applyTransform(graph)) {
                        if (!transformedGraphs.contains(g) && !greedyMatched) {
                            transformedGraphsSearchTree.add(g);
                            transformedGraphs.add(graph);
                            greedyMatched = true;
                        }
                    }
                }

//                if (!greedyMatched) {
//                    for (SystemGraph g : mapsyReducevTransformer.applyTransform(graph)) {
//                        if (!transformedGraphs.contains(g) && !greedyMatched) {
//                            transformedGraphsSearchTree.add(g);
//                            transformedGraphs.add(graph);
//                            greedyMatched = true;
//                        }
//                    }
//                }

                if (!greedyMatched) {
                    for (SystemGraph g : parameterizedMapsyMapvTransformer.applyTransform(graph)) {
                        if (!transformedGraphs.contains(g) && !greedyMatched) {
                            transformedGraphsSearchTree.add(g);
                            transformedGraphs.add(graph);
                            greedyMatched = true;
                        }
                    }
                }
//                if (transformedGraphsSearchTreeSize == transformedGraphsSearchTree.size()){
//                    // ------------------------------Running IDeSyDe-------------------------------------------------------
//                    String runPath = ((NamedSystemGraph) graph).name;
//
//                    Process process = new ProcessBuilder("./idesyde","-v","DEBUG","--run-path","DSE-Results/" +runPath, "out/" + ((NamedSystemGraph) graph).name + ".fiodl", "bus_small_with_hwacc.fiodl").start();
//
//                    InputStream is = process.getInputStream();
//                    InputStreamReader isr = new InputStreamReader(is);
//                    BufferedReader br = new BufferedReader(isr);
//                    String line;
//                    while ((line = br.readLine()) != null) {
//                        System.out.println(line);
//                    }
//                    process.waitFor();
//                    //--------------------------------------------------------------------------------------------------------
//                }
            }
            System.out.println("Round done with " + transformedGraphs.size() + " transformed graphs");
            round = round +1;
        }

        Files.createDirectories(Paths.get("out"));
        for (SystemGraph g: transformedGraphs){
            if (g instanceof NamedSystemGraph) {
                modelHandler.writeModel(g, "out/" + ((NamedSystemGraph) g).name + ".fiodl");
                modelHandler.writeModel(g, "out/" + ((NamedSystemGraph) g).name + ".kgt");
            }
        }
        //------------------------------------END---------------------------------------------------------------------

        modelHandler.writeModel(SystemGraph, "visual.kgt");
    }


}
