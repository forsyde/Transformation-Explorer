package patternMatch;

import forsyde.io.core.SystemGraph;
import forsyde.io.core.ModelHandler;
import forsyde.io.lib.hierarchy.ForSyDeHierarchy;
import forsyde.io.lib.TraitNamesFrom0_6To0_7;
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
        MapSplitTransformer mapSplitTransformer = new MapSplitTransformer();
        MapsyMapvTransformer mapsyMapvTransformer = new MapsyMapvTransformer();
        MapsyReducevTransformer mapsyReducevTransformer = new MapsyReducevTransformer();
        ParameterizedMapsyMapvTransformer parameterizedMapsyMapvTransformer = new ParameterizedMapsyMapvTransformer();

        ModelHandler modelHandler = new ModelHandler().registerTraitHierarchy(new ForSyDeHierarchy()).registerDriver(new KGTDriver()).registerSystemGraphMigrator(new TraitNamesFrom0_6To0_7());
        SystemGraph SystemGraph = modelHandler.loadModel(Paths.get("imageProcessingSY-gray.fiodl"));
        //SystemGraph SystemGraph = modelHandler.loadModel(Paths.get("imageProcessing_migrated.fiodl"));
        //SystemGraph SystemGraph = modelHandler.loadModel(Paths.get("imageProcessing_migrated_gray_hierarchy.fiodl"));
        transformedGraphsSearchTree.add(SystemGraph);
        //SystemGraph SystemGraph = ModelHandler.loadModel(Paths.get("maxBrightnessSY_transformed.fiodl"));
        //SystemGraph SystemGraph = ModelHandler.loadModel(Paths.get("maxBrightnessMapVtransformedSY_transformed.fiodl"));
        //SystemGraph SystemGraph = ModelHandler.loadModel(Paths.get("maxBrightnessMapVtransformedSYtransformedSY0_transformed.fiodl"));

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

                // ------------------------------Running IDeSyDe-------------------------------------------------------
                String runPath = ((NamedSystemGraph) graph).name;

                Process process = new ProcessBuilder("./idesyde","-v","DEBUG","--x-total-time-out","60","--run-path","DSE-Results/" +runPath, "out/" + ((NamedSystemGraph) graph).name + ".fiodl", "bus_small_with_hwacc.fiodl").start();
                //--x-total-time-out
                //--x-improvement-time-out
                InputStream is = process.getInputStream();
                InputStreamReader isr = new InputStreamReader(is);
                BufferedReader br = new BufferedReader(isr);
                String line;
                while ((line = br.readLine()) != null) {
                    System.out.println(line);
                }
                process.waitFor();
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

                if (!greedyMatched) {
                    for (SystemGraph g : mapsyMapvTransformer.applyTransform(graph)) {
                        if (!transformedGraphs.contains(g) && !greedyMatched) {
                            transformedGraphsSearchTree.add(g);
                            transformedGraphs.add(graph);
                            greedyMatched = true;
                        }
                    }
                }

                if (!greedyMatched) {
                    for (SystemGraph g : mapsyReducevTransformer.applyTransform(graph)) {
                        if (!transformedGraphs.contains(g) && !greedyMatched) {
                            transformedGraphsSearchTree.add(g);
                            transformedGraphs.add(graph);
                            greedyMatched = true;
                        }
                    }
                }

//                if (!greedyMatched) {
//                    for (SystemGraph g : parameterizedMapsyMapvTransformer.applyTransform(graph)) {
//                        if (!transformedGraphs.contains(g) && !greedyMatched) {
//                            transformedGraphsSearchTree.add(g);
//                            transformedGraphs.add(graph);
//                            greedyMatched = true;
//                        }
//                    }
//                }
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

//        Files.createDirectories(Paths.get("out"));
//        for (SystemGraph g: transformedGraphs){
//            if (g instanceof NamedSystemGraph) {
//                modelHandler.writeModel(g, "out/" + ((NamedSystemGraph) g).name + ".fiodl");
//                modelHandler.writeModel(g, "out/" + ((NamedSystemGraph) g).name + ".kgt");
//            }
//        }
        //------------------------------------END---------------------------------------------------------------------

        modelHandler.writeModel(SystemGraph, "visual.kgt");
    }


}
