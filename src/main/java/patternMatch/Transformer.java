package patternMatch;

import forsyde.io.core.EdgeInfo;
import forsyde.io.core.SystemGraph;
import forsyde.io.core.Vertex;
import forsyde.io.core.VertexViewer;
import forsyde.io.lib.hierarchy.ForSyDeHierarchy;
import forsyde.io.lib.hierarchy.behavior.BehaviourEntity;
import forsyde.io.lib.hierarchy.behavior.FunctionLikeEntity;
import forsyde.io.lib.hierarchy.behavior.moc.sy.SYMap;
import forsyde.io.lib.hierarchy.behavior.parallel.InterleaveV;
import forsyde.io.lib.hierarchy.behavior.parallel.MapV;
import forsyde.io.lib.hierarchy.behavior.parallel.ReduceV;
import forsyde.io.lib.hierarchy.behavior.parallel.Vectorizable;
import org.jgrapht.Graph;
import org.jgrapht.GraphPath;
import org.jgrapht.alg.interfaces.ShortestPathAlgorithm;
import org.jgrapht.alg.shortestpath.DijkstraShortestPath;
import org.jgrapht.graph.AsSubgraph;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public interface Transformer {

    Set<SystemGraph> applyTransform(SystemGraph systemGraph);

    default void deleteHierarchy(SystemGraph m, Vertex v) {
        ForSyDeHierarchy.MapV.tryView(m, v).ifPresent(mapV -> {
            for (FunctionLikeEntity child : mapV.kernels()) {
                deleteHierarchy(m, child.getViewedVertex());
            }
        });
        ForSyDeHierarchy.ReduceV.tryView(m, v).ifPresent(reduceV -> {
            for (FunctionLikeEntity child : reduceV.kernels()) {
                deleteHierarchy(m, child.getViewedVertex());
            }
        });
        ForSyDeHierarchy.SYMap.tryView(m, v).ifPresent(syMap -> {
            for (FunctionLikeEntity child : syMap.combFunctions()) {
                deleteHierarchy(m, child.getViewedVertex());
            }
        });
        int i=0;
        for (EdgeInfo e : m.incomingEdgesOf(v)){
            if(e.hasTrait(ForSyDeHierarchy.EdgeTraits.BehaviourCompositionEdge)) {
                i+=1;
//                if (e.getSourcePort().get().equals("kernels") ||e.getSourcePort().get().equals("combFunctions"))
            }
        }
        if (i<=1)
            m.removeVertex(v);
    }

    default int countParents(SystemGraph m , Vertex v) {
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

    default List<FunctionLikeEntity> getOrderedChildren(SystemGraph m, SYMap v) {
        Set<Vertex> children = new HashSet<>();
        for (FunctionLikeEntity e : v.combFunctions()) {
            children.add(e.getViewedVertex());
        }
        final Graph<Vertex, EdgeInfo> combFunctionsGraph = new AsSubgraph<>(m, children);
        final ShortestPathAlgorithm<Vertex, EdgeInfo> shortestPathAlgorithm = new DijkstraShortestPath<>(combFunctionsGraph);
        List<FunctionLikeEntity> childrenSorted = new ArrayList<>(v.combFunctions());
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

    default List<FunctionLikeEntity> getOrderedChildren(SystemGraph m, MapV v) {
        Set<Vertex> children = new HashSet<>();
        for (FunctionLikeEntity e : v.kernels()) {
            children.add(e.getViewedVertex());
        }

        for (Vertex v1: m.vertexSet() ){
            ForSyDeHierarchy.Vectorizable.tryView(m, v1).ifPresent(vectorizableViewer -> {
                children.add(v1);
            });
        }
        final Graph<Vertex, EdgeInfo> combFunctionsGraph = new AsSubgraph<>(m, children);
        final ShortestPathAlgorithm<Vertex, EdgeInfo> shortestPathAlgorithm = new DijkstraShortestPath<>(combFunctionsGraph);
        List<FunctionLikeEntity> childrenSorted = new ArrayList<>(v.kernels());
        childrenSorted.sort((src, dst) -> {
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
        return childrenSorted;
    }

    default List<FunctionLikeEntity> getOrderedChildren(SystemGraph m, ReduceV v) {
        Set<Vertex> children = new HashSet<>();
        for (FunctionLikeEntity e : v.kernels()) {
            children.add(e.getViewedVertex());
        }
        final Graph<Vertex, EdgeInfo> combFunctionsGraph = new AsSubgraph<>(m, children);
        final ShortestPathAlgorithm<Vertex, EdgeInfo> shortestPathAlgorithm = new DijkstraShortestPath<>(combFunctionsGraph);
        List<FunctionLikeEntity> childrenSorted = new ArrayList<>(v.kernels());
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
