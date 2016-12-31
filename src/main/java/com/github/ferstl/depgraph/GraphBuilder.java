package com.github.ferstl.depgraph;

import com.github.ferstl.depgraph.dot.EdgeAttributeRenderer;

public interface GraphBuilder<T> {

  GraphBuilder<T> graphName(String graphName);

  GraphBuilder<T> addEdge(T from, T to, EdgeAttributeRenderer<? super T> edgeAttributeRenderer);

  GraphBuilder<T> addEdge(T from, T to);

  T getEffectiveNode(T node);
}
