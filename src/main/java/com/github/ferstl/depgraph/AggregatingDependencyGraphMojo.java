/*
 * Copyright (c) 2014 - 2017 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.github.ferstl.depgraph;

import org.apache.maven.artifact.resolver.filter.ArtifactFilter;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import com.github.ferstl.depgraph.dependency.AggregatingGraphFactory;
import com.github.ferstl.depgraph.dependency.DependencyNode;
import com.github.ferstl.depgraph.dependency.GraphFactory;
import com.github.ferstl.depgraph.dependency.GraphStyleConfigurer;
import com.github.ferstl.depgraph.dependency.MavenGraphAdapter;
import com.github.ferstl.depgraph.dependency.NodeResolution;
import com.github.ferstl.depgraph.graph.GraphBuilder;
import static com.github.ferstl.depgraph.dependency.NodeIdRenderers.VERSIONLESS_ID;
import static com.github.ferstl.depgraph.dependency.NodeIdRenderers.VERSIONLESS_ID_WITH_SCOPE;
import static java.util.EnumSet.allOf;

/**
 * Aggregates all dependencies of a multi-module project into one single graph.
 */
@Mojo(
    name = "aggregate",
    aggregator = true,
    defaultPhase = LifecyclePhase.NONE,
    inheritByDefault = false,
    requiresDependencyCollection = ResolutionScope.TEST,
    requiresDirectInvocation = false,
    threadSafe = true)
public class AggregatingDependencyGraphMojo extends AbstractAggregatingGraphMojo {

  /**
   * If set to {@code true}, the created graph will show the {@code groupId} on all artifacts.
   *
   * @since 1.0.3
   */
  @Parameter(property = "showGroupIds", defaultValue = "false")
  boolean showGroupIds;

  /**
   * If set to {@code true} the artifact nodes will show version information.
   *
   * @since 1.0.0
   */
  @Parameter(property = "showVersions", defaultValue = "false")
  boolean showVersions;

  /**
   * If set to {@code true}, all parent modules (&lt;packaging&gt;pom&lt;/packaging&gt) will be shown with a dotted arrow
   * pointing to their child modules.
   *
   * @since 1.0.0
   */
  @Parameter(property = "includeParentProjects", defaultValue = "false")
  private boolean includeParentProjects;

  @Override
  protected GraphFactory createGraphFactory(ArtifactFilter globalFilter, ArtifactFilter targetFilter, GraphStyleConfigurer graphStyleConfigurer) {
    GraphBuilder<DependencyNode> graphBuilder = graphStyleConfigurer
        .showGroupIds(this.showGroupIds)
        .showArtifactIds(true)
        .showVersionsOnNodes(this.showVersions)
        // This graph won't show any conflicting dependencies. So don't show versions on edges
        .showVersionsOnEdges(false)
        .configure(GraphBuilder.<DependencyNode>create(this.mergeScopes ? VERSIONLESS_ID : VERSIONLESS_ID_WITH_SCOPE));

    MavenGraphAdapter adapter = new MavenGraphAdapter(this.dependencyGraphBuilder, targetFilter);

    if (GraphFormat.forName(this.graphFormat) == GraphFormat.JSON) {
      adapter = new MavenGraphAdapter(this.dependencyTreeBuilder, this.localRepository, targetFilter, allOf(NodeResolution.class));
    }


    return new AggregatingGraphFactory(adapter, globalFilter, graphBuilder, this.includeParentProjects);
  }
}
