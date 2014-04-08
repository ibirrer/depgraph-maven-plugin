/*
 * Copyright (c) 2014 by Stefan Ferstl <st.ferstl@gmail.com>
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

import java.util.Collection;
import java.util.List;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.resolver.filter.ArtifactFilter;
import org.apache.maven.project.MavenProject;

import com.github.ferstl.depgraph.dot.AttributeBuilder;
import com.github.ferstl.depgraph.dot.EdgeRenderer;
import com.github.ferstl.depgraph.dot.DotBuilder;
import com.github.ferstl.depgraph.dot.Node;


class AggregatingGraphFactory implements GraphFactory {

  private final GraphBuilderAdapter graphBuilderAdapter;
  private final ArtifactFilter artifactFilter;
  private final DotBuilder dotBuilder;
  private final boolean includeParentProjects;

  public AggregatingGraphFactory(GraphBuilderAdapter graphBuilderAdapter, ArtifactFilter artifactFilter, DotBuilder dotBuilder, boolean includeParentProjects) {

    this.graphBuilderAdapter = graphBuilderAdapter;
    this.artifactFilter = artifactFilter;
    this.dotBuilder = dotBuilder;
    this.includeParentProjects = includeParentProjects;
  }

  @Override
  public String createGraph(MavenProject parent) {
    @SuppressWarnings("unchecked")
    List<MavenProject> collectedProjects = parent.getCollectedProjects();

    if (this.includeParentProjects) {
      buildModuleTree(parent, this.dotBuilder);
    }

    for (MavenProject collectedProject : collectedProjects) {
      // Process project only if its artifact is not filtered
      if (isPartOfGraph(collectedProject)) {
        this.graphBuilderAdapter.buildDependencyGraph(collectedProject, this.artifactFilter, this.dotBuilder);
      }
    }

    return this.dotBuilder.toString();
  }

  private void buildModuleTree(MavenProject parentProject, DotBuilder dotBuilder) {
    @SuppressWarnings("unchecked")
    Collection<MavenProject> collectedProjects = parentProject.getCollectedProjects();
    for (MavenProject collectedProject : collectedProjects) {
      MavenProject child = collectedProject;
      MavenProject parent = collectedProject.getParent();

      while (parent != null) {
        Node parentNode = filterProject(parent);
        Node childNode = filterProject(child);

        dotBuilder.addEdge(parentNode, childNode, DottedEdgeRenderer.INSTANCE);

        // Stop if we reached the original parent project!
        if (parent.equals(parentProject)) {
          break;
        }

        child = parent;
        parent = parent.getParent();
      }
    }
  }

  private boolean isPartOfGraph(MavenProject project) {
    boolean result = this.artifactFilter.include(project.getArtifact());
    // Project is not filtered and is a parent project
    if (result && project.getModules().size() > 0) {
      result = result && this.includeParentProjects;
    }

    return result;
  }

  private Node filterProject(MavenProject project) {
    Artifact artifact = project.getArtifact();
    if (this.artifactFilter.include(artifact)) {
      return new DependencyNodeAdapter(artifact);
    }

    return null;
  }

  enum DottedEdgeRenderer implements EdgeRenderer {
    INSTANCE {

      @Override
      public String createEdgeAttributes(Node from, Node to) {
        return new AttributeBuilder().style("dotted").toString();
      }

    }
  }
}
