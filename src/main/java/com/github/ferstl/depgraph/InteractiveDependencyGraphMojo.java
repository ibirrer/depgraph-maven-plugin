/*
 * Copyright (c) 2014 - 2016 by Stefan Ferstl <st.ferstl@gmail.com>
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

import java.io.IOException;
import java.io.Writer;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.EnumSet;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.resolver.filter.ArtifactFilter;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Component;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.apache.maven.shared.dependency.tree.DependencyTreeBuilder;
import org.codehaus.plexus.util.FileUtils;
import com.github.ferstl.depgraph.graph.DependencyGraphException;
import com.github.ferstl.depgraph.graph.GraphBuilderAdapter;
import com.github.ferstl.depgraph.graph.GraphFactory;
import com.github.ferstl.depgraph.graph.GraphNode;
import com.github.ferstl.depgraph.graph.NodeResolution;
import com.github.ferstl.depgraph.graph.SimpleGraphFactory;
import com.github.ferstl.depgraph.graph.style.StyleConfiguration;
import com.github.ferstl.depgraph.interactive.JsonBuilder;
import static java.util.EnumSet.allOf;

/**
 * Creates an interactive dependency graph of a maven module.
 */
@Mojo(
    name = "interactive",
    aggregator = false,
    defaultPhase = LifecyclePhase.NONE,
    requiresDependencyCollection = ResolutionScope.TEST,
    requiresDirectInvocation = false,
    threadSafe = true)
public class InteractiveDependencyGraphMojo extends AbstractMojo {

  @Parameter(defaultValue = "${project}", readonly = true)
  private MavenProject project;

  @Parameter(defaultValue = "${localRepository}", readonly = true)
  private ArtifactRepository localRepository;

  @Component
  private DependencyTreeBuilder dependencyTreeBuilder;

  @Override
  public final void execute() throws MojoExecutionException, MojoFailureException {
    ArtifactFilter globalFilter = DoNothingArtifactFilter.INSTANCE;
    ArtifactFilter targetFilter = DoNothingArtifactFilter.INSTANCE;
    StyleConfiguration styleConfiguration = null;

    try {
      GraphFactory graphFactory = createGraphFactory(globalFilter, targetFilter, styleConfiguration);
      writeFiles(graphFactory.createGraph(this.project));

    } catch (DependencyGraphException e) {
      throw new MojoExecutionException("Unable to create dependency graph.", e.getCause());
    } catch (IOException e) {
      throw new MojoExecutionException("Unable to write graph file.", e);
    } catch (URISyntaxException e) {
      throw new MojoExecutionException("Unable to copy graph resources.", e);
    }
  }

  protected GraphFactory createGraphFactory(ArtifactFilter globalFilter, ArtifactFilter targetFilter, StyleConfiguration styleConfiguration) {
    GraphBuilder<GraphNode> graphBuilder = new JsonBuilder();
    GraphBuilderAdapter adapter = createGraphBuilderAdapter(targetFilter);
    return new SimpleGraphFactory(adapter, globalFilter, graphBuilder);
  }

  private void writeFiles(String graphJsonp) throws IOException, URISyntaxException {
    Path reportDirectory = this.project.getBasedir().toPath()
        .resolve(this.project.getBuild().getDirectory())
        .resolve("depgraph-interactive");

    Files.createDirectories(reportDirectory);
    Path graphJsonpFile = reportDirectory.resolve("graph.jsonp");

    try (Writer writer = Files.newBufferedWriter(graphJsonpFile, StandardCharsets.UTF_8)) {
      writer.write(graphJsonp);
    }

    FileUtils.copyURLToFile(InteractiveDependencyGraphMojo.class.getResource("/interactive/index.html"), reportDirectory.resolve("index.html").toFile());
    FileUtils.copyURLToFile(InteractiveDependencyGraphMojo.class.getResource("/interactive/main.js"), reportDirectory.resolve("main.js").toFile());
  }

  private GraphBuilderAdapter createGraphBuilderAdapter(ArtifactFilter targetFilter) {
    GraphBuilderAdapter adapter;
    EnumSet<NodeResolution> resolutions = allOf(NodeResolution.class);
    adapter = new GraphBuilderAdapter(this.dependencyTreeBuilder, this.localRepository, targetFilter, resolutions);
    return adapter;
  }

  private enum DoNothingArtifactFilter implements ArtifactFilter {
    INSTANCE;

    @Override
    public boolean include(Artifact artifact) {
      return true;
    }

  }
}
