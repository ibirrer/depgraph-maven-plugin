package com.github.ferstl.depgraph.interactive;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import org.apache.maven.artifact.Artifact;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.github.ferstl.depgraph.GraphBuilder;
import com.github.ferstl.depgraph.dot.EdgeAttributeRenderer;
import com.github.ferstl.depgraph.graph.GraphNode;
import com.github.ferstl.depgraph.graph.NodeResolution;


public class JsonBuilder implements GraphBuilder<GraphNode> {


  static class GraphJson {

    private List<ArtifactJson> artifacts = new ArrayList<>();
    private List<DependencyJson> dependencies = new ArrayList<>();

    public List<ArtifactJson> getArtifacts() {
      return this.artifacts;
    }

    public void setArtifacts(List<ArtifactJson> artifacts) {
      this.artifacts = artifacts;
    }


    public List<DependencyJson> getDependencies() {
      return this.dependencies;
    }


    public void setDependencies(List<DependencyJson> dependencies) {
      this.dependencies = dependencies;
    }


  }

  static class DependencyJson {

    private int from;
    private int to;

    private NodeResolution resolution;
    private Set<String> scopes;

    public int getFrom() {
      return this.from;
    }

    public void setFrom(int from) {
      this.from = from;
    }

    public int getTo() {
      return this.to;
    }

    public void setTo(int to) {
      this.to = to;
    }

    public NodeResolution getResolution() {
      return this.resolution;
    }

    public void setResolution(NodeResolution resolution) {
      this.resolution = resolution;
    }

    public Set<String> getScopes() {
      return this.scopes;
    }

    public void setScopes(Set<String> scopes) {
      this.scopes = scopes;
    }


  }

  static class ArtifactJson {

    private int id;
    private String artifactId;
    private String version;

    public int getId() {
      return this.id;
    }

    public void setId(int id) {
      this.id = id;
    }

    public String getArtifactId() {
      return this.artifactId;
    }

    public void setArtifactId(String artifactId) {
      this.artifactId = artifactId;
    }

    public String getVersion() {
      return this.version;
    }

    public void setVersion(String version) {
      this.version = version;
    }


  }


  private class Dependency {

    private final GraphNode from;
    private final GraphNode to;

    public Dependency(GraphNode from, GraphNode to) {
      this.from = from;
      this.to = to;
    }


  }


  private String graphName;
  private boolean omitSelfReferences;
  private final LinkedHashMap<Artifact, GraphNode> artifacts = new LinkedHashMap<>();
  private final LinkedHashSet<Dependency> dependencies = new LinkedHashSet<>();


  @Override
  public JsonBuilder graphName(String graphName) {
    this.graphName = graphName;
    return this;
  }

  @Override
  public GraphBuilder<GraphNode> addEdge(GraphNode from, GraphNode to, EdgeAttributeRenderer<? super GraphNode> edgeAttributeRenderer) {
    throw new UnsupportedOperationException("addEdge");
  }


  @Override
  public GraphBuilder<GraphNode> addEdge(GraphNode from, GraphNode to) {
    if (from != null && to != null) {
      this.artifacts.putIfAbsent(from.getArtifact(), from);
      this.artifacts.putIfAbsent(to.getArtifact(), to);
      this.dependencies.add(new Dependency(getEffectiveNode(from), getEffectiveNode(to)));
    }

    return this;
  }

  @Override
  public GraphNode getEffectiveNode(GraphNode node) {
    Artifact artifact = node.getArtifact();

    if (this.artifacts.containsKey(artifact)) {
      return this.artifacts.get(artifact);
    }

    return node;
  }

  @Override
  public String toString() {

    GraphJson graphJson = new GraphJson();

    for (Entry<Artifact, GraphNode> entry : this.artifacts.entrySet()) {
      Artifact artifact = entry.getKey();
      GraphNode node = entry.getValue();

      ArtifactJson artifactJson = new ArtifactJson();
      artifactJson.setId(node.getId());
      artifactJson.setArtifactId(artifact.getArtifactId());
      artifactJson.setVersion(artifact.getVersion());

      graphJson.getArtifacts().add(artifactJson);
    }

    for (Dependency dependency : this.dependencies) {
      DependencyJson dependencyJson = new DependencyJson();
      dependencyJson.from = dependency.from.getId();
      dependencyJson.to = dependency.to.getId();
      dependencyJson.resolution = dependency.to.getResolution();
      dependencyJson.scopes = dependency.to.getScopes();

      graphJson.getDependencies().add(dependencyJson);
    }


    ObjectMapper mapper = new ObjectMapper();
    mapper.enable(SerializationFeature.INDENT_OUTPUT);

    try {
      return "var graph = " + mapper.writeValueAsString(graphJson) + ";";
    } catch (JsonProcessingException e) {
      throw new RuntimeException(e);
    }
  }
}
