import DirectedGraph.Node

import scala.annotation.tailrec

final case class DirectedGraph (nodes: Map[Node, Set[Node]]) {

  lazy val flattened: DirectedGraph = copy(
    rootNodes
      .map(rootNode => rootNode -> getAllDependenciesFor(rootNode))
      .toMap
      .withDefaultValue(Set.empty)
  )

  lazy val rootNodes: Set[Node] = {
    val nonRootNodes = nodes.flatMap(_._2).toSet
    nodes.keySet diff nonRootNodes
  }

  private def getAllDependenciesFor(node: Node) = {
    @tailrec
    def impl(resolvedNodes: Set[Node], unresolvedNodes: Set[Node]): Set[Node] =
      if(unresolvedNodes.isEmpty) resolvedNodes
      else impl(
        resolvedNodes union unresolvedNodes,
        unresolvedNodes flatMap nodes diff resolvedNodes
      )

    impl(Set.empty, nodes(node))
  }
}

object DirectedGraph {
  type Node = String
}
