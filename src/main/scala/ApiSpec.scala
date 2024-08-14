import ApiSpec.requireAllPropertiesOf
import play.api.libs.json._

import scala.annotation.tailrec

final case class ApiSpec (schemas: Map[String, JsObject]) {

  def withAllPropertiesRequired: ApiSpec = copy(
    schemas
      .view
      .mapValues(requireAllPropertiesOf)
      .toMap
  )
}

object ApiSpec {

  implicit val reads: Reads[ApiSpec] =
    schemaPath.read[Map[String, JsObject]] map apply

  def apply(schemas: Map[String, JsObject]): ApiSpec = new ApiSpec(
    {
      val dependencyGraph = getDependencyGraphFrom(schemas)
      val rootNodes = getRootNodesOf(dependencyGraph)

      def flatten(directedGraph: DirectedGraph) =
        rootNodes
          .map { rootNode =>
            rootNode -> resolveDependenciesFor(rootNode, directedGraph)
          }
          .toMap
          .withDefaultValue(Set.empty)

      val flattenedGraph = flatten(dependencyGraph)

      rootNodes
        .map { node =>
          val schema = schemas(node)
          val dependencyNames = flattenedGraph(node)

          val dependencySchemas = dependencyNames
            .map(name => name -> schemas(name) )
            .toMap

          val dependencyJson = schemaPath.write[Map[String, JsObject]].writes(dependencySchemas)
          node -> (schema ++ dependencyJson)
        }
        .toMap
    }
  )

  private def resolveDependenciesFor(startNode: Node, directedGraph: DirectedGraph) = {
    @tailrec
    def impl(resolvedNodes: Set[Node], unresolvedNodes: Set[Node]): Set[Node] =
      if(unresolvedNodes.isEmpty) resolvedNodes
      else impl(
        resolvedNodes union unresolvedNodes,
        unresolvedNodes flatMap directedGraph diff resolvedNodes
      )

    impl(Set.empty, directedGraph(startNode))
  }

  private def getRootNodesOf(directedGraph: DirectedGraph) = {
    val nonRoots = directedGraph.flatMap(_._2).toSet

    directedGraph.keySet diff nonRoots
  }

  private def getDependencyGraphFrom(schemas: Map[String, JsObject]) =
    schemas.view
      .mapValues( schema =>
        (schema \\ "$ref")
          .collect { case JsString(refPattern(dep)) => dep }
          .toSet
      )
      .toMap

  private def requireAllPropertiesOf(schema: JsObject) =
    schema \ "properties" match {
      case JsDefined(jsObject: JsObject) =>
        val required = Json toJson jsObject.keys
        schema + ("required" -> required)
      case _ => schema
    }

  private lazy val refPattern = s"#$schemaPath/([a-zA-Z0-9_]+)".r
  private lazy val schemaPath = __ \ "components" \ "schemas"

  private type DirectedGraph = Map[Node, Set[Node]]
  private type Node = String
}
