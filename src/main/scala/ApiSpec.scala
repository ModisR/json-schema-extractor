
import play.api.libs.json._

final case class ApiSpec (schemas: Map[String, JsObject]) {
  import ApiSpec._

  def withSchemasEmbedded: ApiSpec = copy (
    schemaDependencyGraph
      .rootNodes
      .map { node =>
        val schema = schemas(node)
        val dependencyNames = schemaDependencyGraph.flattened.nodes(node)

        val dependencySchemas = dependencyNames
          .map(name => name -> schemas(name) )
          .toMap

        val dependencyJson = SCHEMA_PATH.write[Map[String, JsObject]].writes(dependencySchemas)
        node -> (schema ++ dependencyJson)
      }
      .toMap
  )

  def withAllPropertiesRequired: ApiSpec = copy(
    schemas
      .view
      .mapValues(requireAllPropertiesOf)
      .toMap
  )

  private lazy val schemaDependencyGraph: DirectedGraph = DirectedGraph(
    schemas
      .view
      .mapValues( schema =>
        (schema \\ "$ref")
          .collect { case JsString(refPattern(dep)) => dep }
          .toSet
      )
      .toMap
  )

  private lazy val refPattern = s"#$SCHEMA_PATH/([a-zA-Z0-9_]+)".r
}

object ApiSpec {

  implicit val reads: Reads[ApiSpec] =
    SCHEMA_PATH.read[Map[String, JsObject]] map apply

  private lazy val SCHEMA_PATH = __ \ "components" \ "schemas"

  private def requireAllPropertiesOf(schema: JsObject) =
    schema \ "properties" match {
      case JsDefined(jsObject: JsObject) =>
        val required = Json toJson jsObject.keys
        schema + ("required" -> required)
      case _ => schema
    }
}
