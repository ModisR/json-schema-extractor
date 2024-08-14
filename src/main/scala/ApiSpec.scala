
import play.api.libs.json._

final case class ApiSpec (schemas: Map[String, JsObject]) {
  import ApiSpec._

  def withSchemasEmbedded: ApiSpec = copy (
    schemaDependencyGraph
      .flattened
      .nodes
      .map { case (rootNode, dependencyNodes) =>
        val rootSchema = schemas(rootNode)

        val dependencySchemas = dependencyNodes
          .map(node => node -> schemas(node) )
          .toMap

        val dependencyJson = SCHEMA_PATH.write[Map[String, JsObject]].writes(dependencySchemas)
        rootNode -> (rootSchema ++ dependencyJson)
      }
  )

  def withAllPropertiesRequired: ApiSpec = copy(
    schemas
      .view
      .mapValues(makeAllPropertiesRequired)
      .toMap
  )

  private lazy val schemaDependencyGraph: DirectedGraph = DirectedGraph(
    schemas
      .view
      .mapValues(getAllRefs)
      .toMap
  )
}

object ApiSpec {

  implicit val reads: Reads[ApiSpec] =
    SCHEMA_PATH.read[Map[String, JsObject]] map apply

  private lazy val REF_PATTERN = s"#$SCHEMA_PATH/([a-zA-Z0-9_]+)".r
  private lazy val SCHEMA_PATH = __ \ "components" \ "schemas"

  private def makeAllPropertiesRequired(schema: JsObject) =
    schema \ "properties" match {
      case JsDefined(jsObject: JsObject) =>
        val required = Json toJson jsObject.keys
        schema + ("required" -> required)
      case _ => schema
    }

  private def getAllRefs(schema: JsObject) =
    (schema \\ "$ref")
      .collect { case JsString(REF_PATTERN(dep)) => dep }
      .toSet
}
