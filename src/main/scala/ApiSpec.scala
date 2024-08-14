import play.api.libs.json.{JsObject, JsString, Reads, __}

import scala.language.postfixOps

final case class ApiSpec (schemas: Map[String, JsObject])

object ApiSpec {

  implicit val reads: Reads[ApiSpec] =
    schemaPath.read[Map[String, JsObject]] map apply

  def apply(schemas: Map[String, JsObject]): ApiSpec = applyImpl(Map.empty, schemas)

  private def applyImpl(acc: Map[String, JsObject], dec: Map[String, JsObject]) = new ApiSpec(
    if (dec.isEmpty) {
      acc
    } else {
      val schemaDependencyGraph = getSchemaDependencyGraphFrom(dec)

      val (_, roots) = partitionNonRootsOf(schemaDependencyGraph)

      dec.view.filterKeys(roots.contains).toMap
    }
  )

  private def partitionNonRootsOf(acyclicGraph: Map[String, Set[String]]) = {
    val nonRoots = acyclicGraph.flatMap(_._2).toSet

    acyclicGraph.keySet partition nonRoots.contains
  }

  private def getSchemaDependencyGraphFrom(schemas: Map[String, JsObject]) =
    schemas.view.mapValues(
      _ \\ "$ref" collect { case JsString(refPattern(dep)) => dep } toSet
    ).toMap

  private lazy val refPattern = s"#$schemaPath/([a-zA-Z0-9_]+)".r
  private lazy val schemaPath = __ \ "components" \ "schemas"
}
