import play.api.libs.json.{JsObject, JsString, Reads, __}

final case class ApiSpec (schemas: Map[String, JsObject])

object ApiSpec {

  implicit val reads: Reads[ApiSpec] =
    (__ \ "components" \ "schemas").read[Map[String, JsObject]] map apply

  def apply(schemas: Map[String, JsObject]): ApiSpec = applyImpl(Map.empty, schemas)

  private def applyImpl(acc: Map[String, JsObject], dec: Map[String, JsObject]) = new ApiSpec(
    if (dec.isEmpty) {
      acc
    } else {
      val dependencyRelations = dec.view.mapValues(
        _ \\ "$ref" collect { case JsString(refPattern(dep)) => dep }
      )

      val dependencies = dependencyRelations.flatMap(_._2).toSet

      val roots = dependencyRelations.keySet diff dependencies

      dec.view.filterKeys(roots.contains).toMap
    }
  )

  private lazy val refPattern = "#/components/schemas/([a-zA-Z0-9_]+)".r
}
