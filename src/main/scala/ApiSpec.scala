import play.api.libs.json.{JsObject, JsString, Reads, __}

final case class ApiSpec (schemas: Map[String, JsObject]) {
  lazy val roots: Set[String] = {
    val dependencies = for {
      (_, schema)               <- schemas
      JsString(refPattern(dep)) <- schema \\ "$ref"
    } yield dep

    schemas.keySet diff dependencies.toSet
  }

  private lazy val refPattern = "#/components/schemas/([a-zA-Z0-9_]+)".r
}

object ApiSpec {
  implicit val reads: Reads[ApiSpec] =
    (__ \ "components" \ "schemas").read[Map[String, JsObject]] map apply
}
