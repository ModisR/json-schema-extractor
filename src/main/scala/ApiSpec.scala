import play.api.libs.json.{JsObject, Reads, __}

final case class ApiSpec (schemas: Map[String, JsObject])

object ApiSpec {
  implicit val reads: Reads[ApiSpec] =
    (__ \ "components" \ "schemas").read[Map[String, JsObject]] map apply
}
