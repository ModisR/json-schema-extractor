import play.api.libs.json.Json

import java.io.PrintWriter

object Main extends App {
  private val stream = getClass getResourceAsStream args.head
  private val json = Json parse stream
  stream.close()

  for {
    (name, schema) <- json.as[ApiSpec].schemas
  } {
    val pw = new PrintWriter(s"$name.schema.json")
    pw.write(Json prettyPrint schema)
    pw.close()
  }
}
