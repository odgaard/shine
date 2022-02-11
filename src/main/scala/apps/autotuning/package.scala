package apps

import exploration.explorationUtil.jsonParser.{readFile}
import play.api.libs.json._

package object autotuning {

  def parseName(filePath: String): String = {
    Json.parse(readFile(filePath)).apply("application_name").toString().replaceAll("\"", "")
  }

}