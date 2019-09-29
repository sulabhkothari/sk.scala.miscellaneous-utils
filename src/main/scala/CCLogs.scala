import java.text.SimpleDateFormat

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.google.common.io.BaseEncoding
import org.joda.time.DateTime

import scala.collection.mutable.ListBuffer
import scala.collection.{immutable, mutable}
import scala.io.Source
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

object CCLogs extends App {
  var arr: Array[Array[Int]] = new Array[Array[Int]](10000)
  scala.io.StdIn.readLine()
  for (i <- 1 to 10000) {
    Thread.sleep(100)
    arr(i) = new Array[Int](100000)

  }
  scala.io.StdIn.readLine()


  case class JWTToken(signed_token_a: String = "", ip: String = "", state: String = "", iat: String = "", exp: String = "",
                      aud: String = "", iss: String = "", sub: String = "", xffIp: String = "", name: String = "", nickname: String = "",
                      email_verified: Boolean = false, email: String = "", amr: List[String] = List(), acr: String = "", nonce: String = "")

  case class LoginEvent(token: JWTToken, timestamp: DateTime)

  case class CCLog(timeStamp: DateTime, ip: IpAddr, verb: String, url: String)

  case class IpAddr(addr: String)

  def getToken(line: String): Option[JWTToken] = {
    val keyValPattern: Regex = "\\?token=([^\\s]+).*".r
    val matches = for (patternMatch <- keyValPattern.findAllMatchIn(line)) yield patternMatch.group(1)
    matches.toList match {
      case token :: List() =>
        val idTokenContents = token.split('.')
        Try(new String(BaseEncoding.base64().decode(idTokenContents(1)), "UTF-8")) match {
          case Success(value) => Some(Try(mapper.readValue(value, classOf[JWTToken])) match {
            case Success(value) => value
            case _ => JWTToken()
          })
          case Failure(_) => None
          case _ => None
        }
      case _ => None
    }
  }

  def isScorecardURL(log: CCLog): Boolean =
    log.url.contains("URL1") ||
      log.url.contains("URL2") ||
      log.url.contains("URL3")

  def getBursts(logs: ListBuffer[CCLog]) = {
    logs.foldLeft(mutable.Map[DateTime, Long]()) {
      (map, log) =>
        val dt = new DateTime(new SimpleDateFormat("yyyy-MM-dd hh:mm:ss").parse("2019-07-22 19:02:47"))
        for (s <- 1 to 4) {
          if (dt.getMillis - log.timeStamp.plusSeconds(s).getMillis == 0) {
            //println(log)
          }
          map.update(log.timeStamp.plusSeconds(s), 1.asInstanceOf[Long] + (map.getOrElse(log.timeStamp.plusSeconds(s)
            , 0.asInstanceOf[Long])))
        }
        map
    } //.filter(_._2 > 29)
  }

  val ipTokenMap = mutable.Map[IpAddr, List[LoginEvent]]()
  val akamaiLogs = ListBuffer[CCLog]()

  val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)

  val fileReader = Source.fromFile("/Users/sulabhkothari/Documents/AkamaiLogs/Customer_r")
  val logPattern: Regex = ("([0-9][0-9]/[a-zA-Z][a-zA-Z][a-zA-z]/[0-9][0-9]\\s[0-9][0-9]:[0-9][0-9]:[0-9][0-9])\\s([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+)\\s([a-zA-Z]+)\\s([^\\s]+)").r
  val str = "22/Jul/19 19:57:16 204.237.190.44 OPTIONS /D/4991/214208/000/<ORIGIN_URL>"
  val sdf = new SimpleDateFormat("dd/MMM/yy hh:mm:ss")
  for {
    line <- fileReader.getLines()
    patternMatch <- logPattern.findAllMatchIn(line)
  } {
    val dt = new DateTime(sdf.parse(patternMatch.group(1)))
    val ip = IpAddr(patternMatch.group(2).toString)
    val verb = patternMatch.group(3).toString
    val url = patternMatch.group(4).toString

    val log = CCLog(dt, ip, verb, url)
    akamaiLogs += log

    getToken(line) match {
      case Some(jwtToken) =>
        ipTokenMap.update(ip, LoginEvent(jwtToken, dt) :: ipTokenMap.getOrElse(ip, List()))
      case _ =>
    }
  }

  println(ipTokenMap.getOrElse(IpAddr("<IPAddr>"), "Not found"))
  val appLogs = akamaiLogs.filter(isScorecardURL).groupBy(_.ip).toList

  appLogs.foreach(ipWithLogs =>
    getBursts(ipWithLogs._2).filter(_._2 > 29)
      .foreach(x => println(s"${ipWithLogs._1.addr}: 5 Seconds ending ${x._1} incurred call volume: ${x._2}")))


}
