import java.text.SimpleDateFormat

import org.joda.time.DateTime

import scala.io.Source

object TimeReporting extends App {
  val appNumbers = Source.fromFile("/Users/sulabhkothari/Downloads/<APP>.csv").mkString.split("""\n""").tail
  val list = appNumbers.map(time => {
    val time_ = time.split("\"")
    new DateTime(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'").parse(time_(1).substring(0, 19) + "Z"))
  })

  case class AppRun(date: String, runtime: Double)

  val formatter = new SimpleDateFormat("yyyy/MM/dd")
  val appruns = list.groupBy(x => formatter.format(x.toDate)).map {
    case (dt, arr) =>
      //arr.foreach(println)
      if (arr.length > 1)
        AppRun(dt, Math.abs((arr(0).getMillis - arr(1).getMillis) / 60000.0) - 4.5)
      else
        AppRun(dt, 0.0)
    //println("===================================================================")
  }

  implicit val dateTimeOrdering = new Ordering[AppRun] {
    override def compare(x: AppRun, y: AppRun): Int = x.date.compareTo(y.date)
  }

  appruns.toList.sorted(dateTimeOrdering).foreach(println)
}
