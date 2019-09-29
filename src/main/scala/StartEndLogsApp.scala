import java.text.SimpleDateFormat

import org.joda.time.DateTime

import scala.io.Source

object StartEndLogsApp extends App {
  val appRunNumbers = Source.fromFile("/Users/sulabhkothari/Downloads/<APP>.csv").mkString.split("""\n""").tail
  val list = appRunNumbers.filter(x => x.contains("Jul") || x.contains("Aug") || x.contains("Jun") || x.contains("May"))
    .map(str => {
        val timeS = str.split("\"")(1)
        timeS.substring(0, timeS.length - "<MATCHKEY>".length)
        new DateTime(new SimpleDateFormat("MMM dd, yyyy hh:mm:ss a")
          .parse(timeS.substring(0, timeS.length - "<MATCHKEY>".length)))
    })

  case class AppRun(date: String, runtime: Double)

  val formatter = new SimpleDateFormat("yyyy/MM/dd")
  val appruns = list.groupBy(x => formatter.format(x.toDate)).map {
    case (dt, arr) =>
      //arr.foreach(println)
      val someTime = new DateTime(formatter.parse(dt))
      val afterTime = new DateTime(formatter.parse("2019/07/07"))

      val startTime = if(someTime.isAfter(afterTime)) {
        someTime.plusHours(7)
      }
      else {
        someTime.plusHours(3)
      }

      if (arr.length > 0)
        AppRun(dt, Math.abs(arr(0).getMillis - startTime.getMillis) / 60000.0)
      else
        AppRun(dt, 0.0)
    //println("===================================================================")
  }

  implicit val dateTimeOrdering = new Ordering[AppRun] {
    override def compare(x: AppRun, y: AppRun): Int = x.date.compareTo(y.date)
  }

  appruns.toList.sorted(dateTimeOrdering).foreach(println)
}
