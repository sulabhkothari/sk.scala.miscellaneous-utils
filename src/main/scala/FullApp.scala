import java.text.SimpleDateFormat

import org.joda.time.DateTime

import scala.io.Source

object FullApp extends App {
  val appNumbers = Source.fromFile("/Users/sulabhkothari/Downloads/<TOTAL>.csv").mkString.split("""\n""").tail
  val list = appNumbers.filter(x => x.contains("Jul") || x.contains("Aug") || x.contains("Jun") || x.contains("May"))
    .map(str => {
      if(str.contains("time=")) {
        new DateTime(new SimpleDateFormat("MMM dd hh:mm:ss.SSS")
          .parse(str.split("\"")(3))).plusYears(49)
      }
      else {
        val timeS = str.split("\"")(1)
        timeS.substring(0, timeS.length - "<MATCHKEY>".length)
        new DateTime(new SimpleDateFormat("MMM dd, yyyy hh:mm:ss a")
          .parse(timeS.substring(0, timeS.length - "<MATCHKEY>".length)))
      }
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

  val ox = OX.CuttingEdge()
  import OY._

  //ox.|+|
}
trait T

object OX {
  case class CuttingEdge() extends T
}
object OY{
  implicit class A(t:T){
    def |+| = println("A")
  }
  implicit class B(t:T) extends A(t) {
    override def |+| = println("A")
  }

}
