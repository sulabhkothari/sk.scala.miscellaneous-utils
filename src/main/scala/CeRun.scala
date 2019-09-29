import java.text.SimpleDateFormat
import java.time.format.DateTimeFormatter

import org.joda.time.{DateTime, Period}

import scala.collection.mutable
import scala.io.Source

object CeRun extends App {

  val appNumbers = Source.fromFile("/Users/sulabhkothari/Downloads/<APP>.csv").mkString.split("""\n""").tail
  println(appNumbers(1))
  val jobTimings = mutable.Map[DateTime, Double]()
  for (job <- appNumbers) {
    val jobDetails = job.split("\"")
    if (jobDetails.length == 6) {
      val dtOfStartRunParser = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss,SSS")
      val startTime = new DateTime(dtOfStartRunParser.parse(jobDetails(3)))
      val dtOfEndRunParser = new SimpleDateFormat("MMM dd hh:mm:ss.SSS")
      val endTime = new DateTime(dtOfEndRunParser.parse(jobDetails(5))).plusYears(49)
      println(s"FS: ${jobDetails(3)} - PS: $startTime, FF: ${jobDetails(5)} - PF: $endTime")
      jobTimings += startTime -> ((endTime.getMillis() - startTime.getMillis)/60000.0)
    }
  }

  println(jobTimings.toList.sorted(new Ordering[(DateTime, Double)]{
    override def compare(x: (DateTime, Double), y: (DateTime, Double)): Int = x._1.compareTo(y._1)
  }))
}
