import java.text.SimpleDateFormat
import java.util.Date

import JobsTiming.jobsWithStartAndFinishTime
import co.theasi.plotly.Plot
import org.joda.time.DateTime

import scala.collection.mutable
import scala.io.Source

object JobsTiming extends App {

  def getTimeAndType(msg: String): (String, Int) = {
    val msgArr = msg.split(" ")
    val timestamp = if (msg.contains("started at") || msg.contains("finished at")) msgArr.last
    else msgArr(msgArr.length - 2) + " " + msgArr.last
    val typeOfTime = if (msg.contains("started at")) 0 else if (msg.contains("finished at")) 1 else 2
    (timestamp, typeOfTime)
  }

  def getDateTime(dt: String) = {
    new DateTime(scala.util.Try(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'").parse(dt)) match {
      case scala.util.Failure(_) => new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'").parse(dt)
      case scala.util.Success(x) => x
    })
  }

  case class GroupHistory(stageName: String, date: Date, startDateTime: DateTime, endDateTime: DateTime, timeTakenInMinutes: Double)

  object GroupHistory {

    def belongsToGroup1(job: JobDetails) =
      job.appName == "<JOB>" ||
        job.appName == "<JOB>" ||
        job.appName == "<JOB>"

    def belongsToGroup2(job: JobDetails) =
      job.appName == "<JOB>" ||
        job.appName == "<JOB>" ||
        job.appName == "<JOB>" ||
        job.appName == "<JOB>" ||
        job.appName == "<JOB>" ||
        job.appName == "<JOB>" ||
        job.appName == "<JOB>" ||
        job.appName == "<JOB>" ||
        job.appName == "<JOB>" ||
        job.appName == "<JOB>" ||
        job.appName == "<JOB>"


    def getGroupHistory(stageName: String, jobsWithStartAndFinishTime: Seq[JobDetails], jobsToFilter:(JobDetails => Boolean))(implicit order: Ordering[DateTime]) =
    jobsWithStartAndFinishTime.filter(jobsToFilter)
      .groupBy(x => formatter.format(x.startDateTime.toDate))
      .map {
        case (dt, jobs) =>
          val startDateTime = jobs.minBy(_.startDateTime)(order).startDateTime
          val endDateTime = jobs.maxBy(_.endDateTime)(order).endDateTime

          GroupHistory(stageName, formatter.parse(dt), startDateTime, endDateTime, (endDateTime.getMillis - startDateTime.getMillis)/60000.0)
      }
  }

  case class JobDetails(correlationId: String = "", timestamp: DateTime = DateTime.now(),
                        appName: String = "", timeTaken: String = "",
                        startDateTime: DateTime = DateTime.now(), endDateTime: DateTime = DateTime.now(), message: String = "")

  case class JobRun(correlationId: String, appName: String, timestamp: DateTime, dt: String, typeOfTime: Int, message: String)


  val appNumbers = Source.fromFile("/Users/sulabhkothari/Downloads/<JOB>-98Days.csv").mkString.split("""\n""").tail
  println(appNumbers(0))
  val listOfJobs = appNumbers.map(job => {
    val jobDetails = job.split("\"")
    val correlationId = jobDetails(1)
    val timestamp = new DateTime(new SimpleDateFormat("yyyy-MM-dd hh:mm:ss,SSS").parse(jobDetails(3)))
    // started at 2019-08-06T10:03:11.723Z

    val jDet = if (jobDetails(4) != ",") {
      val jName = jobDetails(4).split(",")(1)
      //println(s"CorrelationId: $correlationId, JobName: $jName, message: ${jobDetails(5)}")
      val (dt, typeOf) = getTimeAndType(jobDetails(5))
      JobRun(correlationId, jName, timestamp, dt, typeOf, jobDetails(5))
    }
    else {
      val jName = jobDetails(5)
      //println(s"CorrelationId: $correlationId, JobName: $jName, message: ${jobDetails(7)}")
      val jt = jobDetails(7).split(" ")
      val (dt, typeOf) = getTimeAndType(jobDetails(7))
      JobRun(correlationId, jName, timestamp, dt, typeOf, jobDetails(7))
    }
    jDet
  })

  //listOfJobs.foreach(println)

  val jobsWithStartAndFinishTime = listOfJobs.groupBy(_.correlationId).map {
    case (c, arr) => arr.foldRight(JobDetails(correlationId = c))((job, jobD) =>
      JobDetails(jobD.correlationId, job.timestamp, job.appName,
        if (job.typeOfTime == 2) job.dt else jobD.timeTaken,
        if (job.typeOfTime == 0) getDateTime(job.dt) else jobD.startDateTime,
        if (job.typeOfTime == 1) getDateTime(job.dt) else jobD.endDateTime,
        if (job.typeOfTime == 2) job.message else jobD.message
      ))
  }.toSeq

  //jobsWithStartAndFinishTime.foreach(println)

  implicit val dateTimeOrdering = new Ordering[DateTime] {
    override def compare(x: DateTime, y: DateTime): Int = x.compareTo(y)
  }

  val stagesOrdering = new Ordering[GroupHistory] {
    override def compare(x: GroupHistory, y: GroupHistory): Int = x.date.compareTo(y.date)
  }

  import java.text.DateFormat
  import java.text.SimpleDateFormat

  val formatter = new SimpleDateFormat("yyyy/MM/dd")
  //val loadHistoryRuns = StageHistory.getGroupHistory("LoadHistory", jobsWithStartAndFinishTime, StageHistory.isLoadHistoryJob)

  val appStageRuns = GroupHistory.getGroupHistory("<APP>STAGE", jobsWithStartAndFinishTime, GroupHistory.belongsToGroup2)

  //loadHistoryRuns.toList.sorted(stagesOrdering).foreach(println)
  appStageRuns.toList.sorted(stagesOrdering).foreach(println)

}
