import java.io.StringWriter
import java.security.KeyPair

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule

import scala.io.Source
case class Person( name: String, age: Int )

object ProcessJson{
  def main(args: Array[String]): Unit = {


    val person = Person("fred", 25)
    val mapper = new ObjectMapper()
    mapper.registerModule(DefaultScalaModule)

    val out = new StringWriter
    mapper.writeValue(out, person)
    val json = out.toString()
    println(json)

    val person2 = mapper.readValue(json, classOf[Person])
    println(person2)
    //mapper. Source.fromFile("")

   val jsonm = Source.fromFile("<FILEURL-PART1>" +
      "<FILEURL-PART2>.json").mkString
    println(jsonm)
    val map = mapper.readValue(jsonm, classOf[Map[String,Any]])
    println(s"${map.get("name")},${map.get("override_attributes").asInstanceOf[Some[Map[String,Any]]]}")
  }
}