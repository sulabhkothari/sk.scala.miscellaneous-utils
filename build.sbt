name := "miscellaneous-utils"

version := "0.1"

scalaVersion := "2.11.12"




libraryDependencies ++= Seq(
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.0.0",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.7.2",
  "co.theasi" %% "plotly" % "0.2.0",
  "joda-time" % "joda-time" % "2.10.3",
  "com.pauldijou" %% "jwt-core" % "4.0.0",
  "com.google.guava" % "guava" % "18.0"
)
