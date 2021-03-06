import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.oletraveler",
      scalaVersion := "2.12.1",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "FlacDecoder",
    libraryDependencies ++= Seq(scalaTest % Test, scodec, scodecStream, jflac)
  )
