import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.1"
  lazy val scodec = "org.scodec" %% "scodec-core" % "1.10.3"
  lazy val scodecStream = "org.scodec" %% "scodec-stream" % "1.0.1"
  lazy val jflac = "org.jflac" % "jflac-codec" % "1.5.2"
}
