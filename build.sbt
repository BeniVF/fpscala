name := "FPScala"
scalaVersion := "2.11.7"

scalacOptions ++= Seq("-feature")

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.13.0" % "test"
)
