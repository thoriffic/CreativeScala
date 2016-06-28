name := "JohannCreativeScalaS2"

version := "1.0"

scalaVersion := "2.11.8"

resolvers += Resolver.bintrayRepo("underscoreio", "training")

initialCommands in console := """
      |import doodle.core._
      |import doodle.core.Image._
      |import doodle.syntax._
      |import doodle.jvm.Java2DCanvas._
      |import doodle.backend.StandardInterpreter._
      |import doodle.examples._
    """.trim.stripMargin

cleanupCommands in console := """
      |doodle.jvm.quit()
   """.trim.stripMargin

libraryDependencies ++= Seq(
  "underscoreio" %% "doodle" % "0.6.5",
  "org.typelevel" %% "cats" % "0.4.1"
)