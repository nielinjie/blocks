import sbt._
import Keys._

object HelloBuild extends Build {
  val libraryJarPath = SettingKey[File]("lib-jar-path", "path to put lib jars")
  val collectJar = TaskKey[Unit]("collect-jar", "copy jars to lib-jar-path")
  val cjTask = (collectJar <<= (managedClasspath in Compile, libraryJarPath).map {
    (classp, path) =>
      classp.foreach {
        a =>
          val file = a.data
          IO.copyFile(file, path / file.name, true)
      }
  })
  lazy val root = Project(id = "blocks",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(cjTask))
}
