name := "blocks"

version := "1.0"

organization := "nielinjie"

scalaVersion := "2.9.1"

resolvers += ScalaToolsSnapshots

libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "6.0.3",
	"net.sf.jung" % "jung-visualization" % "2.0.1",
	"net.sf.jung" % "jung-graph-impl" % "2.0.1",
	"org.apache.commons" % "commons-lang3" % "3.1",
    "nielinjie" %% "util.data" % "1.0",
    "nielinjie" %% "util.io" % "1.0",
    "nielinjie" %% "util.ui" % "1.0"
)

libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) =>
        deps :+ ("org.scala-lang" % "scala-swing" % sv)
}

libraryJarPath <<=  baseDirectory.apply(_ / "lib")


