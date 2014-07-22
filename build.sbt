import AssemblyKeys._

name := "discemone"

version := "1.0"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
    "log4j" % "log4j" % "1.2.14",
    "com.netflix.rxjava" % "rxjava-scala" % "0.15.0",
    "org.rxtx" % "rxtx" % "2.1.7",
    "org.slf4j" % "slf4j-api" % "1.7.6",
    "org.slf4j" % "slf4j-simple" % "1.7.6",
    "com.typesafe.akka" %% "akka-actor" % "2.2.3",
    "org.scalatest" % "scalatest_2.10" % "1.9",
    "com.typesafe.akka" %% "akka-testkit" % "2.2.3",
    "junit" % "junit" % "4.10" % "test",
    "ch.inventsoft.akka" %% "rxtx-akka-io" % "1.0.2"
)

assemblySettings

mainClass in assembly := Some("org.bustos.discemone.DiscemoneMain")

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case PathList(ps @ _*) if ps.last startsWith "log4j.properties" => {
	 MergeStrategy.concat
    }
    case x => old(x)
  }
}