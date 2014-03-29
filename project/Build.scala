import sbt._

object Discemone extends Build {

  lazy val root = Project("root", file("."))
                    .dependsOn(rxtx_akka_io)
                    //.dependsOn(xbee_api)

  lazy val rxtx_akka_io = RootProject(uri("git://github.com/msiegenthaler/rxtx-akka-io.git"))
  //lazy val xbee_api = RootProject(uri("https://code.google.com/p/xbee-api"))

}