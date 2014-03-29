package org.bustos.discemone

import akka.actor.ActorSystem
import akka.actor.Props

/** Main object for the stand-alone Discemone system
 * 
 */
object DiscemoneMain {

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("discemone")
    //System.out.println(system.settings)
    val a = system.actorOf(Props[Discemone], "Discemone")
  }

}