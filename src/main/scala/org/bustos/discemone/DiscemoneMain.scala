package org.bustos.discemone

import akka.actor.ActorSystem
import akka.actor.Props

/** Main object for the stand-alone Discemone system
 * 
 */

object DiscemoneMain {

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("discemone")
    val a = system.actorOf(Props[Discemone], "Discemone")
  }

}