package org.bustos.discemone

import akka.actor.{ Actor, ActorRef, Props, ActorSystem }
import akka.actor.ActorLogging
import scala.concurrent.duration._

import scala.sys.process._

import org.slf4j.{Logger, LoggerFactory}

/** Communication object for monitoring process statistics
 * 
 * Collects processor statistics (e.g. CPU, free memory etc) for monitoring
 */
object ProcessStatistics { 
  case class Tick
  case class CPUtimeSeries(history: List[Double])
}
/** Communication object for monitoring process statistics
 *  
 *  @constructor create a new process stats object
 */
class ProcessStatistics extends Actor with ActorLogging {
  import context._
  import ProcessStatistics._

  val logger = LoggerFactory.getLogger(getClass)  
  var cpuHistory: List[Double] = List(0.0)
  val tickInterval = 5 seconds
  val hoursToTrack = 5 hours
  val tickScheduler = system.scheduler.schedule (0 milliseconds, tickInterval, self, Tick)
 
  def receive = {  
    case CPUtimeSeries => {
    	sender ! CPUtimeSeries(cpuHistory)
    }	
    case Tick => {
    	val cpuCount = Process("bash" :: "-c" :: "ps aux | awk '{sum += $3} END {print sum}'" :: Nil).!!
    	val cpuCountDouble: Double = cpuCount.toDouble
    	val takeCount: Int = (hoursToTrack / tickInterval).toInt
    	cpuHistory = (cpuCountDouble :: cpuHistory).take (takeCount)
    	//logger.info("CPU: " + cpuCountDouble)
    }
  }
}