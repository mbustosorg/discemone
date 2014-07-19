package org.bustos.discemone

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import akka.actor.Props
import akka.actor.ActorLogging
import akka.util.ByteString
import akka.pattern.ask

import scala.concurrent.Await

import rxtxio.Serial
import rxtxio.Serial._

import org.slf4j.{Logger, LoggerFactory}

/** Collection of sensors
 *  
 *  Dispatches and manages pod sensors.
 *  Responsible for maintaining communication in case of failure.
 */
object SensorHub { 
  case class MonitoredSensorCount(count: Int)
  case class SensorInput(name: String, profile: List[Int])
  case class SensorCommand(name: String, commandString: String)
}

class SensorHub extends Actor with ActorLogging {
  import Sensor._
  import SensorHub._
  import DiscemoneConfig._
  import Discemone._
  import akka.util.Timeout
  import scala.concurrent.duration._
  
  var sensors = Map.empty[String, ActorRef]
  val logger = LoggerFactory.getLogger(getClass)
  implicit val defaultTimeout = Timeout(500)
  
  override def preStart(): Unit = {
    DiscemoneConfig.SensorPorts map (port => {
      val actorName = port.replaceAll("/", "_")
      context.actorOf(Props(new Sensor(port, DiscemoneConfig.SensorBaud)), name = actorName)
    })
  }

  def receive = {
    case SensorStarted(name) => {
    	sensors += (name -> context.sender) 
    	logger.info("Started sensor @ " + name)
    	context.parent ! MonitoredSensorCount (sensors.size)
    }
    case SensorUpdate(profile) => {
    	context.parent ! SensorInput(sender.path.name, profile)
    	if (logger isDebugEnabled) logger.debug(sender.path.name + " @ " + profile.toString)
    }
    case SensorCommand(name, commandString) => {
    	if (name == "")	sensors map {sensor => sensor._2 ! commandString}
    	else {
    	  if (sensors.contains (name)) sensors(name) ! commandString
    	}
    }
    case "SENSOR_LIST_REQUEST" => {
    	val profile = sensors.map (x => {
    		val query = x._2 ? "SENSOR_REQUEST"
    		Await.result (query, 1 second) match {
    		  case SensorDetail(name, threshold, filterLength) => SensorDetail(name, threshold, filterLength)  
    		}
    	  })
    	context.parent ! profile.toList
    }
  }
  
}