package org.bustos.discemone

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import akka.actor.Props
import akka.actor.ActorLogging

import org.slf4j.{Logger, LoggerFactory}

/** Collection of sensors
 *  
 *  Dispatches and manages pod sensors.
 *  Responsible for maintaining communication in case of failure.
 */
object SensorHub { 
  case class MonitoredSensorCount(count: Int)
}

class SensorHub extends Actor with ActorLogging {
  import Sensor._
  import SensorHub._
  import DiscemoneConfig._
  
  var sensors = Map.empty[String, ActorRef]
  val logger = LoggerFactory.getLogger(getClass)
  
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
    case SensorUpdate(messageCount) => {
      logger.info(sender.path.name + " @ " + messageCount)
    }
  }
  
}