package org.bustos.discemone

import akka.actor.{ActorSystem, ActorRef, Actor, Props}
import akka.actor.ActorLogging
import org.slf4j.{Logger, LoggerFactory}

/** Controller for Discemone
 * 
 * The pod sensors and XBee access are managed by this object.
 * This also provides system summary access for rendering
 * by external viewers.  
 */
class Discemone extends Actor with ActorLogging {
  import MemberGateway._
  import SensorHub._
  
  val sensorHub = context.actorOf(Props[SensorHub], "sensorHub")
  val memberGateway = context.actorOf(Props[MemberGateway], "memberGateway")  
  val processStatistic = context.actorOf(Props[ProcessStatistics], "processStatistics")
  
  val logger =  LoggerFactory.getLogger(getClass)
  
  override def preStart(): Unit = {
    memberGateway ! Start
  }

  def receive = {
    case MonitoredSensorCount(count) => {
      logger.info("sensorHub monitoring " + count + " sensors")
    }
    case heartbeat: MemberHeartbeat => {
      logger.info ("Heartbeat: " + heartbeat.representation)
    } 
    case "Count" => {
      logger.info ("Count request")
      sender ! "Got that"
    }
  }
}