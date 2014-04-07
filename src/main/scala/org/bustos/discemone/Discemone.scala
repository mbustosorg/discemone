package org.bustos.discemone

import akka.actor.{ActorSystem, ActorRef, Actor, Props}
import akka.actor.ActorLogging
import akka.pattern.ask

import scala.concurrent.Await

import org.slf4j.{Logger, LoggerFactory}

/** Controller for Discemone
 * 
 * The pod sensors and XBee access are managed by this object.
 * This also provides system summary access for rendering
 * by external viewers.  
 */

object Discemone {
  case class CollectCPUtimeSeries
}

class Discemone extends Actor with ActorLogging {
  import context._
  import Discemone._
  import MemberGateway._
  import SensorHub._
  import ProcessStatistics._ 
  import akka.util.Timeout
  import scala.concurrent.duration._
  
  implicit val defaultTimeout = Timeout(500)
  
  val sensorHub = actorOf(Props[SensorHub], "sensorHub")
  val memberGateway = actorOf(Props[MemberGateway], "memberGateway")  
  val processStatistics = actorOf(Props[ProcessStatistics], "processStatistics")
  
  val logger =  LoggerFactory.getLogger(getClass)
  
  override def preStart(): Unit = {
    memberGateway ! Start
  }

  def receive = {
    case MonitoredSensorCount(count) => {
      logger.info ("MonitoredSensorCount request")
      logger.info("sensorHub monitoring " + count + " sensors")
      logger.info ("MonitoredSensorCount request delivered")
    }
    case heartbeat: MemberHeartbeat => {
      logger.info ("MemberHeartbeat request")
      logger.info ("Heartbeat: " + heartbeat.representation)
      logger.info ("MemberHeartbeat request delivered")
    } 
    case CollectCPUtimeSeries => {
      logger.info ("CollectCPUtimeSeries request")
      val cpuQuery = processStatistics ? CPUtimeSeries 
      sender ! Await.result (cpuQuery, 1 second)
      logger.info ("CollectCPUtimeSeries request delivered")
    } 
    case "Count" => {
      logger.info ("Count request")
      sender ! "Got that"
      logger.info ("Count request delivered")
    }
  }
}