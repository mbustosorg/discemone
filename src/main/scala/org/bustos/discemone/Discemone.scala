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
  case class CollectBatteryTimeSeries
  case class CollectMemoryTimeSeries
  case class MemberCount
  case class ThresholdValue(newValue: Int)
}

class Discemone extends Actor with ActorLogging {
  import context._
  import Discemone._
  import DiscemoneConsole._
  import MemberGateway._
  import Sensor._
  import SensorHub._
  import ProcessStatistics._ 
  import akka.util.Timeout
  import scala.concurrent.duration._
  
  implicit val defaultTimeout = Timeout(500)
  
  val sensorHub = actorOf(Props[SensorHub], "sensorHub")
  val memberGateway = actorOf(Props[MemberGateway], "memberGateway")  
  val processStatistics = actorOf(Props[ProcessStatistics], "processStatistics")
  val consoleInput = actorOf(Props[DiscemoneConsole], "discemoneConsole")
  
  var patternControl = new SensorPatternControl
  val logger =  LoggerFactory.getLogger(getClass)
  
  override def preStart(): Unit = {
    memberGateway ! Start
  }

  def receive = {
    case ConsoleInput(inputString) => {
      logger.debug(inputString)
      sensorHub ! SensorCommand(inputString)
    }
    case MonitoredSensorCount(count) => {
      logger.info ("sensorHub monitoring " + count + " sensors")
    }
    case SensorInput(name, profile) => {
      patternControl.processSensorInput(name, profile)
      patternControl.members.map {case (x, y) => {
        memberGateway ! Send(y.address, Array(patternControl.pattern, patternControl.speed,
        									  patternControl.colorLevel(name, patternControl.redSensor),
        									  patternControl.colorLevel(name, patternControl.greenSensor),
        									  patternControl.colorLevel(name, patternControl.blueSensor),
        									  patternControl.intensity))
      }}
    }
    case heartbeat: MemberHeartbeat => {
      patternControl.processHeartbeat (heartbeat)
      logger.info ("Heartbeat: " + heartbeat.representation)
    } 
    case CollectCPUtimeSeries => {
      val cpuQuery = processStatistics ? CPUtimeSeries 
      sender ! Await.result (cpuQuery, 1 second)
      logger.debug ("CollectCPUtimeSeries request delivered")
    } 
    case CollectMemoryTimeSeries => {
      val cpuQuery = processStatistics ? MemoryTimeSeries 
      sender ! Await.result (cpuQuery, 1 second)
      logger.debug ("CollectMemoryTimeSeries request delivered")
    } 
    case MemberCount => {
      sender ! Some(patternControl.members.size)
      logger.debug ("MemberCount request delivered")      
    }
    case "Count" => {
      sender ! "Got that"
      logger.debug ("Count request delivered")
    }
    case _ => {
      logger.debug ("Received Unknown message")
    }
  }
}