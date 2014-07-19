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
  case class MemberCount
  case class ThresholdValue(newValue: Int)
  case class MetricHistory(history: List[Double])
  case class MetricValue(value: Double)
  case class SensorActivityLevel(id: String)
  case class SensorDetail(name: String, threshold: Int, filterLength: Int)
  case class SensorList(collection: List[Sensor])
  case class MemberDetail(name: String, 
		  				  xbee: Int,      // Lower 32 bit XBee address
		  				  pattern: Int,   // Pattern id
		  				  lat: Float,         // Latitude in decimal degrees
		  				  lon: Float,         // Longitude in decimal degrees
		  				  alt: Float,         // Altitude in feet
		  				  battery: Float)      // Battery voltage
  case class MemberList(collection: List[MemberDetail])
  case class PatternCommand(name: String, intensity: Int, red: Int, green: Int, blue: Int, speed: Int)
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
    // RESTful API activity
  	case "CPU_TIME_SERIES_REQUEST" => {
      val cpuQuery = processStatistics ? CPUtimeSeries 
      sender ! Await.result (cpuQuery, 1 second)
      logger.info ("CPU_TIME_SERIES_REQUEST request delivered")
    } 
    case "MEM_TIME_SERIES_REQUEST" => {
      val cpuQuery = processStatistics ? MemoryTimeSeries 
      sender ! Await.result (cpuQuery, 1 second)
      logger.info ("MEM_TIME_SERIES_REQUEST request delivered")
    } 
    case "BAT_TIME_SERIES_REQUEST" => {
      sender ! MetricHistory(List(4.0, 4.0, 1.0, 1.0))
      logger.info ("BAT_TIME_SERIES_REQUEST request delivered")
    } 
    case SensorActivityLevel(name) => {
      sender ! MetricHistory(List(1.0, 1.0, 1.0, 1.0))
      logger.info ("SensorActivityLevel request for " + name + " delivered")      
    }
    case "SENSOR_LIST_REQUEST" => {
      val query = sensorHub ? "SENSOR_LIST_REQUEST"
      sender ! Await.result (query, 1 second) 
      logger.info ("SensorList request for delivered")            
    }
    case "MEMBER_COUNT" => {
      sender ! patternControl.members.size
      logger.info ("MemberCount request delivered")      
    }
    case "MEMBER_LIST_REQUEST" => {
      sender ! patternControl.memberDetails
      logger.info ("MemberList request delivered")      
    }
    case MemberDetail(name, 0, 0, 0, 0, 0, 0) => {
      patternControl.memberDetail(name)
      logger.info ("Member request delivered")
    }
    // Put commands
    case SensorDetail(name, threshold, filterLength) => {
      if (threshold > 0) sensorHub ! SensorCommand(name, "THRS " + threshold)
      if (filterLength > 0) sensorHub ! SensorCommand(name, "FILT " + threshold)
      logger.info ("Sensor command processed")      
    }
    case PatternCommand(name, intensity, red, green, blue, speed) => {
      sender ! "OK"
      logger.info ("Pattern command processed")      
    }
    // XBee Gateway activity
    case heartbeat: MemberHeartbeat => {
      patternControl.processHeartbeat (heartbeat)
      logger.info ("Heartbeat: " + heartbeat.representation)
    } 
    
    // Sensor activity
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
    
    // Console activity
    case ConsoleInput(inputString) => {
      logger.debug(inputString)
      sensorHub ! SensorCommand("", inputString)
    }
    case _ => {
      logger.debug ("Received Unknown message")
    }
  }
}