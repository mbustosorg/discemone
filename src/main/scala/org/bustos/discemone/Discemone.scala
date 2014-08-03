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
  case class TimeSeriesRequestCPU
  case class TimeSeriesRequestMemory
  case class TimeSeriesRequestBattery
  case class ListRequestSensor
  case class ListRequestMember
  case class MemberCount
  case class ThresholdValue(newValue: Int)
  case class MetricHistory(history: List[Double])
  case class MetricValue(value: Double)
  case class SensorActivityLevel(id: String)
  case class SensorDetail(name: String, threshold: Int, filterLength: Int)
  case class SensorList(collection: List[SensorDetail])
  case class MemberDetail(name: String, 
		  				  xbee: String,      // Lower 32 bit XBee address
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
  
  implicit val defaultTimeout = Timeout(2000)
    
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
  	case TimeSeriesRequestCPU => {
      val future = processStatistics ? TimeSeriesRequestCPU
      sender ! Await.result(future, defaultTimeout.duration)
      logger.info ("TimeSeriesRequestCPU request delivered")
    } 
    case TimeSeriesRequestMemory => {
      val future = processStatistics ? TimeSeriesRequestMemory
      sender ! Await.result (future, 1 second)
      logger.info ("TimeSeriesRequestMemory request delivered")
    } 
    case TimeSeriesRequestBattery => {
      val future = processStatistics ? TimeSeriesRequestBattery
      sender ! Await.result (future, 1 second)
      logger.info ("TimeSeriesRequestBattery request delivered")
    } 
    case ListRequestSensor => {
      val future = sensorHub ? ListRequestSensor
      sender ! Await.result (future, 1 second) 
      logger.info ("ListRequestSensor request for delivered")            
    }
    case SensorActivityLevel(name) => {
      val future = sensorHub ? SensorActivityLevel(name)
      sender ! Await.result (future, 2 second)
      logger.info ("SensorActivityLevel request for " + name + " delivered")      
    }
    case "MEMBER_COUNT" => {
      sender ! MetricValue(patternControl.members.size)
      logger.info ("MemberCount request delivered")      
    }
    case ListRequestMember => {
      sender ! patternControl.memberDetails
      logger.info ("ListRequestMember request delivered")      
    }
    case MemberDetail(name, "", 0, 0, 0, 0, 0) => {
      patternControl.memberDetail(name)
      logger.info ("Member detail request delivered")
    }
    // Put commands
    case SensorDetail(name, threshold, filterLength) => {
      if (threshold > 0) sensorHub ! SensorCommand(name, "THRS " + threshold)
      if (filterLength > 0) sensorHub ! SensorCommand(name, "FILT " + filterLength)
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
        									  patternControl.intensity, 0))
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