package org.bustos.discemone

import akka.actor.{ActorSystem, ActorRef, Actor, Props, Kill}
import akka.actor.ActorLogging
import akka.pattern.ask
import scala.concurrent.Await
import org.slf4j.{Logger, LoggerFactory}
import _root_.com.rapplogic.xbee.api.XBeeException
import java.net.InetAddress

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
  case class SensorDetail(name: String, threshold: Int, filterLength: Int, throttle: Int)
  case class SensorList(collection: List[SensorDetail])
  case class MemberDetail(name: String, 
		  				  xbee: String,       // Lower 32 bit XBee address
		  				  pattern: Int,       // Pattern id
		  				  lat: Float,         // Latitude in decimal degrees
		  				  lon: Float,         // Longitude in decimal degrees
		  				  alt: Float,         // Altitude in feet
		  				  battery: Float,     // Battery voltage
		  				  heartbeatAge: Int   // Seconds since last heartbeat
		  				  )
  case class MemberList(collection: List[MemberDetail])
  case class PatternCommand(name: String, intensity: Int, red: Int, green: Int, blue: Int, speed: Int, modDelay: Int)
  case class PatternNames
  case class CurrentPattern
  case class SetTime(seconds: Int)
}

class Discemone extends Actor with ActorLogging {
  import context._
  import Discemone._
  import DiscemoneConsole._
  import MemberGateway._
  import Sensor._
  import SensorHub._
  import ProcessStatistics._
  import java.io.IOException
  
  import akka.actor.OneForOneStrategy
  import akka.actor.SupervisorStrategy._
  import akka.util.Timeout
  import scala.concurrent.duration._
  
  implicit val defaultTimeout = Timeout(10000)
  
  var xBeeRetries: Int = 0;
    
  val sensorHub = actorOf(Props[SensorHub], "sensorHub")
  val memberGateway = actorOf(Props[MemberGateway], "memberGateway")  
  val processStatistics = actorOf(Props[ProcessStatistics], "processStatistics")
  val consoleInput = actorOf(Props[DiscemoneConsole], "discemoneConsole")
  
  override val supervisorStrategy =
      OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 30 seconds) {
        case _: ArithmeticException      => Resume
        case _: _root_.com.rapplogic.xbee.api.XBeeException              => {
          logger.info ("Restarting MemberGateway")
          Thread.sleep(1000)
          xBeeRetries = xBeeRetries + 1
          if (xBeeRetries > 8) {
              val hostname = InetAddress.getLocalHost().getHostName()
        	  if (hostname.toLowerCase().contains ("raspberrypi")) {
        		  System.exit(1)
        	  }
          }
          Restart
        }
        case _: NullPointerException     => Restart
        case _: IllegalArgumentException => Stop
        case _: Exception                => Escalate
      }
  
  var patternControl = new SensorPatternControl
  val logger =  LoggerFactory.getLogger(getClass)
   
  def receive = {
    // RESTful API activity
  	case TimeSeriesRequestCPU => {
      val future = processStatistics ? TimeSeriesRequestCPU
      sender ! Await.result(future, defaultTimeout.duration)
      logger.info ("TimeSeriesRequestCPU request delivered")
    } 
    case TimeSeriesRequestMemory => {
      val future = processStatistics ? TimeSeriesRequestMemory
      sender ! Await.result (future, defaultTimeout.duration)
      logger.info ("TimeSeriesRequestMemory request delivered")
    } 
    case TimeSeriesRequestBattery => {
      val future = processStatistics ? TimeSeriesRequestBattery
      sender ! Await.result (future, defaultTimeout.duration)
      logger.info ("TimeSeriesRequestBattery request delivered")
    } 
    case ListRequestSensor => {
      val future = sensorHub ? ListRequestSensor
      sender ! Await.result (future, defaultTimeout.duration) 
      logger.info ("ListRequestSensor request for delivered")            
    }
    case SensorActivityLevel(name) => {
      sender ! patternControl.sensorStatus(name)
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
    case MemberDetail(name, "", 0, 0, 0, 0, 0, 0) => {
      patternControl.memberDetail(name)
      logger.info ("Member detail request delivered")
    }
    case PatternNames => {
      sender ! patternControl.patternIdMap
      logger.info ("Pattern names request delivered")
    }
    case CurrentPattern => {
      sender ! patternControl.commandedPattern
      logger.info ("Current pattern request delivered")      
    }
    // Put commands
    case SensorDetail(name, threshold, filterLength, throttle) => {
      if (threshold > 0) sensorHub ! SensorCommand(name, "THRS " + threshold)
      if (filterLength > 0) sensorHub ! SensorCommand(name, "FILT " + filterLength)
      if (throttle > 0) sensorHub ! SensorCommand(name, "THRT " + throttle)
      logger.info ("Sensor command processed")      
    }
    case command: PatternCommand => {
      patternControl.processCommand(command)
      var modDelay: Int = 0
      if (command.modDelay > 0 && command.modDelay < 127) modDelay = 0x80 | command.modDelay
      patternControl.members.map {case (x, y) => {
        logger.info ("Sending to " + y.address + " with modDelay = " + modDelay)
        if (patternControl.innerRing(y.address.toString())) modDelay = modDelay | 0x10
        else if (patternControl.middleRing(y.address.toString())) modDelay = modDelay | 0x30
        else if (patternControl.outerRing(y.address.toString())) modDelay = modDelay | 0x70
        memberGateway ! Send(y.address, Array(command.name.toInt, command.speed, command.red, command.green, command.blue, command.intensity, modDelay))
      }}
      logger.info ("Pattern command processed")      
    }
    case SetTime(seconds) => {
      Runtime.getRuntime().exec("date -s " + seconds); 
      logger.info ("Set time command processed")      
    }
    // XBee Gateway activity
    case heartbeat: MemberHeartbeat => {
      patternControl.processHeartbeat (heartbeat)	 
      if (patternControl.commandedPattern != null && heartbeat.currentPattern > 0 && heartbeat.currentPattern.toString != patternControl.commandedPattern.name) {
        if (System.currentTimeMillis > patternControl.commandedPattern.modDelay * 1000 + patternControl.commandedPatternTime) {
        	memberGateway ! Send(heartbeat.address, patternControl.commandedPatternPayload)
        }
	  }
      logger.info ("Heartbeat: " + heartbeat.representation)
    } 
    
    // Sensor activity
    case MonitoredSensorCount(count) => {
      logger.info ("sensorHub monitoring " + count + " sensors")
    }
    case SensorInput(name, profile) => {
      patternControl.processSensorInput(name, profile)
      patternControl.membersForSensor (name).map {case (x, y) => {
        memberGateway ! Send(y.address, patternControl.sensorResponsePayload (name))
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