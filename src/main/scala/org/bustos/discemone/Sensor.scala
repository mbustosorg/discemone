package org.bustos.discemone

import akka.actor.{ Actor, ActorRef, Props, ActorSystem }
import akka.actor.Stash
import akka.actor.ActorLogging
import akka.io.IO
import akka.util.ByteString

//import rxtxio.Serial
//import rxtxio.Serial._
import _root_.com.github.jodersky.flow.AccessDeniedException
import _root_.com.github.jodersky.flow.Serial
import _root_.com.github.jodersky.flow.Serial._
import _root_.com.github.jodersky.flow.SerialSettings
import _root_.com.github.jodersky.flow.Parity

import scala.util.matching.Regex

import org.slf4j.{Logger, LoggerFactory}

/** Communication object for the pod sensor
 * 
 * Responsible for starting and managing USB serial link
 */
object Sensor { 
  case class SensorStarted(name: String)
  case class SensorUpdate(profile: List[Int])
  case object SensorNoSuchPortException extends Throwable
  case object SensorIOException
  
  def apply(portName: String, baudRate: Int) = Props(classOf[Sensor], portName, baudRate)
  private def formatData(data: ByteString) = data.mkString("[", ",", "]") + " " + (new String(data.toArray, "UTF-8"))  
}

/** Communication object for the pod sensor
 *  
 *  @constructor create a new sensor monitor
 *  @param portName USB portname
 *  @param baudRate communication speed for port
 */

class Sensor(portName: String, baudRate: Int) extends Actor with ActorLogging {
  import Sensor._
  import context._  
  import Discemone._

  val logger = LoggerFactory.getLogger(getClass)
  var runningString = ""
  var messageCount = 0
  var mode = ""
  var sensorThreshold = DiscemoneConfig.SensorThresholdDefault
  var filterLength = DiscemoneConfig.SensorFilterDefault
  var throttle = DiscemoneConfig.SensorThrottleDefault
  var sensorHistory: Map [Char, List [Int]] = Map()
  val sensorNames: List [Char] = List('a', 'b', 'c', 'd', 'e', 'f', 'g')
  val HistoryLimit: Integer = 100
  val SensorMessageExpression: Regex = ".*DATA:s_0=([0-9]+),s_1=([0-9]+),s_2=([0-9]+),s_3=([0-9]+),s_4=([0-9]+),s_5=([0-9]+),s_6=([0-9]+).*".r
  val ThrsConfirmExpression: Regex = "THRS:([0-9]+)".r
  val FiltConfirmExpression: Regex = "FILT:([0-9]+)".r
  val ThrtConfirmExpression: Regex = "THRT:([0-9]+)".r
	
  override def preStart() = {
    logger.info(s"Requesting to open sensor on port: ${portName}, baud: ${baudRate}")
    val settings = SerialSettings(
    					baud = baudRate,
    					characterSize = 8,
    					twoStopBits = false,
    					parity = Parity.None
    		)

    IO(Serial) ! Serial.Open(portName, settings)

    //IO (Serial) ! ListPorts
    //IO (Serial) ! Open(portName, baudRate)
  }
  
  def updateSensorHistory (name: Char, value: Int) = {
	if (!sensorHistory.contains(name)) sensorHistory += (name -> List(value))
	else sensorHistory += (name -> (value :: sensorHistory.get(name).get).take (HistoryLimit))
  }
  
  def receive = {    
  	//case Ports(ports) => {
  	//  ports.filter(x => x.contains("usb")).map(x => logger.info(s"Available USB portname: ${x}"))
  	//}
  	case CommandFailed(cmd: Open, reason: AccessDeniedException) => {
      logger.error(s"Connection failed, access denied")
  	}
  	case CommandFailed(cmd, reason) => {
      logger.error(s"Connection failed, stopping terminal. Reason: ${reason}")
      context stop self
      //throw SensorNoSuchPortException
    }
  	case Opened(settings) => {
      val operator = sender
    //}
    //case Opened(operator, _) => {
      context become opened(operator)
      context watch operator
      parent ! SensorStarted(self.path.name)
  	}
  }

  /** Interface for sensor once serial communication has been established
   *  
   *  @param operator serial IO component
   */ 
  def opened(operator: ActorRef): Receive = {
    case s: String => {
      logger.debug ("SEND: " + s)
      operator ! Write(ByteString(s + "\r\n"))
    }
    case Received(data) => {
      val dataString = new String(data.filter(x => x != 0x0A && x != 0x0D) .toArray, 0) // Filter out LF and CR
      dataString match {
        case SensorMessageExpression(a, b, c, d, e, f, g) => {
        	updateSensorHistory ('a', a.toInt)
        	updateSensorHistory ('b', b.toInt)
			updateSensorHistory ('c', c.toInt)
			updateSensorHistory ('d', d.toInt)
			updateSensorHistory ('e', e.toInt)
			updateSensorHistory ('f', f.toInt)
			updateSensorHistory ('g', g.toInt)
			logger.debug("Processed: " + dataString)
        }
        case ThrsConfirmExpression(newValue) => {
          sensorThreshold = newValue.toInt
          logger.info ("Received: " + dataString)
        }
        case FiltConfirmExpression(newValue) => {
          filterLength = newValue.toInt
          logger.info ("Received: " + dataString)
        }
        case ThrtConfirmExpression(newValue) => {
          throttle = newValue.toInt
          logger.info ("Received: " + dataString)
        }
        case unhandledString => {
          runningString += unhandledString
          runningString match {
          	case SensorMessageExpression(a, b, c, d, e, f, g) => {
            	updateSensorHistory ('a', a.toInt)
            	updateSensorHistory ('b', b.toInt)
            	updateSensorHistory ('c', c.toInt)
            	updateSensorHistory ('d', d.toInt)
            	updateSensorHistory ('e', e.toInt)
            	updateSensorHistory ('f', f.toInt)
            	updateSensorHistory ('g', g.toInt)
            	runningString = ""
            }
            case _ => {
            	//logger.debug("Assembled: " + runningString)
            }
          }
        }
      }
      messageCount += 1
      val profile = sensorNames.map (x => {if (sensorHistory.contains(x)) sensorHistory(x).head else 0})
      parent ! SensorUpdate(profile) 
    }
    case Closed => {
      logger.info("Operator closed normally, exiting terminal.")
      context unwatch operator
      context stop self
    }
    case ListRequestSensor => {
    	sender ! SensorDetail(self.path.name, sensorThreshold, filterLength, throttle) 
    }
  }
}