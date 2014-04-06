package org.bustos.discemone

import akka.actor.{ Actor, ActorRef, Props, ActorSystem }
import akka.actor.Stash
import akka.actor.ActorLogging
import akka.io.IO
import akka.util.ByteString

import rxtxio.Serial
import rxtxio.Serial._

import org.slf4j.{Logger, LoggerFactory}

/** Communication object for the pod sensor
 * 
 * Responsible for starting and managing USB serial link
 */
object Sensor { 
  case class SensorStarted(name: String)
  case class SensorUpdate(data: Int)
  case class SendCommand(commandName: String, commandValue: String)
  def apply(portName: String, baudRate: Int) = Props(classOf[Sensor], portName, baudRate)
  private def formatData(data: ByteString) = data.mkString("[", ",", "]") + " " + (new String(data.toArray, "UTF-8"))  
}

/** Communication object for the pod sensor
 *  
 *  @constructor create a new sensor monitor
 *  @param portName USB portname
 *  @param baudRate communication speed for port
 */

class Sensor(portName: String, baudRate: Int) extends Actor with ActorLogging with Stash {
  import Sensor._
  import context._
  
  val logger = LoggerFactory.getLogger(getClass)
  var messageCount = 0
  var mode = ""
  var sensorThreshold = 10
  var sensorHistory: Map [Char, List [Int]] = Map()
  val sensorNames: List [Char] = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i')
  val HistoryLimit: Integer = 100
  
  override def preStart() = {
    logger.info(s"Requesting to open sensor on port: ${portName}, baud: ${baudRate}")
    IO (Serial) ! ListPorts
    IO (Serial) ! Open(portName, baudRate)
  }
  
  def updateSensorHistory (name: Char, value: Int) = {
	if (!sensorHistory.contains(name)) sensorHistory += (name -> List(value))
	else sensorHistory += (name -> (value :: sensorHistory.get(name).get).take (HistoryLimit))
  }
  
  def receive = {    
  	case Ports(ports) => {
  	  ports.filter(x => x.contains("usb")).map(x => logger.info(s"Available USB portname: ${x}"))
  	}
    case CommandFailed(cmd, reason) => {
      logger.error(s"Connection failed, stopping terminal. Reason: ${reason}")
      context stop self
    }
    case Opened(s, _) => {
      val operator = sender
      context become opened(operator)
      parent ! SensorStarted(self.path.name)
      unstashAll()
    }
    case other => stash()
  }

  /** Interface for sensor once serial communication has been established
   *  
   *  @param operator serial IO component
   */ 
  def opened(operator: ActorRef): Receive = {
    case Received(data) => {
      // Check if it's a sensor update
      // Check if it's a command response
      
      new String(data.toArray, 0) match {
        case "TESTMODE" => mode = "TESTMODE"
        case "PRODMODE" => mode = "PRODMODE"
        case r"THRSHLD:([0-9])${newValue}" => sensorThreshold = newValue.toInt
        case r"SENSOR:([0-9])$a-([0-9])$b-([0-9])$c-([0-9])$d-([0-9])$e-([0-9])$f-([0-9])$g-([0-9])$h-([0-9])$i.*" => {
          updateSensorHistory ('a', a.toInt)
          updateSensorHistory ('b', b.toInt)
          updateSensorHistory ('c', c.toInt)
          updateSensorHistory ('d', d.toInt)
          updateSensorHistory ('e', e.toInt)
          updateSensorHistory ('f', f.toInt)
          updateSensorHistory ('g', g.toInt)
          updateSensorHistory ('h', h.toInt)
          updateSensorHistory ('i', i.toInt)
        }
        case _ => {
          // Unknown command
        }
      }
      messageCount += 1
      if (messageCount % 100 == 0) {
        parent ! SensorUpdate(messageCount) 
      }
    }
    case SendCommand(name, value) => {
      operator ! Write (ByteString (name + " " + value))      
    }
    case Closed => {
      logger.info("Operator closed normally, exiting terminal.")
      context unwatch operator
      context stop self
    }
    
  }
}