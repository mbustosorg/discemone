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
  
  override def preStart() = {
    logger.info(s"Requesting manager to open port: ${portName}, baud: ${baudRate}")
    IO (Serial) ! ListPorts
    IO (Serial) ! Open(portName, baudRate)
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
      messageCount += 1
      if (messageCount % 100 == 0) {
        parent ! SensorUpdate(messageCount) 
      }
      //logger.info(s"Received data: ${formatData(data)}")
    }
    
    case Closed => {
      logger.info("Operator closed normally, exiting terminal.")
      context unwatch operator
      context stop self
    }
    
  }
}