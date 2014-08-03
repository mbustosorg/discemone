package org.bustos.discemone

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import akka.actor.ActorLogging
import scala.concurrent.duration._
import java.util.concurrent.TimeUnit

import com.rapplogic.xbee._
import com.rapplogic.xbee.api._
import com.rapplogic.xbee.api.zigbee._
import com.rapplogic.xbee.api.wpan._

import org.slf4j.{Logger, LoggerFactory}

/** Provide input / output functionality for XBee communication
 * 
 * Will die on persistent IOException so that the parent can restart.
 */

object MemberGateway {
  case class Start
  case class Send (address: XBeeAddress64, data: Array[Int])
  case class Broadcast (data: Array[Int])
  case object CheckIncoming
}

class MemberGateway extends Actor with ActorLogging {

  import MemberGateway._
  import context._
  import DiscemoneConfig._
  import java.io.IOException
  
  val logger =  LoggerFactory.getLogger(getClass)
  
  var controller: ActorRef = null
  val xbee: XBee = new XBee()
  var xbeeStarted: Boolean = false 
  var ioexceptionCount: Int = 0
  val IOExceptionCountLimit: Int = 20
  val tickInterval = 10 milliseconds
  val tickScheduler = system.scheduler.schedule (0 milliseconds, tickInterval, self, CheckIncoming)
  
  override def preStart(): Unit = {
	logger.info(s"Requesting to open XBee on port: ${DiscemoneConfig.XBeePort}, baud: ${DiscemoneConfig.XBeeBaud}")
    try {
    	xbee.open(DiscemoneConfig.XBeePort, DiscemoneConfig.XBeeBaud)
    	logger.info ("XBee started")
    	xbeeStarted = true
    } catch {
      case _: Throwable => logger.info ("Could not start XBee")
    }
  }

  def receive = {
    case Start => {
    	controller = sender
    }
    case CheckIncoming => {
    	if (xbeeStarted) {
		    try {
		    	val response = xbee.getResponse(1)
			    response match {
			        case x: ZNetRxResponse => {
			          val heartbeat = new MemberHeartbeat(x.getRemoteAddress64, x.getData)
			          controller ! heartbeat
			        } 
			        case x: ZNetTxStatusResponse => {
			          logger.debug("Status Response Received")			          
			        }
			        case _ => {
			          logger.debug("Unknown response Received")
			        } 
			    }
		    } catch {
		      case xb: XBeeTimeoutException =>
		      case ioe: IOException => ioexceptionCount += 1  
		      case _: Throwable => logger.info("Unknown exception")
		    }
		    if (ioexceptionCount >= IOExceptionCountLimit) {
		      logger.info("Exceeded IOExceptionCountLimit")
		      context stop self
		    }    	  
    	}
    }
    case Send(address, data) => {
      if (xbeeStarted) {
    	  val newMessage = new ZNetTxRequest (address, data)
    	  xbee.sendAsynchronous(newMessage)
    	  if (logger isDebugEnabled) logger.debug("Sent message " + data.toString + " to " + address.toString)
      }
    }
  }
  
}  
