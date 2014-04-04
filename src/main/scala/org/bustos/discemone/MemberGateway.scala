package org.bustos.discemone

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import akka.actor.ActorLogging
import scala.concurrent.duration.Duration
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
  case class Send(data: TxRequest64)
  case object CheckIncoming
}

class MemberGateway extends Actor with ActorLogging {

  import MemberGateway._
  import DiscemoneConfig._
  import java.io.IOException
  
  val logger =  LoggerFactory.getLogger(getClass)
  
  var controller: ActorRef = null
  val xbee: XBee = new XBee()
  var xbeeStarted: Boolean = false 
  var ioexceptionCount: Int = 0
  val IOExceptionCountLimit: Int = 20

  override def preStart(): Unit = {
	logger.info(s"Requesting to open XBee on port: ${DiscemoneConfig.XBeePort}, baud: ${DiscemoneConfig.XBeeBaud}")
    try {
    	xbee.open(DiscemoneConfig.XBeePort, DiscemoneConfig.XBeeBaud)
    	xbeeStarted = true
    } catch {
      case _: Throwable => logger.info ("Could not start XBee")
    }
  }

  def receive = {
    case Start => {
    	controller = sender
    	if (xbeeStarted) self ! CheckIncoming    	
    }
    case CheckIncoming => {
	    try {
	    	val response = xbee.getResponse(100)
		    response match {
		        case x: ZNetRxResponse => {
		          controller ! new MemberHeartbeat(x.getData())
		          logger.info("ZNetRxResponse Received")
		        } 
		        case _ => {
		          logger.info("Unknown response Received")
		        } 
		    }
	    } catch {
	      case xb: XBeeTimeoutException =>
	      case ioe: IOException => ioexceptionCount += 1  
	      case _: Throwable => logger.info("Unknown exception")
	    }
	    if (ioexceptionCount < IOExceptionCountLimit) self ! CheckIncoming
	    else {
	      logger.info("Exceeded IOExceptionCountLimit")
	      context stop self
	    }
    }
    case Send(data) => {}
  }
  
}  
