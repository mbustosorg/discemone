package org.bustos.discemone

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor, Kill }
import akka.actor.ActorLogging
import scala.concurrent.duration._
import java.util.concurrent.TimeUnit

import _root_.com.rapplogic.xbee._
import _root_.com.rapplogic.xbee.api._
import _root_.com.rapplogic.xbee.api.zigbee._
import _root_.com.rapplogic.xbee.api.wpan._
import _root_.com.rapplogic.xbee.util.ByteUtils

import org.slf4j.{Logger, LoggerFactory}

/** Provide input / output functionality for XBee communication
 * 
 * Will die on persistent IOException so that the parent can restart.
 */

object MemberGateway {
  case class Send (address: XBeeAddress64, data: Array[Int])
  case class Broadcast (data: Array[Int])
  case class CheckIncoming
}

class MemberGateway extends Actor with ActorLogging {

  import MemberGateway._
  import context._
  import DiscemoneConfig._
  import java.io.IOException
  
  val logger =  LoggerFactory.getLogger(getClass)
  
  val xbee: XBee = new XBee(new XBeeConfiguration().withMaxQueueSize(100).withStartupChecks(false));
  
  //val flowConnection = actorOf(Props[FlowXBeeConnection], "flowXbeeConnection")
  
  var xbeeStarted: Boolean = false 
  var xbeeStarting: Boolean = false
  var ioexceptionCount: Int = 0
  val IOExceptionCountLimit: Int = 20
  val tickInterval = 10 milliseconds
  val tickScheduler = system.scheduler.schedule (0 milliseconds, tickInterval, self, CheckIncoming)

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
	  // Keep the call to postStop(), but no stopping of children
	  postStop()
  }
    
  def receive = {
    case CheckIncoming => {
      if (!xbeeStarted && !xbeeStarting) {
        xbeeStarting = true
    	logger.info(s"Requesting to open XBee on port: ${DiscemoneConfig.XBeePort}, baud: ${DiscemoneConfig.XBeeBaud}")
	    try {
	    	xbee.open(DiscemoneConfig.XBeePort, DiscemoneConfig.XBeeBaud)
	    	logger.info ("XBee started")
	    	xbeeStarted = true
	    	xbeeStarting = false
	    } catch {
	      case ex: Throwable => {
	        try {
	        	if (xbee.isConnected) xbee.close
	        } catch {
	          case ex: Throwable => {
	        	logger.info ("Could not close XBee: " + ex.toString)
	        	throw ex	            
	          }
	        }
	        logger.info ("Could not start XBee: " + ex.toString)
	        throw ex
	      }
	    }
      } else if (xbeeStarted && !xbeeStarting) {
    	  try {
    		  val response = xbee.getResponse(1)
			    response match {
			        case x: ZNetRxResponse => {
			          val heartbeat = new MemberHeartbeat(x.getRemoteAddress64, x.getData)
			          parent ! heartbeat
			        } 
			        case x: ZNetTxStatusResponse => {
			        	if (logger isDebugEnabled) logger.debug("Status Response Received")			          
			        }
			        case _ => {
			        	if (logger isDebugEnabled) logger.debug("Unknown response Received")
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
    	  if (logger isDebugEnabled) {
    	    logger.debug("Sent message " + ByteUtils.toBase16(data, ",") + " to " + address.toString)
    	  }
      }
    }
  }
  
}  
