package org.bustos.discemone

import java.net.InetAddress

/** Discemone hardware connectivity configuration
 * 
 * Port names are specific to the host.
 */

object DiscemoneConfig {

	val hostname = InetAddress.getLocalHost().getHostName()
	val XBeeBaud = 57600 
	val SensorBaud = 57600
	val SensorThresholdDefault = 1000
	val SensorFilterDefault = 2
	val SensorThrottleDefault = 1000
	
	val SensorList = SensorPorts.toList
	
	def XBeePort: String = {
	  if (hostname.toLowerCase().contains ("maxpro")) {
	    "/dev/tty.usbserial-AH001572"
	    //"/dev/cu.usbserial-AH001572"
	  }
	  else {
	    "/dev/ttyS80"
	  }
	}
	
	def SensorPorts: Seq[String] = {
	  if (hostname.toLowerCase().contains ("maxpro")) {
	    //Seq("/dev/tty.usbmodem8881", "/dev/tty.usbmodem8871", "/dev/tty.usbmodem451571")
	    Seq("/dev/cu.usbmodem451571", 
	        "/dev/cu.usbmodem451801", 
	        "/dev/cu.usbmodem452591"
	        )
	  } else {
	    Seq("/dev/ttyS81",  // Sensor 1
	        "/dev/ttyS82",  // Sensor 2
	        "/dev/ttyS83"   // Sensor 3
	        )
	  }
	}
		
}