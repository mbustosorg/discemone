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
	
	def XBeePort: String = {
	  if (hostname == "maxPro.local") "/dev/tty.usbserial-AH001572"
	  else "/dev/ttyS81"
	}
	
	def SensorPorts: Seq[String] = {
	  if (hostname == "maxPro.local") {
	    Seq("/dev/tty.usbmodem8881")
	  } else {
	    Seq("/dev/ttyS80")
	  }
	}
		
}