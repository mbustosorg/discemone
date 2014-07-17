package org.bustos.discemone

import com.rapplogic.xbee._
import com.rapplogic.xbee.api._
import com.rapplogic.xbee.api.zigbee._
import com.rapplogic.xbee.api.wpan._

/** Sculpture member representative
 * 
 * Object representing state of affairs of 
 * a sculpture member.  Current pattern and 
 * settings.
 */

class Member (newAddress: XBeeAddress64) {

  def address = newAddress
  var red: Byte = 0
  var green: Byte = 0
  var blue: Byte = 0
  var speed: Byte = 128.toByte
  var intensity: Byte = 128.toByte
  
  var heartbeat: MemberHeartbeat = null
  
  var timestamp: Long = 0
	
  def setFromHeartbeat (newHeartbeat: MemberHeartbeat) {
    heartbeat = newHeartbeat
    timestamp = System.currentTimeMillis()
  }
  
  def setFromPayload (newPayload: Array[Int]) {
    
  }
}