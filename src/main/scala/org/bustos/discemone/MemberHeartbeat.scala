package org.bustos.discemone

import com.rapplogic.xbee.api._

/** Record class for tower member heartbeat messages
 * 
 * @constructor create a new heartbeat representation
 * @param data heartbeat payload
 */

case class MemberHeartbeat (newAddress: XBeeAddress64, data: Array[Int]) {

  def address = newAddress
  def representation: String = {
	if (data.length > 18) {
		"M:" + message + "|V:" + versionId + "|P:" + currentPattern + "|B:" + "%.2f".format(batteryVoltage) + "|T:" + memberType + 
		"|LAT:" + latitude.toString + "|LON:" + longitude.toString + "|A64:" + address.toString
	} else {
		"M:" + message + "|V:" + versionId + "|A64:" + address.toString()	  
	}
  }
  def message: Int = data(0)
  def versionId: Int = data(1)
  //0,          // Byte 2: Frame location (2 bytes)
  //0,
  def currentPattern: Int = {
    if (data.length > 3) data(4)
    else -1
  }
  def batteryVoltage: Double = {
	if (data.length > 5) {
	  val fullVoltage: Double = (data(6) << 8).toDouble + data(5).toDouble 
	  if (memberType == 1) fullVoltage / 1024.0 * (8.3 / (5.1 / 7.1))
	  else fullVoltage / 81.0
	}
	else -1
  }
  def frameRate: Int = {
    if (data.length > 6) data(6)
    else -1
  }
  def memberType: Int = {
    if (data.length > 7) data(8)
    else -1
  }
  //0,          // Byte 9: Failed messages (2 bytes)
  //0,
  def latitude: Float = {
    if (data.length > 14) (data(14) << 24 | data(13) << 16 | data(12) << 8 | data(11)).toFloat / 100000    	
    else 0    	  
  }
  def longitude: Float = {
    if (data.length > 18) (data(18) << 24 | data(17) << 16 | data(16) << 8 | data(15)).toFloat / 100000
    else 0
  }
}