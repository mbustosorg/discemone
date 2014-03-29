package org.bustos.discemone

/** Record class for tower member heartbeat messages
 * 
 * @constructor create a new heartbeat representation
 * @param data heartbeat payload
 */
case class MemberHeartbeat (data: Array[Int]) {

  def representation: String = {
	"M:" + message + ".V:" + versionId + ".P:" + currentPattern + ".B:" + batteryVoltage + ".T:" + memberType
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
	  val fullVoltage = data(5) << 7 + data(6) 
	  if (memberType == 1) fullVoltage / 1024.0 * (8.3 / (5.1 / 7.1))
	  else fullVoltage / 1024.0 * (8.0 / (4.1 / 5.0))
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
  //0,          // Byte 11: Latitude (4 bytes)
  //0,
  //0,
  //0,
  //0,          // Byte 15: Longitude (4 bytes)
  //0,
  //0,
  //0
}