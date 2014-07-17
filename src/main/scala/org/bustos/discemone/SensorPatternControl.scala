package org.bustos.discemone

import java.awt.Color
import org.slf4j.{Logger, LoggerFactory}

/** Sensor Pattern Control Mechanism
 * 
 * Object to manage sensor inputs and how they
 * are aggregated to control and create interactive
 * responses.
 */

object SensorPatternControl { 
  case class ColorCircle extends SensorPatternControl; ColorCircle()
  case class RGB extends SensorPatternControl; RGB()
  case class Spectrum extends SensorPatternControl; Spectrum()
}

class SensorPatternControl {
  
  import SensorPatternControl._
  
  val logger = LoggerFactory.getLogger(getClass)
  
  val redSensor = 8
  val greenSensor = 7
  val blueSensor = 6
  var mode: SensorPatternControl = ColorCircle()
    
  var members = Map.empty[String, Member]
  var sensorProfile = Map.empty[String, List[Int]]
  var sensorChange = Map.empty[String, List[Int]]
  var sensorMax = Map.empty[String, List[Int]]
  var sensorMin = Map.empty[String, List[Int]]
  var sensorTimestamp = Map.empty[String, List[Long]]
  
  var hue = 0.0.toFloat;
  var sat = 1.0.toFloat;
  var bri = 1.0.toFloat;
  var pattern = 1;
  var speed = 255;
  var intensity = 255;

  def colorLevel (name: String, sensorId: Int): Int = {
    if (sensorProfile.contains(name)) {
      mode match {
        case ColorCircle() => {
          val color = Color.getHSBColor(hue, sat, bri)
          sensorId match {
            case `redSensor` => color.getRed()
            case `greenSensor` => color.getGreen()
            case `blueSensor` => color.getBlue()
            case _ => 0
          }
        }
        case RGB() => {
	      var proportion: Float = (sensorProfile(name)(sensorId) - sensorMin(name)(sensorId)).toFloat / (sensorMax(name)(sensorId) - sensorMin(name)(sensorId)).toFloat
	      if (proportion < 0.10) 0
	      else (proportion * 255.0).toInt          
        }
        case Spectrum() => {
          0
        }
      }
    } else 0
  }

  def displaySensorStatus(name: String) {
    if (logger.isDebugEnabled && sensorProfile.contains(name)) {
      var bounds: String = (for (i <- 0 to sensorProfile.size) yield {sensorMin(name)(i).toString + "-" + sensorProfile(name)(i).toString + "_" + sensorMax(name)(i).toString}).toString
  	  logger.debug(name + " " + bounds)
    }
  }
  /** Cycle through patterns based on sensor inputs
    * 
    */
  def detectPatternSelect(name: String) {
	 if (colorLevel(name, redSensor) > 200 && colorLevel(name, greenSensor) > 200) {
	   mode match {
	     case ColorCircle() => mode = RGB()
	     case RGB() => mode = Spectrum()
	     case Spectrum() => mode = ColorCircle()
	   }
	 }
  }
  
  def processSensorInput(name: String, profile: List[Int]) {
	  if (!sensorProfile.contains(name)) {
		sensorChange += (name -> profile)
	    sensorMax += (name -> profile)
	    sensorMin += (name -> profile)
	    sensorTimestamp += (name -> List.fill(profile.size) {System.currentTimeMillis()})
	  } else {
	    sensorChange += (name -> (for (itemIndex <- 0 to profile.size - 1) yield profile(itemIndex) - sensorProfile(name)(itemIndex)).toList)
	    sensorMax += (name -> (for (itemIndex <- 0 to profile.size - 1) yield profile(itemIndex).max(sensorMax(name)(itemIndex))).toList)
	    sensorMin += (name -> (for (itemIndex <- 0 to profile.size - 1) yield 
	      if (sensorMin(name)(itemIndex) == 0) profile(itemIndex) else profile(itemIndex).min(sensorMin(name)(itemIndex))).toList)
	    sensorTimestamp += (name -> (for (itemIndex <- 0 to profile.size - 1) yield 
	      if (profile(itemIndex) != sensorProfile(name)(itemIndex)) System.currentTimeMillis() else sensorTimestamp(name)(itemIndex)).toList)
	  }
	  sensorProfile += (name -> profile)
	  displaySensorStatus(name)
	  detectPatternSelect(name)
      hue += 0.01.toFloat
      if (hue >= 1.0) hue = 0
      pattern = 3      
  }
  
  def processHeartbeat(heartbeat: MemberHeartbeat) {
	  val address = heartbeat.address.toString()	
	  if (!members.contains(address)) {
		  members += (address -> new Member(heartbeat.address))
	  }
	  members(address).setFromHeartbeat(heartbeat)
  }

}