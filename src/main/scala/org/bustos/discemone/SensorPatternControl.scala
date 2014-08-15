package org.bustos.discemone

import java.awt.Color
import org.slf4j.{Logger, LoggerFactory}

/** Sensor Pattern Control Mechanism
 * 
 * Object to manage sensor inputs and how they
 * are aggregated to control and create interactive
 * responses.
 */

class SensorPatternControl {
  
  import Discemone._
  
  val logger = LoggerFactory.getLogger(getClass)
  
  val redSensor = 0
  val greenSensor = 1
  val blueSensor = 2
  
  var commandedPattern: PatternCommand = PatternCommand ("34", 120, 0, 255, 0, 255, 0)
  var commandedPatternTime: Long = System.currentTimeMillis
  
  var mode = "ColorCircle"
    
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
  var speed = 10;
  var intensity = 255;

  def colorLevel (name: String, sensorId: Int): Int = {
    if (sensorProfile.contains(name)) {
      mode match {
        case "ColorCircle" => {
          val color = Color.getHSBColor(hue, sat, bri)
          sensorId match {
            case `redSensor` => color.getRed()
            case `greenSensor` => color.getGreen()
            case `blueSensor` => color.getBlue()
            case _ => 0
          }
        }
        case "RGB" => {
	      var proportion: Float = (sensorProfile(name)(sensorId) - sensorMin(name)(sensorId)).toFloat / (sensorMax(name)(sensorId) - sensorMin(name)(sensorId)).toFloat
	      if (proportion < 0.10) 0
	      else (proportion * 255.0).toInt          
        }
      }
    } else 0
  }

  def sensorResponsePayload (sensorName: String): Array[Int] = {
    Array(pattern, speed,
    		colorLevel(sensorName, redSensor),
    		colorLevel(sensorName, greenSensor),
    		colorLevel(sensorName, blueSensor),
    		intensity, 0)
  }
  
  def commandedPatternPayload: Array[Int] = {
    Array(commandedPattern.name.toInt, commandedPattern.speed,
          commandedPattern.red,
          commandedPattern.green,
          commandedPattern.blue,
          commandedPattern.intensity,
          0)
  }
  
  def sensorStatus(name: String): String = {
    if (sensorProfile.contains(name)) {
    	(for (i <- 0 to sensorProfile(name).size - 1) yield {sensorMin(name)(i).toString + " [" + sensorProfile(name)(i).toString + "] " + sensorMax(name)(i).toString}).toList.toString
    } else ""
  }
  
  /** Cycle through patterns based on sensor inputs
    * 
    */
  def detectPatternSelect(name: String) {
	 if (colorLevel(name, redSensor) > 200 && colorLevel(name, greenSensor) > 200) {
	   mode match {
	     case "ColorCircle" => mode = "RGB"
	     case "RGB" => mode = "ColorCircle"
	   }
	 }
  }
  
  def processCommand(pattern: PatternCommand) {
	  commandedPattern = pattern
	  commandedPatternTime = System.currentTimeMillis
	  if (patternIdMap.contains (pattern.name)) {
		  logger.info ("Commanded pattern '" + patternIdMap (pattern.name) + "'")
	  } else {
		  logger.info ("Unknown pattern id: " + pattern)
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
	      if (sensorMin(name)(itemIndex) == 0) profile(itemIndex) 
	      else {
	        if (System.currentTimeMillis() - sensorTimestamp(name)(itemIndex) > 5000) profile(itemIndex)
	        else profile(itemIndex).min(sensorMin(name)(itemIndex))
	      }
	      ).toList)
	    sensorTimestamp += (name -> (for (itemIndex <- 0 to profile.size - 1) yield 
	      if (profile(itemIndex) != sensorProfile(name)(itemIndex)) System.currentTimeMillis() else sensorTimestamp(name)(itemIndex)).toList)
	  }
	  sensorProfile += (name -> profile)
	  //if (logger.isDebugEnabled && sensorProfile.contains(name)) {
		  logger.info(name + " " + sensorStatus(name))
	  //}
	  detectPatternSelect(name)
      hue += 0.01.toFloat
      if (hue >= 1.0) hue = 0
      pattern = 3      
  }
  
  def processHeartbeat(heartbeat: MemberHeartbeat) {
	  val address = heartbeat.address.toString()	
	  if (!members.contains(address)) {
		  members += (address -> new Member((members.size + 1).toString, heartbeat.address))
	  }
	  members(address).setFromHeartbeat(heartbeat)
	  if (heartbeat.currentPattern == 45) {
		  logger.info ("Shaking detected")
	  }
  }
  
  def memberDetail(name: String): MemberDetail = {
      if (members.contains(name)) {
        val member = members(name)
        MemberDetail(name, member.address.toString(), 	
        			 member.heartbeat.currentPattern,
        			 member.heartbeat.latitude.toFloat,
        			 member.heartbeat.longitude.toFloat,
        			 0.0f,
        			 member.heartbeat.batteryVoltage.toFloat,
        			 (System.currentTimeMillis().toInt - member.heartbeat.timeStamp.toInt) / 1000
        			 )
      }
      else MemberDetail("unknown", "", 0, 0.0f, 0.0f, 0.0f, 0.0f, 0)
  }
  
  def memberDetails: MemberList = {
    MemberList(members.values.map(x => MemberDetail(x.name, x.address.toString(), 
    												x.heartbeat.currentPattern,
    												x.heartbeat.latitude.toFloat,
    												x.heartbeat.longitude.toFloat,
    												0.0f,
    												x.heartbeat.batteryVoltage.toFloat,
    												(System.currentTimeMillis().toInt - x.heartbeat.timeStamp.toInt) / 1000
    												)).toList)
  }
 
  def membersForSensor (name: String): Map[String, Member] = {
     members.filter {case (x, y) => {
       if (name == DiscemoneConfig.SensorList(0)) {
    	   x.toInt <= 11
       } else if (name == DiscemoneConfig.SensorList(1)) {
    	   x.toInt >= 12 || x.toInt <= 22
       } else if (name == DiscemoneConfig.SensorList(2)) {
    	   x.toInt >= 22 || x.toInt <= 33
       } else {
    	   false
       }
     }}
  }
  
  def innerRing (name: String): Boolean = {
      if (knownRadios.contains (name)) {
    	  val memberId: String = knownRadios (name)
    	  if (memberId == "7" || memberId == "8" || memberId == "9" || memberId == "10" || memberId == "11" ||
    	      memberId == "18" || memberId == "19" || memberId == "20" || memberId == "21" || memberId == "22" ||
    	      memberId == "29" || memberId == "30" || memberId == "31" || memberId == "32" || memberId == "33") true
		  else false
      } else false
  }

  def middleRing (name: String): Boolean = {
      if (knownRadios.contains (name)) {
    	  val memberId: String = knownRadios (name)
    	  if (memberId == "1" || memberId == "2" ||
    	      memberId == "12" || memberId == "13" ||
    	      memberId == "23" || memberId == "24") true
    	  else false
      } else false
  }
	 
  def outerRing (name: String): Boolean = {
      if (knownRadios.contains (name)) {
    	  val memberId: String = knownRadios (name)
    	  if (memberId == "3" || memberId == "4" || memberId == "5" || memberId == "6" ||
		  	memberId == "14" || memberId == "15" || memberId == "16" || memberId == "17" ||
		  	memberId == "25" || memberId == "26" || memberId == "27" || memberId == "28") true
		  else false
      } else false
  }
	 
      val patternIdMap = Map ("2" -> "Heartbeat Request", "3" -> "Full Color",
		  				  "4" -> "Sparkle", "5" -> "Descend", 
		  				  "6" -> "Off", "7" -> "Flash", 
		  				  "8" -> "Fire", "9" -> "Heart", 
		  				  "10" -> "Breath", "11" -> "Organic", 
		  				  "12" -> "Cylon", "13" -> "Drop", 
		  				  "14" -> "Character", "16" -> "Cylon Vertical", 
		  				  "17" -> "Cylon Pong", "18" -> "Breathe Evolve",
		  				  "19" -> "Word", "20" -> "Sound", 
		  				  "21" -> "Animate 1", "22" -> "Test Pattern", 
		  				  "23" -> "Prism", "24" -> "Prism Distance", 
		  				  "25" -> "Matrix", "26" -> "Rainbow Chase",
		  				  "27" -> "Image Scroll", "28" -> "Starfield", 
		  				  "29" -> "Spiral", "30" -> "Tilt", 
		  				  "31" -> "Shake Sparkle", "32" -> "Sparkler", 
		  				  "33" -> "Grass Wave", "34" -> "Radio Tower",
		  				  "35" -> "Bouncing Ball", "36" -> "Spectrum Analyze", 
		  				  "37" -> "Forest Run", "38" -> "Searching Eye", 
		  				  "39" -> "Bubble Wave", "40" -> "Broken", "41" -> "Pong",
		  				  "42" -> "Giant Spectrum", "43" -> "Flame", "44" -> "Candle", "45" -> "Shaking")

     val knownRadios = Map ("0x00,0x13,0xa2,0x00,0x40,0x79,0xE7,0x2B" -> "1",
	 	 	 			   	"0x00,0x13,0xa2,0x00,0x40,0x8B,0x5E,0x93" -> "2",		
							"0x00,0x13,0xa2,0x00,0x40,0x8B,0x68,0xAB" -> "3",
							"0x00,0x13,0xa2,0x00,0x40,0x79,0x5F,0x48" -> "4",
							"0x00,0x13,0xa2,0x00,0x40,0x94,0x21,0xA5" -> "5",
							"0x00,0x13,0xa2,0x00,0x40,0x79,0xE7,0x6A" -> "6",
							"0x00,0x13,0xa2,0x00,0x40,0x8B,0x60,0xE2" -> "7",
							"0x00,0x13,0xa2,0x00,0x40,0x79,0xE4,0xCD" -> "8",
							"0x00,0x13,0xa2,0x00,0x40,0x8B,0x68,0x26" -> "9",
							"0x00,0x13,0xa2,0x00,0x40,0x8B,0x61,0xCF" -> "10",
							"0x00,0x13,0xa2,0x00,0x40,0xA2,0x5C,0x1D" -> "11",
							"0x00,0x13,0xa2,0x00,0x40,0x79,0xE7,0x36" -> "12",
							"0x00,0x13,0xa2,0x00,0x40,0x94,0x21,0xBA" -> "13",
							"0x00,0x13,0xa2,0x00,0x40,0xA2,0x34,0x23" -> "14",
							"0x00,0x13,0xa2,0x00,0x40,0x8B,0x93,0x38" -> "15",
							"0x00,0x13,0xa2,0x00,0x40,0x8B,0x67,0xF6" -> "16",
							"0x00,0x13,0xa2,0x00,0x40,0x8B,0x94,0x36" -> "17",
							"0x00,0x13,0xa2,0x00,0x40,0x8B,0x93,0x24" -> "18",
							"0x00,0x13,0xa2,0x00,0x40,0x8B,0x93,0xD4" -> "19",
							"0x00,0x13,0xa2,0x00,0x40,0x79,0xE6,0xDE" -> "20",
							"0x00,0x13,0xa2,0x00,0x40,0x79,0xE6,0xF3" -> "21",
							"0x00,0x13,0xa2,0x00,0x40,0x8B,0x68,0x2A" -> "22",
							"0x00,0x13,0xa2,0x00,0x40,0xA2,0x5C,0x04" -> "23",
							"0x00,0x13,0xa2,0x00,0x40,0x8B,0x94,0x40" -> "24",
							"0x00,0x13,0xa2,0x00,0x40,0x8B,0x60,0xC4" -> "25",
							"0x00,0x13,0xa2,0x00,0x40,0x79,0xE4,0xD0" -> "26",
							"0x00,0x13,0xa2,0x00,0x40,0x89,0xB5,0xC8" -> "27",
							"0x00,0x13,0xa2,0x00,0x40,0x8B,0x94,0x48" -> "28",
							"0x00,0x13,0xa2,0x00,0x40,0x8B,0x94,0x03" -> "29",
							"0x00,0x13,0xa2,0x00,0x40,0x8B,0x94,0x5B" -> "30",
							"0x00,0x13,0xa2,0x00,0x40,0x90,0x28,0xC8" -> "31",
							"0x00,0x13,0xa2,0x00,0x40,0xA2,0x5C,0x30" -> "32",
							"0x00,0x13,0xa2,0x00,0x40,0xBE,0x2F,0x22" -> "33")
}