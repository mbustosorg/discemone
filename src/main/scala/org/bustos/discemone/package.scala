package org.bustos

/** This is the ScalaDoc for the package.
 *  Discemone is a large scale interactive sculpture being built for Burning Man 2014. 
 *  It consists of a collection of 30' tall illuminated towers connected through an XBee network. 
 *  This package contains the code responsible for coordinating various components of the system. 
 *  This includes sensor inputs from capacitive input devices.
 *  Full project coverage is available on our [[https://www.facebook.com/seagrassProject Facebook]] page. 
 */

import scala.util.matching.Regex

package object discemone {
   
	implicit class RegexContext(sc: StringContext) {
	  def r = new Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
	}
}