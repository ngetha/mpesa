package se.ngetha.mpesa.ipn.sinks

/**
 * Simple Profiler
 * 
 * Usage : -
 * val prof = ProfMan()
 * // do work here
 * println(prof.end("work"))
 * ->> callProf | work | 200 ms
 */
case class ProfMan(){
  var startTime = System.nanoTime();
  def end(name : String) : String = {    
    "callProf | %s |%d ms".format(name, (System.nanoTime() - startTime) / 1000000)
  }
}
