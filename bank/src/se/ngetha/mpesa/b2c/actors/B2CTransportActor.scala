/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package se.ngetha.mpesa.b2c.actors

import akka.actor.Actor
import akka.actor.Props
import akka.event.Logging
import java.io.InputStream
import java.io.OutputStream
import java.net.HttpURLConnection
import java.net.URL
import java.util.Scanner
import scala.util.Failure
import scala.util.Success
import scala.util.Try


class Prof(){
  var startTime = System.nanoTime();
  def end(name : String) : String = {    
    "callProf | %s |%d ms".format(name, (System.nanoTime() - startTime) / 1000000)
  }
}
/**
 * Failure Response Wrapper
 */
case class HTTPTransportFailure(msg : String, ex : Throwable)
/**
 * Success Response Wrapper
 */
case class HTTPTransportResponse(respBody : String, headers : Map[String, String])

object HTTPTransportActor{
  /**
   * Create Props for an actor of this type.
   * @param req An instance of the B2CRequest
   * @return a Props for creating this actor, which can then be further configured
   *         (e.g. calling `.withDispatcher()` on it)
   */
  def props(url : String, postBody : String) : Props = 
    Props(new HTTPTransportActor(url, postBody))
}

/**
 * This Actor performs the HTTP Network IO
 * TODO - Allow GET Support, Allow Header Support
 */
class HTTPTransportActor(url : String, postBody : String) extends Actor{
  // logger
  val log = Logging(context.system, this)
  
  // receive method
  def receive = {
    case "send" => {
        // perform the transport
        val either = doTransport()
        
        // send back the actual response
        val resp = either.isLeft match{
          case true => either.left.get
          case false => either.right.get
        }
        
        // send
        sender ! resp
      }
    case _ => log.info("Got unknown message")
  }
  
  /**
   * Perform the HTTP Transport call to the SOAP End Point
   * Will return either a failure object or the HTTP Response Body & Headers
   */
  def doTransport() : Either[HTTPTransportFailure, HTTPTransportResponse] = {
    
    log.info(s"Doing http transport to $url")
    val con = (new URL(url)).openConnection.asInstanceOf[HttpURLConnection]
    
    // set the connection configs
    con.setDoInput(true)
    con.setDoOutput(true)
    con.setUseCaches(false)
    con.setRequestProperty("SOAPAction","FOO")
    con.setRequestProperty("Content-Type", "text/xml")    
    con.setRequestProperty("Content-Length",postBody.length+"")
    
    // try write
    log.info("Attempting to write over Wire")   
    
    val prof = new Prof()
    Try(writeOverWire(con)) match {
      case Success(in) => {
          log.info("Read OK"); 
          log.info(prof.end("writeOverWire"));
          
          // now parse response
          Try(readOverWire(in, con))  match{
              case Success(resp) => {
                  log.info("Parsed response OK")
                  Right(new HTTPTransportResponse(resp._1, resp._2))
              }
              case Failure(ex) => {
                  log.error(s"!readResponse ${ex.getMessage}")
                  Left(new HTTPTransportFailure(s"Failure - ${ex.getMessage}", ex))
              }
          }
      }
      case Failure(ex) => {
          log.info(prof.end("writeOverWire"));
          log.error(s"!callNetwork -> ${ex.getMessage}"); 
          Left(new HTTPTransportFailure(s"Failure - ${ex.getMessage}", ex))
      }
    }    
  }
  
  /**
   * Here we perform all the TCP Work to send data over the wire
   * and return the 
   */
  private def writeOverWire(con : HttpURLConnection) : InputStream = {
    val prof = new Prof()
    log.info("callNetwork")
    val out = con.getOutputStream  
    
  
    log.info("Got it - Sending over Wire")
  
    out.write(postBody.getBytes("UTF-8"))
    out.flush
  
    log.info("Sent over Wire OK - Parsing Headers")
   
  
    val in =
      if (con.getResponseCode == 200)
        con.getInputStream
    else con.getErrorStream
    log.info(prof.end("callNetwork"))
    in
  }
  
  /**
   * Given the Input Stream, read over it and return the response body
   * and the response headers
   * @param in - the InputStream
   */
  private def readOverWire(in: InputStream, con: java.net.URLConnection) 
  : (String , Map[String, String]) = {
    log.info("readResponse")
    val prof = new Prof()
    // read the headers
    val headers = parseResponseHeaders(con, 1)  
    
    // debug headers
    for(header <- headers){
      log.info("%s -> %s".format(header._1, header._2))
    }
    
    // read the response body
    val respBody =  new String(slurp(in))
    
    log.info(prof.end("readResponse"))
        
    // return
    (respBody, headers)
  } 
  
  private def slurp(in: java.io.InputStream): Array[Byte] = {

    val out = new java.io.ByteArrayOutputStream()

    try {
      copyStream(in, out)
      out.toByteArray()
    } finally {
      out.close()
      in.close()
    }
  }
  
  private def copyStream(istream: InputStream, ostream: OutputStream): Unit = {

    var bytes = new Array[Byte](1024)
    var len = -1

    while ({ len = istream.read(bytes, 0, 1024); len != -1 })
      ostream.write(bytes, 0, len)
  }
  
  private def parseResponseHeaders(con: java.net.URLConnection, n: Int): Map[String, String] = {
    
    val key = con.getHeaderFieldKey(n)

    if (key == null)
      Map.empty
    else
      parseResponseHeaders(con, n + 1) ++ Map(key -> con.getHeaderField(key))
  }
  
 
}
