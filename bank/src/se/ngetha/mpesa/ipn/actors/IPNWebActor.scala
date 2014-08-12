/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package se.ngetha.mpesa.ipn.actors

import com.mongodb.casbah.Imports._
import org.apache.log4j.Logger
import scala.concurrent._
import scala.concurrent.duration._
import akka.event.Logging
import akka.pattern.ask
import akka.routing.RoundRobinRouter
import akka.util.Timeout
import akka.actor._
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import se.ngetha.mpesa.ipn.sinks.ProfMan
import se.ngetha.mpesa.ipn.sinks._
import spray.can.Http
import spray.can.server.Stats
import spray.routing.HttpService
import spray.routing.Route
import spray.util._
import spray.http._
import HttpMethods._
import MediaTypes._
import scala.language.postfixOps
import com.typesafe.config._
import ExecutionContext.Implicits.global

/**
 * Test URL
 * http://127.0.0.1:8080/ipn?id=2970&orig=MPESA&dest=254700733153&tstamp=2011-07-06+22%3A48%3A56.0&text=BM46ST941+Confirmed.+%0Aon+6%2F7%2F11+at+10%3A49+PM+%0AKsh8%2C723.00+received+from+RONALD+NDALO+254722291067.+%0AAccount+Number+5FML59-01+%0ANew+Utility+balance+is+Ksh6%2C375%2C223.00&customer_id=2&user=123&pass=123&routemethod_id=2&routemethod_name=HTTP&mpesa_code=BM46ST941&mpesa_acc=5FML59-01&mpesa_msisdn=254722291067&mpesa_trx_date=6%2F7%2F11&mpesa_trx_time=10%3A49+PM&mpesa_amt=8723.0&mpesa_sender=RONALD+NDALO
 * 
 */

/**
 * Class representing request variables from the IPN Service - all as Strings
 */
case class IPNRequest(id : String, orig : String, dest : String,
                      tstamp : String,text : String, user : String, 
                      pass : String, mpesa_code : String, 
                      mpesa_acc : String, mpesa_msisdn : String, 
                      mpesa_trx_date : String,
                      mpesa_trx_time : String, mpesa_amt : String, 
                      mpesa_sender : String){
  /**
   * A short human readable String
   */
  def toShortString() : String = {
    s"${mpesa_sender} of ${mpesa_msisdn} sent ${mpesa_amt} to ${mpesa_acc} with code ${mpesa_code} at ${mpesa_trx_date} ${mpesa_trx_time}"
  }
  
  /**
   * A short JSON string
   */
  def toShortJSONString() : String = {
    s"{id : ${id}, orig : ${orig}, dest : ${dest}, text : ${text}, ipn : short-json, data : {sender: ${mpesa_sender} , msisdn: ${mpesa_msisdn}, amt: ${mpesa_amt}, to: ${mpesa_acc}, code: ${mpesa_code}, time: ${mpesa_trx_date} ${mpesa_trx_time}}}"
  }
  
  /**
   * The full JSON
   */
  def toJSONString() : String = {
    s"{ipn : short-json, data : {sender: ${mpesa_sender} , msisdn: ${mpesa_msisdn}, amt: ${mpesa_amt}, to: ${mpesa_acc}, code: ${mpesa_code}, time: ${mpesa_trx_date} ${mpesa_trx_time}}}"
  }
  
  /**
   * Express as a mongo DB Object
   */
  def toMongoDBObject() : MongoDBObject = {
    // flatten
    val obj = MongoDBObject("sender" -> mpesa_sender, "msisdn" -> mpesa_msisdn,
                            "amt" -> mpesa_amt, "to" -> mpesa_acc, 
                            "code" -> mpesa_code, "time" -> mpesa_trx_date,
                            "date" -> mpesa_trx_time, "id" -> id,
                            "orig" -> orig, "dest" -> dest, 
                            "tstamp" -> tstamp, "text" -> text);
    obj
  }
}


trait IPNHttpService extends HttpService {    
  // logger
  val log = Logger.getLogger("IPNHttpService")
  
  // default error
  val defaultError = "FAIL|We could not process your request at this time"
  
  // config
  val conf = ConfigFactory.load()
  
//  //These implicit values allow us to use futures
//  //in this trait.
//  implicit def executionContext = actorRefFactory.dispatcher
//  
//  // worker
//  val str = "bar"
//  val props = IPNActor.props(str)
//  val worker = actorRefFactory.actorOf(props)  
//    
//  // router
//  val router1 = actorRefFactory.actorOf(props.withRouter(
//      RoundRobinRouter(nrOfInstances = 20)))
  
  // create the MongoDB Sink and initialize it  
  log.info("initMongoPool")
  val initPoolTry : Try[Unit] = Try(MongoDBPool.init(
                conf.getString("ipn-app.sinks.mongo.host"),
                Integer parseInt conf.getString("ipn-app.sinks.mongo.port")))

  // match
  val initPoolOk : Boolean = initPoolTry match {
    case Success(ok) => {
        log.info("initMongoPool - OK!")
        true
    }
    case Failure(fail) => {
        log.info(s"!initMongoPool - Fail ${fail}")
        false
    }
  }
  
  // create the SQS Sink and initialize it
  val sqsInitTry : Try[String] = 
    Try(conf.getString("ipn-app.sinks.sqs.queueUrl"))
  
  // tuple of the result  
  val initSQSRes = sqsInitTry match {
    case Success(url) => {
        log.info("initSQSPool - OK!")
        (true, url)
    }
    case Failure(fail) => {
        log.info(s"!initSQSPool - Fail ${fail}")
        (false, "na")
    }
  }
  

  /**
   * This method processes a given IPNReq
   * Processing is a 2 step process
   * 1. Writing to the IPNDB via MongoDBSink
   * 2. Queueing the request for upstream processing via SQSSink
   * 
   * Note that (2) is done Async and the reply is sent to the IPNService
   * after (1) based on success or failure
   */
  def processIPN(req : IPNRequest) : String = {
    // profiler
    val prof = ProfMan()
    
    //log
    log.info(s"procIpn ${req.mpesa_code}")
    log.info(IPNStateSubmitOK)
    
    // if we failed tog get queueURL we fail
    if(initSQSRes._1 == false){
      log.fatal("Cowardly refusing to process IPNRequest when SQSInit Failed. See error above")
      log.fatal(s"!procIpn ${req}")
      log.info(prof.end("procIpn"))
      return defaultError
    }
    
    
    // if we failed to init, we always fail
    initPoolTry match {
      case Failure(x) => {
          log.fatal("Cowardly refusing to process IPNRequest when MongoInit Failed. See error above")
          log.fatal(s"!procIpn ${req}")
          log.info(prof.end("procIpn"))
          return defaultError
      }
      case Success(x) => {}
    }
    
    // router1 ! req
    
    log.debug(s"Raw Req -> ${req}")
    
    // create the MongoSink
    val mongo = new MongoDBSink()
    
    // sink it
    log.info(s"doSink ${req.mpesa_code}")
    val sink = new SQSSink(initSQSRes._2)
    
    // sink to Mongo    
    val res = mongo.sink(req)
    
    // process the response
    val ipnResp : String = res.isLeft match {
      case true => {
          log.info("doSink -> Ok")
          s"Dear ${req.mpesa_sender}, your contribution of ${req.mpesa_amt} is received!"
      }
      case false => {
          log.info(s"!doSink -> Fail ${res.right}")
          s"Sorry ${req.mpesa_sender}, your contribution of ${req.mpesa_amt} could not be processed"
      }
    }
    
    // sink to SQS in the Future
    log.info(s"Sinking SQS in Future ${req.mpesa_code}")
    val sqsSinkRes : Future[Either[SinkOK, SinkFail]] = future{
      sink.sink(req)
    }
    
    // add on complete hooks
    sqsSinkRes.onComplete{
      case Success(either) => {
          log.info("Got response from SQSSink")
          either.isLeft match{
            case true => { 
               log.info(s"sinkSQS -> OK ${req.mpesa_code}")
               MongoDBPool.updateSQSSinkStatus(req, 
                                               IPNStateSubmitOK, 
                                               "ok")
            }
            case false => {
                log.fatal(s"!sinkSQS -> Failed ${req.mpesa_code} ${either.right}")
                MongoDBPool.updateSQSSinkStatus(req, 
                                                IPNStateSubmitFailed, 
                                                either.right toString)
            }
          }          
      }
      case Failure(ex) => {
          log.fatal(s"!sinksSQS -> Exception ${req.mpesa_code} ${ex}")
      }
    }
    
    // report time took
    log.info(prof.end("procIpn"))
    ipnResp    
  }

  // do some work
  def doWork(str : String) : String = {
    println(s"got str ${str}")    
    //router1 ! "receive"
    "doo"
  }
  
  // routes starting with tbe building blocks
  val getOrPost = get | post
  val search = path("search") & getOrPost  
  val ipn = path("ipn") & getOrPost & parameters('id, 'orig, 'dest, 'tstamp,
                                                 'text, 'user, 'pass, 
                                                 'mpesa_code, 'mpesa_acc,
                                                 'mpesa_msisdn, 'mpesa_trx_date,
                                                 'mpesa_trx_time, 'mpesa_amt,
                                                 'mpesa_sender)
  
  // construct the routing rules and processing block
  val ipnRoutes : Route = {
    search {
      parameter("q") { 
        query => 
        ctx => ctx.complete(doWork(query))       
      }
    }~
    ipn {
      (id, orig, dest, tstamp,
       text, user, pass, mpesa_code, mpesa_acc, mpesa_msisdn, mpesa_trx_date,
       mpesa_trx_time, mpesa_amt, mpesa_sender) => {
        // create the req
        val req = IPNRequest(id, orig, dest, tstamp,
                             text.filter(_ >= ' '), user, pass, mpesa_code, mpesa_acc, mpesa_msisdn, mpesa_trx_date,
                             mpesa_trx_time, mpesa_amt, mpesa_sender)
        ctx => ctx.complete(processIPN(req))
      }          
    }
  }
}

/**
 * IPN Actor receives notifications from IPN Service
 */
class IPNWebActor extends Actor with IPNHttpService{
  def actorRefFactory = context
  def receive = {
    //log.info("processing")
    runRoute(ipnRoutes)
  }  
}