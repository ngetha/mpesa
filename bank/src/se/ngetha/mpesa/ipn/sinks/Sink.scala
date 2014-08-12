package se.ngetha.mpesa.ipn.sinks

import com.typesafe.config.Config
import org.apache.log4j.Logger
import scala.util.{Try, Success, Failure}
import se.ngetha.mpesa.ipn.actors.IPNRequest
import com.amazonaws.regions.Region
import com.amazonaws.regions.Regions
import com.amazonaws.services.sqs.AmazonSQS
import com.amazonaws.services.sqs.AmazonSQSClient
import com.mongodb.casbah.Imports._
import scala.collection.JavaConverters._

/**
 * Responses to sink
 */
sealed trait SinkResponse
case class SinkOK() extends SinkResponse
case class SinkFail(msg : String, fault : Throwable) extends SinkResponse

/**
 * States of IPN
 * 1. In the MongoDB collection -> Received
 * 2. Submitted to SQS and failed -> SubmitFailed
 * 3. Submitted to SQS and inserted to Q -> SubmitOK
 */
sealed trait IPNProcessingState
case object IPNStateReceived extends IPNProcessingState
case object IPNStateSubmitFailed extends IPNProcessingState
case object IPNStateSubmitOK extends IPNProcessingState


/**
 * The template for all sinks - they accept a config and a way to sink data
 * 
 * A Sink is an output of the IPNRequest - it could be a call to a HTTP Service,
 * a Queue, a ZeroMQ Socket, a DB e.t.c
 * 
 */
trait IPNSink{
  /**
   * Sink the request into the sink
   * @param req - the IPNRequest to be sank
   * @return - Either SinkOK or SinkFail instance
   */
  def sink(req : IPNRequest) : Either[SinkOK, SinkFail] 
}

/**
 * MongoDBSink implementation
 * Takes IPNRequests and writes them to the mongo db collection
 */
class MongoDBSink() extends IPNSink{ 
  /**
   * We take the IPNRequest and insert it into the MongoDB Collection
   * The structure of the document is like so
   * { sank-at : timestamp, sank-by : ipnsd, ....<ipn json elements here >...]
   * @param req - the IPN Request
   */
  def sink(req : IPNRequest) : Either[SinkOK, SinkFail]  = {
    // logger
    val log = Logger.getLogger("MongoSink")
    val prof = ProfMan()
    
    // get the collection
    log.info(s"Sinking IPNReq down the mongo drain ${req.mpesa_code}/${req.mpesa_sender}/${req.mpesa_msisdn}/${req.mpesa_amt}")
    log.debug(s"Sinking INPReq - Full - ${req}")
    log.debug("Getting con from pool")
    val col = MongoDBPool.db("api")("ipn")
    
    // get the IPNReq as a MongoDBObject
    val ipnDoc = req.toMongoDBObject
    log.debug(ipnDoc)
    
    // add the insert timestamp
    val timestamp: String = (System.currentTimeMillis / 1000) + ""
    ipnDoc += "sink_time" -> timestamp
    
    // add state
    ipnDoc += "sank_status" -> IPNStateReceived.toString
    ipnDoc += "sank_status_msg" -> "init"
    
    // Try insert
    log.debug("Sinking!")
    val res = Try(col.insert(ipnDoc))
    
    // process the result
    res match{
      case Success(v) => {
          log.info(s"sinkIPN ${req.mpesa_code}")
          log.info(prof.end("sinkIPN"))
          Left(SinkOK())
      }
      case Failure(ex) => {
          log.fatal(s"!sinkIPN raw ${req}")
          log.fatal(s"!sinkIPN reason ${ex}")
          log.info(prof.end("sinkIPN"))
          Right(SinkFail("Insert Error", ex))
      }
    }       
  }
}

/**
 * Singleton class holding the MongoDBClient
 */
object MongoDBPool{
  private var mongoClient : MongoClient = null;
  
  private val log = Logger.getLogger("MongoDBPool")
  
  def init(host : String, port : Int) = {
    log.info(s"Init MongoPool mongo://${host}:${port}")
    assert ( port > 0, {log.fatal("Port must be greater than 0")})
    assert ( host != null, {log.fatal("Host must be a valid host")})
    mongoClient = MongoClient(host, port)
  }
  
  def db(db : String) = {
    log.debug(s"Looking for db with name ${db}")
    mongoClient(db)
  }
  
  /**
   * This method is called after an attempt has been made to insert
   * a request to SQS to update its status.
   * 
   * The status is useful for reconciliation.
   * 
   * @param req the IPNRequest instance
   * @param newState the new state
   * @param msg - any message associated with the new state
   */
  def updateSQSSinkStatus(req : IPNRequest, newState : IPNProcessingState, 
                          msg : String) : Either[SinkOK, SinkFail] = {
    val prof = ProfMan()
    val col = MongoDBPool.db("api")("ipn")
    
    // get the IPNReq as a MongoDBObject
    val query = MongoDBObject("code" -> req.mpesa_code)    
    log.debug(query)
    
    // add the insert timestamp
    val timestamp: String = (System.currentTimeMillis / 1000) + ""    
    
    // add state
    // states (mongo_sank -> sqs_sank)
    val updCols = MongoDBObject("$set" -> MongoDBObject(
                                "sank_status" -> newState.toString, 
                                "sank_status_msg" -> msg,
                                "sqs_time" -> timestamp))
    val updateQry = s"{q=> ${query}, upd=> ${updCols}}"
    log.info(s"Updating status with -> ${updateQry}")
   
    // Try insert
    log.debug("Updating Sank!")
    val res = Try(col.findAndModify(query, updCols))
    
    // process the result
    res match{
      case Success(v) => {
          log.info(s"postSinkIPN ${req.mpesa_code}")
          log.info(prof.end("postSinkIPN"))
          Left(SinkOK())
      }
      case Failure(ex) => {
          log.fatal(s"!postSinkIPN raw ${req}")
          log.fatal(s"!postSinkIPN reason ${ex}")
          log.info(prof.end("postSinkIPN"))
          Right(SinkFail("Update Error", ex))
      }
    }       
  }
}

/**
 * This method enqueues IPN Requests on the Amazon AWS SQS
 * The Queue name is specified in the config under ipn-app.sinks.sqs.queueURL
 */
class SQSSink(queueUrl : String) extends IPNSink{
  /**
   * We take the IPNRequest and insert it into the SQS Queue
   * The structure of the document is like so
   * { sank-at : timestamp, sank-by : ipnsd, ....<ipn json elements here >...]
   * @param req - the IPN Request
   */
  def sink(req : IPNRequest) : Either[SinkOK, SinkFail]  = {
    // logger
    val log = Logger.getLogger("SQSSink")
    val prof = ProfMan()
    
    // get the collection
    log.info(s"Sinking IPNReq down the SQS drain ${req.mpesa_code}/${req.mpesa_sender}/${req.mpesa_msisdn}/${req.mpesa_amt}")
    log.debug(s"Sinking INPReq - Full - ${req}")
    val sqs = new AmazonSQSClient()
    
    // get queue and region
    //val region = Region.getRegion(Regions.EU_WEST_1);
    //val queueUrl = "https://sqs.eu-west-1.amazonaws.com/649117426437/ipn"
    //sqs.setRegion(region);
    log.debug(s"Q -> ${queueUrl}")    
    
    // get the IPNReq as a MongoDBObject
    val ipnDoc = req.toMongoDBObject
    log.debug(ipnDoc)
    
    // add the insert timestamp
    val timestamp: String = (System.currentTimeMillis / 1000) + ""
    ipnDoc += "sink_time" -> timestamp
    
    // Try insert
    log.debug("Sinking!")
    
    // list queues for now
    val json = ipnDoc.toString
    val res = Try(sqs.sendMessage(queueUrl, json))
      
    // process the result
    res match{
      case Success(v) => {
          log.info(s"sinkIPNSQS ${req.mpesa_code}")
          log.info(prof.end("sinkIPNSQS"))
          
          // now we must update the status in the local db
          // and flag the transaction as sank by sqs successfully
          Left(SinkOK())
      }
      case Failure(ex) => {
          log.fatal(s"!sinkIPNSQS raw ${req}")
          log.fatal(s"!sinkIPNSQS reason ${ex}")
          log.info(prof.end("sinkIPNSQS"))
          Right(SinkFail("Q Insert Error", ex))
      }
    } 
  }
}