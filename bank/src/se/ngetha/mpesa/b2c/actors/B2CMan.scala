
package se.ngetha.mpesa.b2c.actors

import akka.actor.Actor
import akka.actor.Props
import akka.event.Logging
import se.ngetha.mpesa.b2c.xml._

/*
 * This actor is the B2C actor, it receives a request to send
 * a certain amount of money to a certain customer and it will
 * create children to send it and update the ledger (db)
 */

trait B2CMPesaRequest;
  
case class B2CRequest(to : String, amount : Double, from : String,
                      transId : String) extends B2CMPesaRequest

object B2CActor{
  /**
   * Create Props for an actor of this type.
   * @param req An instance of the B2CRequest
   * @return a Props for creating this actor, which can then be further configured
   *         (e.g. calling `.withDispatcher()` on it)
   */
  def props(req : B2CRequest) : Props = Props(new B2CActor(req))
}

class B2CActor(req : B2CRequest) extends Actor{
  // logger
  val log = Logging(context.system, this)
  
  // receive method
  def receive = {
    case "send" => sendMoney()
    case se.ngetha.mpesa.b2c.actors.HTTPTransportFailure(f,g) => processFailure(f,g)
    case HTTPTransportResponse(body, headers) => processSuccess(body, headers)
    case _ => log.info("Got unknown message")
  }
  
  def sendMoney() : String = {
    log.info("State - READY")
    log.info(s"Will send $req")
    
    // build the request
    val soapBody = this.soapForRequest().toXml()
    log.info("State - MARSHAL")
    
    // get the URL endpoint
    //val url = "http://192.168.3.215:7777/mpesa/RequestMgrService"
    val url = "http://www.google.com"
    
    // start the child actor
    val transport = context.actorOf(HTTPTransportActor.props(url, soapBody),
                                    name = "soap-tran")
    // send a msg to it
    log.info("State - SENDING")
    transport ! "send"
    log.info("State - SENT-WAIT")
    // wait for the response
    "sent"
  }
  
  /**
   * Respond to Failure
   */
  def processFailure(msg : String, ex : Throwable) : String = {
    log error (s"!callMPesa {msg: ${msg}}")
    "fail"
  }
  
  /**
   * Respond to Success - Note this is a success network call - not biz logic
   */
  private def processSuccess(respBody : String, headers : Map[String, String]) : String = {
    log info (s"callMpesa {msg: ${respBody}")
    "sent"
  }
  
  /**
   * Build a SOAP Request for this request
   */
  private def soapForRequest() : SOAPRequest = {
    val params = new Parameters(List[Parameter]
                                (new Parameter("K", "V"), new Parameter("X", "C")))                                
    val refData = new ReferenceData(List[ReferenceItem]
                                    (new ReferenceItem("K", "V"), new ReferenceItem("X", "C")))
    
    val tx = new Transaction("commandId : String", "langCode : String", "originConvId : String",
                             "convId : String", "remark : String"," encParams : String", params ,
                             refData );
    val caller = new Caller("0", "ThirdPartyID0", "Password0", "CheckSum0", "ResultURL0");
    val initiator = new Initiator("0", "ThirdPartyID0", "Password0", "CheckSum0");
    val pri = new PrimartyParty("0", "ThirdPartyID0", "458")
    val seco = new ReceiverParty("0", "ThirdPartyID0", "458")
    val ad = new AccessDevice("foo", "bar")
    val iden = new Identity(caller, initiator, pri, seco, ad);
    val req = new MPesaRequest("PRIV_KEY_SHA256", tx, iden);
    val soap = new SOAPRequest("1", "pwd", "sid", req)
    log.info("SOAP Body -> ".concat(soap.toXml()))
    soap
  }
}