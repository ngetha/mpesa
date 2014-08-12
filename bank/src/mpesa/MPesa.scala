package mpesa


import se.ngetha.mpesa.b2c.actors.B2CActor
import se.ngetha.mpesa.b2c.actors.B2CRequest
import se.ngetha.mpesa.b2c.actors.B2CRequest
import se.ngetha.mpesa.b2c.actors.B2CRequest
import se.ngetha.mpesa.ipn.actors.IPNWebActor
import se.ngetha.mpesa.b2c.actors.WebServiceActor
import se.ngetha.mpesa.b2c.xml._
import akka.actor._
import akka.io.IO
import spray.can.Http


object MPesa {
 
  def main(args : Array[String]): Unit = {
    
    implicit val system = ActorSystem("mpesa")
   
    // the handler actor replies to incoming HttpRequests
    //val handler = system.actorOf(Props[IPNWebActor], name = "handler")
    //IO(Http) ! Http.Bind(handler, interface = "localhost", port = 8080)
  
    val reqA = B2CRequest("1",4.5,"ken","34")
    val props = B2CActor.props(reqA)
    val remoteActor = system.actorOf(props, name = "b2c1")
    val respA = remoteActor ! "send"
    println(s"Response from actors -> $respA")
    
    //println(data);
//    val params = new Parameters(List[Parameter](new Parameter("K", "V"), new Parameter("X", "C")))
//                                
//    val refData = new ReferenceData(List[ReferenceItem](new ReferenceItem("K", "V"), new ReferenceItem("X", "C")))
//    
//    val tx = new Transaction("commandId : String", "langCode : String", "originConvId : String",
//                             "convId : String", "remark : String"," encParams : String", params ,
//                             refData );
//    val caller = new Caller("0", "ThirdPartyID0", "Password0", "CheckSum0", "ResultURL0");
//    val initiator = new Initiator("0", "ThirdPartyID0", "Password0", "CheckSum0");
//    val pri = new PrimartyParty("0", "ThirdPartyID0", "458")
//    val seco = new ReceiverParty("0", "ThirdPartyID0", "458")
//    val ad = new AccessDevice("foo", "bar")
//    val iden = new Identity(caller, initiator, pri, seco, ad);
//    val req = new MPesaRequest("PRIV_KEY_SHA256", tx, iden);
//    val soap = new SOAPRequest("1", "pwd", "sid", req)
//    val resp1 = new MPesaResponse("00000001","ok")
//    //println(soap toXml)
//    //var xstr = "<![CDATA[<?xml version=\"1.0\" encoding=\"UTF-8\"?><response xmlns=\"http://api-v1.gen.mm.vodafone.com/mminterface/response\"><ResponseCode>ResponseCode0</ResponseCode><ResponseDesc>ResponseDesc0</ResponseDesc><ConversationID>ewrer</ConversationID><OriginatorConversationID>werwer423</OriginatorConversationID><ServiceStatus>0</ServiceStatus></response>]]>"
//    var xstr = "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:req=\"http://api-v1.gen.mm.vodafone.com/mminterface/request\"><soapenv:Header/><soapenv:Body><req:ResponseMsg><![CDATA[<?xml version=\"1.0\" encoding=\"UTF-8\"?><response xmlns=\"http://api-v1.gen.mm.vodafone.com/mminterface/response\"><ResponseCode>ResponseCode0</ResponseCode><ResponseDesc>ResponseDesc0</ResponseDesc><ConversationID>ewrer</ConversationID><OriginatorConversationID>werwer423</OriginatorConversationID><ServiceStatus>0</ServiceStatus></response>]]></req:ResponseMsg></soapenv:Body></soapenv:Envelope>"
//    var xstr2 = "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:res=\"http://api-v1.gen.mm.vodafone.com/mminterface/result\"><soapenv:Header/><soapenv:Body><res:ResultMsg><![CDATA[<?xml version=\"1.0\" encoding=\"UTF-8\"?><result xmlns=\"http://api-v1.gen.mm.vodafone.com/mminterface/result\"><ResultType>0</ResultType><ResultCode>ResultCode</ResultCode><ResultDesc>ResultDesc0</ResultDesc><OriginatorConversationID>OriginatorConversationID0</OriginatorConversationID><ConversationID>ConversationID0</ConversationID><TransactionID>TransactionID0</TransactionID><ResultParameters><ResultParameter><Key>Key0</Key><Value>Value0</Value></ResultParameter><ResultParameter><Key>Key1</Key><Value>Value1</Value></ResultParameter></ResultParameters><ReferenceData><ReferenceItem><Key>Key2</Key><Value>Value2</Value></ReferenceItem><ReferenceItem><Key>Key3</Key><Value>Value3</Value></ReferenceItem></ReferenceData></result>]]></res:ResultMsg></soapenv:Body></soapenv:Envelope>"
//    val resp = new SOAPResponseParser(xstr2)
//    //println(resp.parse())
//    //println(resp1.toSOAPSReq())
//    println("ok")
  }
}

