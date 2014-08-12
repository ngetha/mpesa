/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package bank

import scala.language.postfixOps
import scala.util.Try
import scala.xml.Elem
import scala.xml.Node
import scala.util.Try
import scala.collection.mutable.ListBuffer

/**
 * Parent class of the nodess
 */
trait MPesaNode{
  def addChild(n: Node, newChild: Node) = n match {
    case Elem(prefix, label, attribs, scope, child @ _*) =>
      Elem(prefix, label, attribs, scope, child ++ newChild : _*)
    case _ => sys.error("Can only add children to elements!")
  }
  
  def toXml() : Elem
  //override def toString() = toXmlStr()
  def toXmlStr() = toXml() toString()
}

/**
 * Handy class to create leaf nodes
 * new Leaf("foo","bar") yields scala.xml.Elem -> <foo>bar</foo>
 */
class Leaf(tag : String, content : String) 
extends MPesaNode{
  def toXml() : Elem = <xml>{content}</xml>.copy(label = tag)
}

/**
 * A lot of elements are simply <Key/><Value/> wrapped around
 * a tag with a diff name e.g <ReferenceItem/>, <ParameterType/> e.t.c
 * So we create a simple Enum type to simplify our work and avoid
 * repetition
 */
object KVType extends Enumeration {
  type KVType = Value
  val ReferenceItemType, ParameterType, ResultParameterType = Value
}

import KVType._

/**
 * Handy class forming the template of the KV types
 */
class KVWrapper(kvt : KVType, key : String, value : String) 
extends MPesaNode{
  def toXml() : Elem = {
    // add the children
    var doc = kvt match { 
      case ReferenceItemType => <ReferenceItem/> 
      case ParameterType => <Parameter/>
      case ResultParameterType => <ResultParameter/>
    }
      
    // add the stand alone
    doc = this.addChild(doc, (<Key>{key}</Key>))
    doc = this.addChild(doc, (<Value>{value}</Value>))
      
    // yield ze doc
    doc
  } 
  
  override def toString() : String = { " {%s -> %s} " format (key,value)}
}

/**
 * Handy class forming the template of the KV collection types
 */
class KVCollectionWrapper(kvt : KVType, kvColl : Seq[KVWrapper]) 
extends MPesaNode{
    
  def toXml() : Elem = {
    // add the children
    var doc = kvt match { 
      case ReferenceItemType => {<ReferenceData>{ for(kv <- kvColl) yield kv toXml() }</ReferenceData> }
      case ParameterType => {<Parameters>{ for(kv <- kvColl) yield kv toXml() }</Parameters>}
      case ResultParameterType => {<ResultParameters>{ for(kv <- kvColl) yield kv toXml() }</ResultParameters>}
    }    
    
    // yield ze doc
    doc
  } 
  
  override def toString() : String = {
    var str = (kvt toString) concat " => ";  
    kvColl.map{ kv => str += (kv toString)}; 
    str 
  }
}

/**
 * Parameter Class 
 * @see WSDL Definition
 */
class Parameter(key : String, value : String ) 
extends KVWrapper(ParameterType, key, value)

/**
 * Parameter Class 
 * @see WSDL Definition
 */
class ReferenceItem(key : String, value : String ) 
extends KVWrapper(ReferenceItemType, key, value)

/**
 * ResultParameter Class 
 * @see WSDL Definition
 */
case class ResultParameter(key : String, value : String) 
extends KVWrapper(ResultParameterType, key, value)

/**
 * Parameter Class 
 * @see WSDL Definition
 */
class AccessDevice(idType : String, id : String ) 
extends MPesaNode{
  def toXml() : Elem = <AccessDevice><IdentifierType>{ idType }</IdentifierType><Identifier>{ id }</Identifier></AccessDevice>;    
}

/**
 * Collection of parameters
 * @see WSDL Definition
 */
class Parameters(params : Seq[Parameter]) 
extends KVCollectionWrapper(ParameterType, params)


/**
 * Collection of ReferenceItem
 * @see WSDL Definition
 */
class ReferenceData(items : Seq[ReferenceItem]) 
extends KVCollectionWrapper(ReferenceItemType, items){
  //def toXml() : Elem = <RefernceData>{ for(item <- items) yield item toXml() }</RefernceData>  
}

/**
 * Collection of ResultParameters
 * @see WSDL Definition
 */
class ResultParameters(items : Seq[ResultParameter]) 
extends KVCollectionWrapper(ResultParameterType, items){
  //def toXml() : Elem = <RefernceData>{ for(item <- items) yield item toXml() }</RefernceData>  
}

/**
 * Collection of Transaction
 * @see WSDL Definition
 */
class Transaction(commandId : String, langCode : String, originConvId : String,
                  convId : String, remark : String, encParams : String, params : Parameters,
                  refData : ReferenceData) extends MPesaNode{
  def toXml() : Elem = {
    // add the children
    var doc = <Transaction/>
    // add the stand alone
    doc = this.addChild(doc, ((new Leaf("CommandID", commandId)) toXml) head)
    doc = this.addChild(doc, ((new Leaf("LanguageCode", langCode)) toXml) head)
    doc = this.addChild(doc, ((new Leaf("OriginatorConversationID", originConvId)) toXml) head)
    doc = this.addChild(doc, ((new Leaf("ConversationID", convId)) toXml) head)
    doc = this.addChild(doc, ((new Leaf("Remark", remark)) toXml) head)
    doc = this.addChild(doc, ((new Leaf("EncryptedParameters", encParams)) toXml) head)
    // add the refData
    doc = this.addChild(doc, (refData toXml()) head)
    // add the params
    doc = this.addChild(doc, (params toXml()) head)
    // add the timestamp
    val timestamp: String = String.valueOf(System.currentTimeMillis / 1000)
    doc = this.addChild(doc, ((new Leaf("Timestamp", timestamp)) toXml) head)
      
    // yield ze doc
    doc
  }   
}

/**
 * Collection of Transaction
 * @see WSDL Definition
 */
class Identity(caller : Caller, initiator : Initiator, pparty : PrimartyParty,
               rparty : ReceiverParty, accessDev : AccessDevice) extends MPesaNode{
  def toXml() : Elem = {
    // add the children
    var doc = <Identity/>
    // add the stand alone
    doc = this.addChild(doc, (caller toXml) head)
    doc = this.addChild(doc, (initiator toXml) head)
    doc = this.addChild(doc, (pparty toXml) head)
    doc = this.addChild(doc, (rparty toXml) head)
    doc = this.addChild(doc, (accessDev toXml) head)
    
    // yield ze doc
    doc
  }
   
}

/**
 * Caller
 * @see WSDL Definition
 */
class Caller(callerType : String, thirdPId : String, pwd : String,
             checkSum : String, resultURL : String) extends MPesaNode{
  def toXml() : Elem = {
    // add the children
    var doc = <Caller/>
    // add the stand alone
    doc = this.addChild(doc, ((new Leaf("CallerTypeID", callerType)) toXml) head)
    doc = this.addChild(doc, ((new Leaf("ThirdPartyID", thirdPId)) toXml) head)
    doc = this.addChild(doc, ((new Leaf("Password", pwd)) toXml) head)
    doc = this.addChild(doc, ((new Leaf("CheckSum", checkSum)) toXml) head)
    doc = this.addChild(doc, ((new Leaf("ResultURL", resultURL)) toXml) head)
      
    // yield ze doc
    doc
  }   
}

/**
 * PrimartyType and ReceiverType are basically the same
 * objects, diff tag - so to not repeat ourselves we use this
 */
object PartyType extends Enumeration {
  type PartyType = Value
  val PrimartyParty, ReceiverParty, InitiatorParty = Value
}

import PartyType._

/**
 * Initiator
 * @see WSDL Definition
 */
class Initiator(idType : String, id : String, sec : String,
                sc : String) extends MPesaNode{
  def toXml() : Elem = {
    // create a new party
    var doc = (new Party(InitiatorParty, idType, id, sc) toXml)
    
    // add security credential   
    doc = this.addChild(doc head, ((new Leaf("SecurityCredential", sec)) toXml) head)
        
    // yield ze doc
    doc
  }   
}
    
/**
 * Party - handy class wrapper over PrimartyParty  ReceiverParty  Initiator
 * @see WSDL Definition
 */
class Party(partyType : PartyType, idType : String, id : String,
            sc : String) extends MPesaNode{
  def toXml() : Elem = {
    // add the children
    var doc = partyType match { 
      case PrimartyParty => <PrimartyParty/> 
      case ReceiverParty => <ReceiverParty/>
      case InitiatorParty => <Initiator/>
    }
      
    // add the stand alone
    doc = this.addChild(doc, ((new Leaf("IdentifierType", idType)) toXml) head)
    doc = this.addChild(doc, ((new Leaf("Identifier", id)) toXml) head)     
    doc = this.addChild(doc, ((new Leaf("ShortCode", sc)) toXml) head)
      
    // yield ze doc
    doc
  } 
}

/**
 * The ReceiverParty class
 */
class ReceiverParty(idType : String, id : String,sc : String) 
extends Party(ReceiverParty, idType, id, sc)

/**
 * The PrimartyParty class
 * @see WSDL Def
 */
class PrimartyParty(idType : String, id : String,sc : String) 
extends Party(PrimartyParty, idType, id, sc)

/**
 * The MPesaRequest
 * @see WSDL Def - Section 5.1.1
 */
class MPesaRequest(keyOwner : String, trans : Transaction, iden : Identity) 
extends MPesaNode{
  def toXml() : Elem = {
    // add the children
    var doc = <request xmlns="http://api-v1.gen.mm.vodafone.com/mminterface/request"/>
    // add the stand alone
    doc = this.addChild(doc, (trans toXml) head)
    doc = this.addChild(doc, (iden toXml) head)
    doc = this.addChild(doc, ((new Leaf("KeyOwner", keyOwner)) toXml) head)
      
    // yield ze doc
    doc
  }   
}

/**
 * The SoapRequest
 * @see WSDL Def - Section 5.1.1
 */
class SOAPRequest(spId : String, spPassword : String, serviceId : String,
                  mpesaReq : MPesaRequest){
  def toXml () : String = {
    // the cdata
    val xml = (mpesaReq toXml) toString
    
    // build the cdata
    val cdata = "<![CDATA[<?xml version=\"1.0\" encoding=\"UTF-8\"?>%s]]>" format xml  
    
    // the soap request
    var doc = <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
      xmlns:req="http://api-v1.gen.mm.vodafone.com/mminterface/request">
      <soapenv:Header>
        <tns:RequestSOAPHeader xmlns:tns="http://www.huawei.com.cn/schema/common/v2_1">
          <tns:spId>{spId}</tns:spId>
          <tns:spPassword>{spPassword}</tns:spPassword>
          <tns:serviceId>{serviceId}</tns:serviceId>
          <tns:timeStamp>{String.valueOf(System.currentTimeMillis / 1000)}</tns:timeStamp>
        </tns:RequestSOAPHeader>
      </soapenv:Header>
      <soapenv:Body>
        <req:RequestMsg>
          %s
        </req:RequestMsg>
      </soapenv:Body>
              </soapenv:Envelope>
    
    // yield ze doc
    (doc toString) format(cdata)
  }
}

/**
 * Simple trait representing the parsed SOAPResponse
 */
trait SOAPResponse extends MPesaNode

/**
 * MPesaResponse - Response from Broker from Us
 * @see WSDL  Section 5.1.2
 */
case class MPesaResponse(respCode : String, respDesc : String, convId : String, 
                         originConvId : String, svcStatus : String) extends SOAPResponse{
  /**
   * Constructor to just build the response to the Broker
   */
  def this(respCode : String, respDesc : String) = {
    this(respCode, respDesc, null, null, null)
  }
    
  /**
   * convert toXML
   */
  def toXml() : Elem = {
    // root
    var doc = <response xmlns="http://api-v1.gen.mm.vodafone.com/mminterface/response"/>
      
    // we add whatever is not null
    if(respCode != null){
      doc = this.addChild(doc, ((new Leaf("ResponseCode", respCode)) toXml) head)
    }
      
    if(respDesc != null){
      doc = this.addChild(doc, ((new Leaf("ResponseDesc", respDesc)) toXml) head)
    }
      
    doc
  }
    
  /**
   * Convert to the SOAP Response object
   * @see WSDL Doc Section 5.2.2
   */
  def toSOAPSReq() : String = {
    // first get the root element
    val xmlStr : String = toXmlStr()    
    
    // build the cdata
    val cdata = "<![CDATA[<?xml version=\"1.0\" encoding=\"UTF-8\"?>%s]]>" format xmlStr
    
    // the soap request
    var doc = <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
      xmlns:req="http://api-v1.gen.mm.vodafone.com/mminterface/request">
      <soapenv:Header/>                   
      <soapenv:Body>
        <req:ResponseMsg>
          %s
        </req:ResponseMsg>
      </soapenv:Body>
              </soapenv:Envelope>
    
    // yield ze doc   
    (doc toString) format(cdata)
  }
}

/**
 * MPesaResult - Broker -> Us from Core API
 * @see WSDL Section 5.2.1
 */
case class MPesaResult(resultType : String, resultCode : String, resultDesc: String,
                       originConvId : String, convId : String, transId: String, 
                       resultParams : ResultParameters, refData : ReferenceData) extends SOAPResponse{
  def toXml() : Elem = {
    <response/>
  }
}

/**
 * ParseFault Class
 */
class ResponseParseFault(msg : String)

/**
 * The APIResponse parser
 * @see WSDL Doc
 */
class SOAPResponseParser(xmlIn : String){    
  /**
   * Parse the xml
   */
  def parse() : Either[SOAPResponse, ResponseParseFault] = {
    // first we get the first element in the body and get its response
    val rdoc : Try[Node] = Try(xml.XML loadString xmlIn)
    if(!rdoc.isSuccess){
      return Right(new ResponseParseFault((rdoc get) toString))
    }
      
    // get the str
    //val xmlInStrX : String = (((rdoc get) \\ "Body") head) text
    //println(rdoc get)
    //println((((rdoc get) \\ "Body") text) )
      
    // load the doc
    // The xml is the literal in between the CDATA      
    //val xmlInStr = (xmlIn trim) substring("<![CDATA[" length(), (xmlIn length()) - ("]]>" length()))
    val xmlInStr = (((rdoc get) \\ "Body") text)
    val doc : Try[Node] = Try(xml.XML loadString xmlInStr)
      
    // get the doc and work it
    if(!doc.isSuccess){
      println("Failed to parse")
      println(doc get)
      Right(new ResponseParseFault((doc get) toString))
    }
    else{
      // apply vars
      println("Parse OK")
        
      // get the root node
      val root = doc get
        
      // get the name of the root node
      val name = root label
        
      if(name equalsIgnoreCase "response"){
        // MPesa Response       
        val mp = 
          new MPesaResponse((root \\ "ResponseCode") text,
                            (root \\ "ResponseDesc") text, 
                            (root \\ "ConversationID") text, 
                            (root \\ "OriginatorConversationID") text, 
                            (root \\ "ServiceStatus") text)
        println(mp)
        Left(mp)
      }
      else if(name equalsIgnoreCase "result"){
        // MPesa Result
          
        // get the ResultParams
        val resultParams = root \\ "ResultParameters" \ "ResultParameter"
        val resultParamsSq = new scala.collection.mutable.ListBuffer[ResultParameter]
          
        // add to list
        for(node <- resultParams){
          resultParamsSq += new ResultParameter((node \\ "Key") text, (node \\ "Value") text)            
        }
          
        // get the ReferenceData
        val refData = root \\ "ReferenceData" \ "ReferenceItem"
        val refDataSq = new ListBuffer[ReferenceItem]
          
        // add to list
        for(node <- refData){
          refDataSq += new ReferenceItem((node \\ "Key") text, (node \\ "Value") text)            
        }
          
        val mp = 
          new MPesaResult((root \\ "ResultType") text,
                          (root \\ "ResultCode") text, 
                          (root \\ "ResultDesc") text, 
                          (root \\ "OriginatorConversationID") text, 
                          (root \\ "ConversationID") text,
                          (root \\ "TransactionID") text,
                          new ResultParameters(resultParamsSq), 
                          new ReferenceData(refDataSq))
        println(mp)
        Left(mp)
      }
      else{
        Right(new ResponseParseFault("Unknown SOAP Response type -> $name"))
      }
        
    }
      
      
        
  }
}

/**
 * 
 */
object Main {
  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
    //println(data);
    val params = new Parameters(List[Parameter](new Parameter("K", "V"), new Parameter("X", "C")))
                                
    val refData = new ReferenceData(List[ReferenceItem](new ReferenceItem("K", "V"), new ReferenceItem("X", "C")))
    
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
    val resp1 = new MPesaResponse("00000001","ok")
    //println(soap toXml)
    //var xstr = "<![CDATA[<?xml version=\"1.0\" encoding=\"UTF-8\"?><response xmlns=\"http://api-v1.gen.mm.vodafone.com/mminterface/response\"><ResponseCode>ResponseCode0</ResponseCode><ResponseDesc>ResponseDesc0</ResponseDesc><ConversationID>ewrer</ConversationID><OriginatorConversationID>werwer423</OriginatorConversationID><ServiceStatus>0</ServiceStatus></response>]]>"
    var xstr = "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:req=\"http://api-v1.gen.mm.vodafone.com/mminterface/request\"><soapenv:Header/><soapenv:Body><req:ResponseMsg><![CDATA[<?xml version=\"1.0\" encoding=\"UTF-8\"?><response xmlns=\"http://api-v1.gen.mm.vodafone.com/mminterface/response\"><ResponseCode>ResponseCode0</ResponseCode><ResponseDesc>ResponseDesc0</ResponseDesc><ConversationID>ewrer</ConversationID><OriginatorConversationID>werwer423</OriginatorConversationID><ServiceStatus>0</ServiceStatus></response>]]></req:ResponseMsg></soapenv:Body></soapenv:Envelope>"
    var xstr2 = "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:res=\"http://api-v1.gen.mm.vodafone.com/mminterface/result\"><soapenv:Header/><soapenv:Body><res:ResultMsg><![CDATA[<?xml version=\"1.0\" encoding=\"UTF-8\"?><result xmlns=\"http://api-v1.gen.mm.vodafone.com/mminterface/result\"><ResultType>0</ResultType><ResultCode>ResultCode</ResultCode><ResultDesc>ResultDesc0</ResultDesc><OriginatorConversationID>OriginatorConversationID0</OriginatorConversationID><ConversationID>ConversationID0</ConversationID><TransactionID>TransactionID0</TransactionID><ResultParameters><ResultParameter><Key>Key0</Key><Value>Value0</Value></ResultParameter><ResultParameter><Key>Key1</Key><Value>Value1</Value></ResultParameter></ResultParameters><ReferenceData><ReferenceItem><Key>Key2</Key><Value>Value2</Value></ReferenceItem><ReferenceItem><Key>Key3</Key><Value>Value3</Value></ReferenceItem></ReferenceData></result>]]></res:ResultMsg></soapenv:Body></soapenv:Envelope>"
    val resp = new SOAPResponseParser(xstr2)
    //println(resp.parse())
    println(resp1.toSOAPSReq())
  }
}