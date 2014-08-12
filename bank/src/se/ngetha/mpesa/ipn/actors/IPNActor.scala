/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package se.ngetha.mpesa.ipn.actors

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.Props

object IPNActor{  
  def props(str : String) : Props = Props(new IPNActor(str))
}

class IPNActor(str: String) extends Actor with ActorLogging{
  
  def receive = {
    case "receive" => {
        log.info(s"Got receive ${str}")
        sender ! "ok"
    }
    case IPNRequest(id, orig, dest, tstamp,
                             text, user, pass, mpesa_code, mpesa_acc, 
                             mpesa_msisdn, mpesa_trx_date,
                             mpesa_trx_time, mpesa_amt, mpesa_sender) => {
        // create instance
        val vx = IPNRequest(id, orig, dest, tstamp,
                             text, user, pass, mpesa_code, mpesa_acc, 
                             mpesa_msisdn, mpesa_trx_date,
                             mpesa_trx_time, mpesa_amt, mpesa_sender)
         // process the request
        log.info("IPNReq -> "+ vx.toShortJSONString())
        
        sender ! "adfadsfds"
    }
    case _ => log.info(s"Got unkown message ${str}")
  }
  
  def processIncoming(r : IPNRequest) : String = {
      "dpp"
  }
}
