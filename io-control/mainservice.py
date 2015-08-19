#!/usr/bin/env python

import logging
import service
import yami
import discovery

def init():
  
  global serv
  serv = service.service("io-control-dev", on_msg)

  logging.debug("IO Control Service started")

  discovery.register(on_service)

def exit():
  global serv
  serv.exit()
  logging.debug("IO Control Service exited")
    
def on_msg(message):
  global serv
  serv.on_msg(message)

def on_service(service, available):
  if available:
    #TODO: generic input/output service, part of the name?
    if service == "input-output" or service == "relay-board":
      logging.debug("IO service %s is available", service)

      # getting all inputs from service
      message = service.agent.send(discovery.get(service), service, "get_inputs")
      message.wait_for_completion(1000)

      state = message.get_state()[0]
      if state == OutgoingMessage.REPLIED:
        reply_content = message.get_reply()

        for id in reply_content["inputs"]:
          logging.debug("Input %d found", id)
      
          params = yami.Parameters()
          params["input"] = id

          message = service.agent.send(discovery.get(service), service,
                          "get_input_value", params);
          message.wait_for_completion(1000)

          state = message.get_state()[0]
          if state == OutgoingMessage.REPLIED:
            reply_content2 = message.get_reply()
            logging.debug("Input %d value = %f", id, reply_content2["value"])
            
      # getting all outputs from service
      message = service.agent.send(discovery.get(service), service, "get_all_outputs")
      message.wait_for_completion(1000)

      state = message.get_state()[0]
      if state == OutgoingMessage.REPLIED:
        reply_content = message.get_reply()

        for id in reply_content["outputs"]:
          logging.debug("Output %d found", id)
      
          params = yami.Parameters()
          params["output"] = id

          message = service.agent.send(discovery.get(service), service,
                          "get_output_state", params);
          message.wait_for_completion(1000)

          state = message.get_state()[0]
          if state == OutgoingMessage.REPLIED:
            reply_content2 = message.get_reply()
            logging.debug("Output %d state = %d", id, reply_content2["state"])