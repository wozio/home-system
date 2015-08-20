#!/usr/bin/env python

import logging
import service
import yami
import discovery

# these are inputs and outputs found in the system
# configured to be used and monitored are copied from here to configuration arrays
outputs = []
inputs = {}

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
  if message.get_message_name() == "get_inputs":
    #preparing lists for sending
    names = []
    values = []
    times = []
    
    global inputs
    for name, input in inputs:
      names += name
      values += input["value"]
      times += input["time"]
    
    params = yami.Parameters()
    params["names"] = names
    params["values"] = values
    params["times"] = times
    
    message.reply(params)
  else  
    serv.on_msg(message)

def on_service(new_service, available):
  if available:
    #TODO: generic input/output service, part of the name?
    if new_service == "input-output" or new_service == "relay-board":
      logging.debug("IO service %s is available", new_service)

      # getting all inputs from service
      message = service.agent.send(discovery.get(new_service), new_service, "get_inputs")

      message.wait_for_completion(1000)

      state = message.get_state()[0]
      if state == message.REPLIED:
        reply_content = message.get_reply()

        for id in reply_content["inputs"]:
          logging.debug("Input %d found", id)
      
          params = yami.Parameters()
          params["input"] = id

          message = service.agent.send(discovery.get(new_service), new_service,
                          "get_input_value", params);
          message.wait_for_completion(1000)

          state = message.get_state()[0]
          if state == message.REPLIED:
            reply_content2 = message.get_reply()
            logging.debug("Input %d value = %f", id, reply_content2["value"])
            
            global inputs
            inputs[new_service + "_" + id] += [{
              "id": id,
              "value": reply_content2["value"],
              "time": reply_content2["time"]
            }]
              
            
      # getting all outputs from service
      message = service.agent.send(discovery.get(new_service), new_service, "get_all_outputs")
      message.wait_for_completion(1000)

      state = message.get_state()[0]
      if state == message.REPLIED:
        reply_content = message.get_reply()

        for id in reply_content["outputs"]:
          logging.debug("Output %d found", id)
      
          params = yami.Parameters()
          params["output"] = id

          message = service.agent.send(discovery.get(new_service), new_service,
                          "get_output_state", params);
          message.wait_for_completion(1000)

          state = message.get_state()[0]
          if state == message.REPLIED:
            reply_content2 = message.get_reply()
            logging.debug("Output %d state = %d", id, reply_content2["state"])