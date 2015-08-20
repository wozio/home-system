#!/usr/bin/env python

import logging
import service
import yami
import yagent
import discovery
import outputs
import output
import input
import inputs

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

    for n, i in inputs.inputs.iteritems():
      names.append(n)
      inputState = i.get_state()
      values.append(inputState[0])
      times.append(inputState[1])

    # putting lists into parameters
    params = yami.Parameters()
    params["names"] = names
    params["values"] = values
    params["times"] = times

    message.reply(params)
  else:
    serv.on_msg(message)

def on_service(new_service, available):
  if available:
    #TODO: generic input/output service, part of the name?
    if new_service == "input-output" or new_service == "relay-board":
      logging.debug("IO service %s is available", new_service)

      # getting all inputs from service
      message = yagent.agent.send(discovery.get(new_service), new_service, "get_inputs")

      message.wait_for_completion(1000)

      state = message.get_state()[0]
      if state == message.REPLIED:
        reply_content = message.get_reply()

        for id in reply_content["inputs"]:
          logging.debug("Input %d found", id)

          i = input.input(new_service + "_" + str(id), new_service, id)
          inputs.add(i)

      # getting all outputs from service
      message = service.agent.send(discovery.get(new_service), new_service, "get_all_outputs")
      message.wait_for_completion(1000)

      state = message.get_state()[0]
      if state == message.REPLIED:
        reply_content = message.get_reply()

        for id in reply_content["outputs"]:
          logging.debug("Output %d found", id)
          
