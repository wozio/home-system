#!/usr/bin/env python

import logging
import service
import yami
import yagent
import discovery
import outputs
import output
import inputs

def init():

  global serv
  serv = service.service("io-control-dev", on_msg)

  logging.debug("IO Control Service started")

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

    for n, i in sorted(inputs.inputs.iteritems()):
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
  elif message.get_message_name() == "get_outputs":
    #preparing lists for sending
    names = []
    values = []
    times = []

    for n, o in sorted(outputs.outputs.iteritems()):
      names.append(n)
      outputState = o.get_state()
      values.append(outputState[0])
      times.append(outputState[1])

    # putting lists into parameters
    params = yami.Parameters()
    params["names"] = names
    params["values"] = values
    params["times"] = times

    message.reply(params)
  else:
    serv.on_msg(message)
    
