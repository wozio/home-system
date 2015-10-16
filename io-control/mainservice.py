#!/usr/bin/env python

import logging
import service
import yami
import yagent
import discovery
import configuration
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
    inputs_list = []

    for n, i in sorted(inputs.inputs.iteritems()):
      input_params = yami.Parameters()
      input_params["name"] = n
      input_params["value"] = i.get()
      inputs_list.append(input_params)

    params = yami.Parameters()
    params["inputs"] = inputs_list
    message.reply(params)

  elif message.get_message_name() == "get_outputs":
    #preparing lists for sending
    outputs_list = []

    for n, o in sorted(outputs.outputs.iteritems()):
      output_params = yami.Parameters()
      output_params["name"] = n
      output_params["value"] = o.get()
      outputs_list.append(output_params)

    params = yami.Parameters()
    params["outputs"] = outputs_list
    message.reply(params)

  elif message.get_message_name() == "get_services":
    #preparing lists for sending
    services = []

    for s in sorted(configuration.services):
        service_params = yami.Parameters()
        service_params["name"] = s["name"]
        settings = []
        for st in s["settings"]:
            setting_params = yami.Parameters()
            setting_params["name"] = st["name"]
            setting_params["type"] = st["type"]
            settings.append(setting_params)
        service_params["settings"] = setting_params
        services.append(service_params)

    params = yami.Parameters()
    params["services"] = services

    message.reply(params)
  else:
    serv.on_msg(message)
