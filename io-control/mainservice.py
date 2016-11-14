#!/usr/bin/env python

import logging
import service
import yami
import yagent
import configuration
import ioservices
import outputs
import output
import inputs
import discovery
import traceback

name = "io-control-dev"

services_subscriptions = {}
service_subscriptions = {}

def init():

    global serv
    serv = service.service(name, on_msg)

    discovery.register(on_service)

    logging.debug("IO Control Service started")
    
    ioservices.subscribe(on_ioservice_change)

def exit():
    global serv
    serv.exit()
    logging.debug("IO Control Service exited")

def on_service(new_service, available):
  if available:
    if new_service.find("io.", 0, 3) == 0:
      logging.debug("IO service %s is available", new_service)

      # subscribe for io state change notifications
      params = yami.Parameters()
      params["name"] = name
      params["endpoint"] = yagent.endpoint

      yagent.agent.send(discovery.get(new_service), new_service,
        "subscribe", params)
  else:
    to_remove = []
    for i, s in services_subscriptions.iteritems():
      if s == new_service:
        to_remove.append(i)
      
    for i in to_remove:
      services_subscriptions.pop(i, None)
      logging.debug("service subscription notification %d removed", i)

def on_ioservice_change(service):
  
  logging.debug("service '%s'changed, sending to subscribers", service.name)
  
  params = service.prepare()
  
  to_remove = []

  for i, s in services_subscriptions.iteritems():
    logging.debug("sending to '%s'", s)
    try:
      yagent.agent.send(discovery.get(s), s,
        "services_change", params)
    except RuntimeError:
      to_remove.append(i)
      
  for i in to_remove:
    services_subscriptions.pop(i, None)
    logging.debug("service subscription notification %d removed", i)

def on_msg(message):
    global serv

    if message.get_message_name() == "state_change":
        params = message.get_parameters()
        if params["type"] == 0: # temperature input
            inputs.on_state_change(params["name"], params["id"], params["state"], params["value"])
        elif params["type"] == 1: # switch output
            outputs.on_state_change(params["name"], params["id"], params["state"], params["value"])

    elif message.get_message_name() == "get_inputs":
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
        
    elif message.get_message_name() == "subscribe_services":
      i = 0
      while i in services_subscriptions:
        i += 1
      service = message.get_parameters()["service"]
      services_subscriptions[i] = service
      logging.debug("service '%s' subscribed for services change notification with id %d", service, i)
      
      params = yami.Parameters()
      params["id"] = i
      message.reply(params)
      
      # preparing services to send
      services_to_send = []

      for ioservice in ioservices.Ioservices.itervalues():
        try:
          print ioservice
          services_to_send.append(ioservice.prepare())
        except Exception as e:
          print traceback.format_exc()
          raise e

      params = yami.Parameters()
      params["services"] = services_to_send
      
      yagent.agent.send(discovery.get(service), service,
        "services_full", params)
      
    elif message.get_message_name() == "unsubscribe_services":
      i = message.get_parameters()["id"]
      services_subscriptions.pop(i, None)
      logging.debug("service subscription notification %d removed", i)

    elif message.get_message_name() == "get_services":
      message.reply(prepare_services())

    elif message.get_message_name() == "set_setting_value":
        logging.debug("set_setting_value")
        try:
            params = message.get_parameters()
            ioservices.Ioservices[params["service"]].settings[params["setting"]].set(params["value"])
        except KeyError as e:
            logging.warn("Something is not found, either in parameters or in system: %s", e.strerror)
    elif message.get_message_name() == "get_io":
        logging.debug("set_setting_value")
        try:
            params = message.get_parameters()
            ioservices.Ioservices[params["service"]].settings[params["setting"]].set(params["value"])
        except KeyError as e:
            logging.warn("Something is not found, either in parameters or in system: %s", e.strerror)

    else:
        serv.on_msg(message)
