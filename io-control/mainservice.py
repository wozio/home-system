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

name = "io-control-dev"

service_callbacks = {}

def init():

    global serv
    serv = service.service(name, on_msg)

    discovery.register(on_service)

    logging.debug("IO Control Service started")

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
                            "subscribe", params);
                            
def prepare_services():
  services = []

  for service in ioservices.Ioservices.itervalues():
    service_params = yami.Parameters()
    service_params["name"] = service.name

    if len(service.settings) > 0:
      settings = []
      for st in service.settings.itervalues():
        setting_params = yami.Parameters()
        setting_params["name"] = st.name
        setting_params["type"] = st.type
        setting_params["value"] = st.get()
        values = []
        for val in st.values:
          values.append(val)
        setting_params["values"] = values
        settings.append(setting_params)
      service_params["settings"] = settings

    if len(service.displays) > 0:
      displays = []
      for d in service.displays.itervalues():
        display_params = yami.Parameters()
        display_params["name"] = d.name
        display_params["type"] = d.type
        display_params["state"],display_params["value"] = d.get()
        displays.append(display_params)
      service_params["displays"] = displays

    services.append(service_params)

  params = yami.Parameters()
  params["services"] = services
  return params

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
      while i in service_callbacks:
        i += 1
      service = message.get_parameters()["service"]
      service_callbacks[i] = service
      params = yami.Parameters()
      params["id"] = i
      message.reply(params)
      
      yagent.agent.send(discovery.get(service), service,
        "services_change", prepare_services());
      
    elif message.get_message_name() == "unsubscribe_services":
      service_callbacks.pop(message.get_parameters()["id"], None)

    elif message.get_message_name() == "get_services":
      message.reply(prepare_services())

    elif message.get_message_name() == "set_setting_value":
        logging.debug("set_setting_value")
        try:
            params = message.get_parameters()
            ioservices.Ioservices[params["service"]].settings[params["setting"]].set(params["value"])
        except KeyError as e:
            logging.warn("Something is not found, either in parameters or in system: %s", e.strerror)

    else:
        serv.on_msg(message)
