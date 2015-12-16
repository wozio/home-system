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

    elif message.get_message_name() == "get_services":
        #preparing lists for sending
        services = []

        for s in configuration.services:
            service_params = yami.Parameters()
            service_params["name"] = s["name"]
            settings = []
            for st in s["settings"]:
                setting_params = yami.Parameters()
                setting_params["name"] = st["name"]
                setting_params["type"] = st["type"]
                setting_params["value"] = ioservices.Ioservices[s["name"]].settings[st["name"]].get()
                values = []
                for val in st["data"]["values"]:
                    values.append(val)
                setting_params["values"] = values
                settings.append(setting_params)
            service_params["settings"] = settings
            services.append(service_params)

        params = yami.Parameters()
        params["services"] = services

        message.reply(params)

    elif message.get_message_name() == "set_setting_value":
        try:
            params = message.get_parameters()
            ioservices.Ioservices[params["service"]].settings[params["setting"]].set(params["value"])
        except KeyError as e:
            logging.warn("Something is not found, either in parameters or in system: %s", e.strerror)

    else:
        serv.on_msg(message)
