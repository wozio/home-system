#!/usr/bin/env python

import logging
import input
import configuration
import yami
import yagent
import discovery

inputs_per_id = {}
inputs = {}

def init():
    logging.debug("Inputs init")

    for i in configuration.defined_inputs:
        iobj = input.input(i['name'], i['service'], i['id'])
        inputs_per_id[(i['service'], i['id'])] = iobj
        inputs[i['name']] = iobj

    discovery.register(on_service)

def on_service(new_service, available):
    if available:
        if new_service.find("io.", 0, 3) == 0:
            logging.debug("IO service %s is available", new_service)

            # getting all inputs from service
            message = yagent.agent.send(discovery.get(new_service), new_service, "get_inputs")

            message.wait_for_completion(1000)

            state = message.get_state()[0]
            if state == message.REPLIED:
                reply_content = message.get_reply()

                for id in reply_content["inputs"]:
                    logging.debug("Input %d found", id)

                    if (new_service, id) not in inputs_per_id:
                        i = input.input(new_service + "_" + str(id), new_service, id)
                        inputs_per_id[(new_service, id)] = i
                        inputs[i.name()] = i
                    else:
                      logging.debug("Input known")
