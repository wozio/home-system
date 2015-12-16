#!/usr/bin/env python

import logging
import input
import mytimer
import configuration
import yagent
import discovery

inputs_per_id = {}
inputs = {}

def init():
    logging.debug("Inputs init")

    # timer is always defined input
    itimer = mytimer.timer()
    inputs_per_id[("", "Timer")] = itimer
    inputs["Timer"] = itimer

    #defined inputs in configuration
    for i in configuration.defined_inputs:
        iobj = input.input(i['name'], i['service'], i['id'])
        inputs_per_id[(i['service'], i['id'])] = iobj
        inputs[i['name']] = iobj

    discovery.register(on_service)

def exit():
  for i in inputs.itervalues():
    i.exit()

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

                for iid in reply_content["inputs"]:
                    logging.debug("Input %d found", iid)

                    if (new_service, iid) not in inputs_per_id:
                        i = input.input(new_service + "_" + str(iid), new_service, iid)
                        inputs_per_id[(new_service, iid)] = i
                        inputs[i.name()] = i
                    else:
                        logging.debug("Input known")
                
