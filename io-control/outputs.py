#!/usr/bin/env python

import logging
import output
import configuration
import yami
import yagent
import discovery

outputs = {}
outputs_per_id = {}

def init():
    logging.debug("Outputs init")

    for o in configuration.defined_outputs:
        oobj = output.output(o['name'], o['service'], o['id'])
        outputs_per_id[(o['service'], o['id'])] = oobj
        outputs[o['name']] = oobj

    discovery.register(on_service)

def exit():
  pass

def on_service(new_service, available):
    if available:
        if new_service.find("io.", 0, 3) == 0:
            logging.debug("IO service %s is available", new_service)

            # getting all inputs from service
            message = yagent.agent.send(discovery.get(new_service), new_service, "get_outputs")

            message.wait_for_completion(1000)

            state = message.get_state()[0]
            if state == message.REPLIED:
                reply_content = message.get_reply()

                for id in reply_content["outputs"]:
                    logging.debug("Output %d found", id)

                    if (new_service, id) not in outputs_per_id:
                        o = output.output(new_service + "_" + str(id), new_service, id)
                        outputs_per_id[(new_service, id)] = o
                        outputs[o.name] = o
                    else:
                      logging.debug("Output known")
                      
