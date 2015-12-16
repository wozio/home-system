#!/usr/bin/env python

import logging
import output
import configuration

outputs = {}
outputs_per_id = {}

def init():
    logging.debug("Outputs init")

    for o in configuration.defined_outputs:
        oobj = output.output(o['name'], o['service'], o['id'])
        outputs_per_id[(o['service'], o['id'])] = oobj
        outputs[o['name']] = oobj

def on_state_change(name, id, state):
    logging.debug("Output %s %d state change", name, id)

    if (name, id) not in outputs_per_id:
        logging.debug("Output not known, creating")
        o = output.output(name + "_" + str(id), name, id)
        outputs_per_id[(name, id)] = o
        outputs[o.name()] = o
        o.on_state_change(state)
    else:
        logging.debug("Output known")
        outputs_per_id[(name, id)].on_state_change(state)
