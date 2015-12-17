#!/usr/bin/env python

import logging
import input
import mytimer
import configuration

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

def exit():
    for i in inputs.itervalues():
        i.exit()

def on_state_change(name, id, state, value):
    #logging.debug("Input %s %d state change", name, id)

    if (name, id) not in inputs_per_id:
        logging.debug("Input not known, creating")
        i = input.input(name + "_" + str(id), name, id)
        inputs_per_id[(name, id)] = i
        inputs[i.name] = i
        i.on_state_change(state, value)
    else:
        #logging.debug("Input known")
        inputs_per_id[(name, id)].on_state_change(state, value)
