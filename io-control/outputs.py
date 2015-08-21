#!/usr/bin/env python

import logging
import output
import configuration

outputs = {}

def init():
  logging.debug("Outputs init")
  global outputs
  for o in configuration.outputs:
    outputs[o["name"]] = output.output(o["service"], o["id"])
    
def add(o):
    outputs[o.get_name()] = o;

def exit():
  global outputs
