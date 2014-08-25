#!/usr/bin/env python

import logging
import output

outputs = {}

def init():
  logging.debug("Outputs init")
  global outputs
  outputs["rb1"] = output.output("relay-board", 1)

def exit():
  global outputs
