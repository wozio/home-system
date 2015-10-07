#!/usr/bin/env python

import logging
import output
import configuration

outputs = {}

def init():
  logging.debug("Outputs init")

def add(o):
    outputs[o.get_name()] = o;

def exit():
  pass
