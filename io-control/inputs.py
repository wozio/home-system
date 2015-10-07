#!/usr/bin/env python

import logging
import input
import configuration

inputs = {}

def init():
  #TODO add discovering the inputs from system
  logging.debug("Inputs init")

def add(i):
    inputs[i.get_name()] = i;

def exit():
  pass
