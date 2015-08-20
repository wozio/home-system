#!/usr/bin/env python

import logging
import input
import configuration

inputs = {}

def init():
  logging.debug("Inputs init")
  global inputs
  for i in configuration.inputs:
    inputs[i["name"]] = input.input(i["service"], i["id"])

def add(i):
    inputs[i.get_name()] = i;

def exit():
  global inputs
