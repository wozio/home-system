#!/usr/bin/env python

import logging
import rules
import yami

class Setting:

  def __init__(self, name, data):
    logging.info("Created setting '%s'", name)

    self.name = name
    self.type = data["type"]
    self.values = data["values"]
    self.value = self.values.iterkeys().next()
    self.change_callback = None
    self.set(data["default"])

  def set(self, value):
    if value not in self.values:
        logging.warn("Required value '%s' no in values list", value)
        return;

    logging.debug("Setting '%s' set to value '%s'", self.name, value)
    # disable previous rule
    rules.rules[self.values[self.value]["rule"]].disable()
    # enable new rule
    rules.rules[self.values[value]["rule"]].enable()

    self.value = value
    if self.change_callback != None:
      self.change_callback(self)

  def get(self):
    return self.value;

  def subscribe(self, callback):
    self.change_callback = callback

  def prepare(self):
    params = yami.Parameters()
    params["name"] = self.name
    params["type"] = self.type
    params["value"] = self.get()
    params["values"] = self.values

    return params