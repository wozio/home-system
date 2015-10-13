#!/usr/bin/env python

import socket
import struct
import logging
import yami
import yagent
import discovery

class output:

  def __init__(self, output_name, output_service, output_id):
    self.id = output_id
    self.service = output_service
    self.name = output_name
    self.state = 0
    self.callbacks = []
    self.ready = False
    self.wanted_state = 0

    logging.info("Created output '%s' with service=%s and id=%d", self.name, output_service, output_id)

    yagent.agent.register_object(self.name, self.on_msg)

    discovery.register(self.on_service)

  def get(self):
    if not self.ready:
      raise RuntimeError("Output not ready")
    return self.state

  def set(self, state):
    self.wanted_state = state

    if self.wanted_state != self.state:
      logging.debug("'%s' state set to %f", self.name, state)
      self.set_state()

  def set_state(self):
    params = yami.Parameters()
    params["output"] = int(self.id)
    params["state"] = int(self.wanted_state)

    yagent.agent.send(discovery.get(self.service), self.service,
                    "set_output_state", params);

  def on_service(self, service, available):
    if service == self.service:
      if available:
        logging.debug("Output service %s is available", service)

        # subscribe for output state change notifications
        params = yami.Parameters()
        params["id"] = long(self.id)
        params["name"] = self.name
        params["endpoint"] = yagent.endpoint

        yagent.agent.send(discovery.get(self.service), self.service,
                        "subscribe", params);

  def on_msg(self, message):
    if message.get_message_name() == "state_change":
      new_state = message.get_parameters()["state"]
      if not self.ready or new_state != self.state:
        self.ready = True
        self.state = message.get_parameters()["state"]
        logging.debug("'%s' state changed to %f", self.name, self.state)
        for c in self.callbacks:
          c()
      if self.wanted_state != self.state:
        logging.debug("Wanted state of '%s' different from actual, setting to %f", self.name, state)
        self.set_state()
    else:
      logging.debug("Unknown message %s from %s", message.get_message_name(), message.get_source())
      message.reject("Unknown message")

  def subscribe(self, callback):
    self.callbacks.append(callback)
    callback()
