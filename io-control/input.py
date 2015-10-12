#!/usr/bin/env python

import socket
import struct
import logging
import yami
import yagent
import discovery

class input:

  def __init__(self, input_name, input_service, input_id):
    self.id = input_id
    self.service = input_service
    self.name = input_name
    self.state = 0
    self.callbacks = []
    self.ready = False

    logging.info("Created input '%s' with service=%s and id=%d", input_name, input_service, input_id)

    yagent.agent.register_object(self.name, self.on_msg)

    discovery.register(self.on_service)

  def get(self):
    if not self.ready:
      raise RuntimeError("Input not ready")
    return self.state

  def on_service(self, service, available):
    if service == self.service:
      if available:
        logging.debug("Input service %s is available", service)

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
    else:
      logging.debug("Unknown message %s from %s", message.get_message_name(), message.get_source())
      message.reject("Unknown message")

  def subscribe(self, callback):
    self.callbacks.append(callback)
