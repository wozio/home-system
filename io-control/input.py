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
    self.time = 0
    self.state = 0
    self.callbacks = []

    logging.info("Created input %s with service=%s and id=%d", input_name, input_service, input_id)

    yagent.agent.register_object(self.name, self.on_msg)

    discovery.register(self.on_service)

  def get_name(self):
    return self.name

  def get_state(self):
    return self.state, self.time

  def on_service(self, service, available):
    if service == self.service:
      if available:
        logging.debug("Input service %s is available", service)

        # subscribe for output state change notifications
        params = yami.Parameters()
        params["id"] = self.id
        params["name"] = self.name
        params["endpoint"] = yagent.endpoint

        yagent.agent.send(discovery.get(self.service), self.service,
                        "subscribe", params);

  def on_msg(self, message):
    if message.get_message_name() == "state_change":
      self.state = message.get_parameters()["state"]
      self.time = message.get_parameters()["time"]
      logging.debug("Input %s state changed to %f", self.name, self.state)
      for c in self.callbacks:
        c()
    else:
      logging.debug("Unknown message %s from %s", message.get_message_name(), message.get_source())
      message.reject("Unknown message")

  def subscribe(self, callback):
    self.callbacks.append(callback)
    callback()
