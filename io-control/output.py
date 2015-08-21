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
    self.time = 0

    logging.info("Created output with service=%s and id=%d", output_service, output_id)

    yagent.agent.register_object(self.name, self.on_msg)

    discovery.register(self.on_service)

  def get_name(self):
      return self.name

  def get_state(self):
    return self.state, self.time;

  def on_service(self, service, available):
    if service == self.service:
      if available:
        logging.debug("Output service %s is available", service)

        # subscribe for output state change notifications
        params = yami.Parameters()
        params["output"] = self.id
        params["name"] = self.name
        params["endpoint"] = yagent.endpoint

        yagent.agent.send(discovery.get(self.service), self.service,
                        "subscribe_output_state_change", params);

  def on_msg(self, message):
    if message.get_message_name() == "output_state_change":
      self.state = message.get_parameters()["state"]
      logging.debug("Output %d state changed to %d", self.id, self.state)
    else:
      logging.debug("Unknown message %s from %s", message.get_message_name(), message.get_source())
      message.reject("Unknown message")
