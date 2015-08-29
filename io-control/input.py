#!/usr/bin/env python

import socket
import struct
import logging
import yami
import yagent
import discovery

class input:

  def __init__(self, input_name, input_service, input_id, ):
    self.id = input_id
    self.service = input_service
    self.name = input_name
    self.time = 0
    self.value = 0

    logging.info("Created input %s with service=%s and id=%d", input_name, input_service, input_id)

    yagent.agent.register_object(self.name, self.on_msg)

    discovery.register(self.on_service)

  def get_name(self):
    return self.name

  def get_state(self):
    self.read_value()
    return self.value, self.time

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
                        "subscribe_state_change", params);

  def read_value(self):
    params = yami.Parameters()
    params["input"] = self.id

    message = yagent.agent.send(discovery.get(self.service), self.service,
                    "get_input_value", params)

    message.wait_for_completion()

    state = message.get_state()[0]
    if state == message.REPLIED:
      reply_content = message.get_reply()

      self.value = reply_content["value"]
      self.time = reply_content["time"]

      logging.debug("Input %s value %f on time %d", self.name, self.value, self.time)

  def on_msg(self, message):
    if message.get_message_name() == "output_state_change":
      self.state = message.get_parameters()["state"]
      logging.debug("Input %d state changed to %f", self.id, self.state)
    else:
      logging.debug("Unknown message %s from %s", message.get_message_name(), message.get_source())
      message.reject("Unknown message")
