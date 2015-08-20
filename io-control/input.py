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

    self.read_value()

  def get_name(self):
    return self.name

  def get_state(self):
    self.read_value()
    return self.value, self.time

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

