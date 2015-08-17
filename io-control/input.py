#!/usr/bin/env python

import socket
import struct
import logging
import yami
import discovery

class input:

  def __init__(self, input_service, input_num):
    self.input_num = input_num
    self.input_service = input_service
    self.time = 0
    self.value = 0

    logging.info("Created input with service=%s and input=%d", input_service, input_num)
    
  def get_value(self):
    agent = yami.Agent()
    params = yami.Parameters()
    params["input"] = self.input_num

    message = agent.send(discovery.get(self.input_service), self.input_service,
                    "get_input_value", params)
                    
    message.wait_for_completion()

    state = message.get_state()[0]
    if state == OutgoingMessage.REPLIED:
      reply_content = message.get_reply()
      
      self.value = reply_content["value"]
      self.time = reply_content["time"]
                    
    return self.value;
    
  def get_time(self):
    return self.time;
