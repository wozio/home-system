#!/usr/bin/env python

import socket
import struct
import logging
import yami
import discovery

def cec():
  print "CEC"

class output:

  def __init__(self, output_service, output_num):
    self.output_num = output_num
    self.output_service = output_service
    self.name = "output_" + str(self.output_service) + str(self.output_num)

    logging.info("Created output with service=%s and output=%d", output_service, output_num)
    
    discovery.register(self.on_service)
    
  def on_service(self, service, available):
    if service == self.output_service:
      if available:
        logging.debug("Output service %s is available", service)
        
        # get ip address
        ip = [(s.connect(('8.8.8.8', 80)), s.getsockname()[0], s.close()) for s in [socket.socket(socket.AF_INET, socket.SOCK_DGRAM)]][0][1]
        
        # initialize yami agent
        self.agent = yami.Agent(connection_event_callback = cec)
        self.ye = self.agent.add_listener("tcp://" + ip + ":*")
        self.agent.register_object("*", self.on_msg)

        # subscribe for output state change notifications
        params = yami.Parameters()
        params["output"] = self.output_num
        params["name"] = self.name
        params["endpoint"] = self.ye
        
        self.agent.send(discovery.get(service), self.output_service,
                        "subscribe_output_state_change", params);
      else:
        logging.debug("Output service %s is not available", service)
      
  def on_msg(self, message):
    if message.get_message_name() == "output_state_change":
      new_state = message.get_parameters()["state"]
      logging.debug("Output state changed to %d", new_state)
    else:
      logging.debug("Unknown message %s from %s", message.get_message_name(), message.get_source())
