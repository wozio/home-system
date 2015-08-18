#!/usr/bin/env python

import logging
import service
import yami
import discovery

class io-control-service(service.service):
  def __init__(self):
    self.name = "io-control-dev"
    super(io-control-service, self).__init__(self.name)
    logging.debug("IO Control Service started")

    discovery.register(self.on_service)

  def exit(self):
    super(io-control-service, self).exit()
    logging.debug("IO Control Service exited")

  def on_service(self, service, available):
    #TODO: generic input/output service, part of the name?
    if service == "input-output":
      if available:
        logging.debug("IO service %s is available", service)

        # getting all inputs from service
        message = agent.send(discovery.get(service), service, "get_inputs")
        message.wait_for_completion(1000)

        state = message.get_state()[0]
        if state == OutgoingMessage.REPLIED:
          reply_content = message.get_reply()

          for id in reply_content["inputs"]:
            logging.debug("Input %d found", id)
        
            params = yami.Parameters()
            params["input"] = id

            message = self.agent.send(discovery.get(service), service,
                            "get_input_value", params);
            message.wait_for_completion(1000)

            state = message.get_state()[0]
            if state == OutgoingMessage.REPLIED:
              reply_content2 = message.get_reply()
              logging.debug("Input %d value = %f", id, reply_content2["value"])