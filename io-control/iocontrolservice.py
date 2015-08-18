#!/usr/bin/env python

import logging
import service
import yami
import discovery

class iocontrolservice(service.service):
  def __init__(self):
    self.name = "io-control-dev"
    super(iocontrolservice, self).__init__(self.name)
    logging.debug("IO Control Service started")

    discovery.register(self.on_service)

  def exit(self):
    super(iocontrolservice, self).exit()
    logging.debug("IO Control Service exited")

  def on_service(self, service, available):
    if available:
      #TODO: generic input/output service, part of the name?
      if service == "input-output" or service == "relay-board":
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
              
        # getting all outputs from service
        message = agent.send(discovery.get(service), service, "get_all_outputs")
        message.wait_for_completion(1000)

        state = message.get_state()[0]
        if state == OutgoingMessage.REPLIED:
          reply_content = message.get_reply()

          for id in reply_content["outputs"]:
            logging.debug("Output %d found", id)
        
            params = yami.Parameters()
            params["output"] = id

            message = self.agent.send(discovery.get(service), service,
                            "get_output_state", params);
            message.wait_for_completion(1000)

            state = message.get_state()[0]
            if state == OutgoingMessage.REPLIED:
              reply_content2 = message.get_reply()
              logging.debug("Output %d state = %d", id, reply_content2["state"])