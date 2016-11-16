import logging
import traceback
import random
import threading
import service
import yami
import yagent
import discovery

class test_io:

    def __init__(self):
        self.name = "io.test"

        logging.info("Created io test service '%s'", self.name)

        self.s = None
        self.e = None

        self.serv = service.service(self.name, self.on_msg)

    def __del__(self):
        self.serv = None

    def new_value(self):
        if self.s and self.e:
            params = yami.Parameters()
            params["name"] = self.name
            params["id"] = 1
            params["type"] = 0 #temperature input
            params["state"] = 1
            params["value"] = random.uniform(-20, 30)
            logging.info("Sending new value to '%s'", self.s)
            yagent.agent.send(self.e, self.s,
                "state_change", params)
            global t
            t = threading.Timer(1, self.new_value)
            t.start()

    def on_msg(self, msg):
        try:
            if msg.get_message_name() == "subscribe":
                self.s = msg.get_parameters()["name"]
                self.e = msg.get_parameters()["endpoint"]
                logging.debug("service '%s (%s)' subscribed", self.s, self.e)

                self.new_value()
                
        except Exception as e:
            logging.error(traceback.format_exc())
            raise e

logger = logging.getLogger()
logger.setLevel(logging.DEBUG)
formatter = logging.Formatter('[%(asctime)s] [%(levelname)s] [%(thread)d] [%(filename)s:%(lineno)d] %(message)s')

ch = logging.StreamHandler()
ch.setFormatter(formatter)
logger.addHandler(ch)

logging.info("Starting Home System IO Test")

discovery.init()
  
tio = test_io()

while 1:
  if raw_input() == "q":
      break

t.cancel()
