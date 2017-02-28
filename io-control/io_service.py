import traceback
import time
import threading
import service
import yami
import yagent
import subscribers

class io_base:

    def __init__(self, id):
        self.subscriptions = subscribers.subscribers()

    def set_value(self, id, v):
        params = yami.Parameters()
        params["name"] = self.name
        params["id"] = self.id
        params["type"] = self.type
        params["state"] = 1
        params["value"] = v
        self.value = v

        self.subscriptions.send("state_change", params)

    def add_subscription(self, n):
        self.subscriptions.add(n)

class io_service:

    def __init__(self, name):
        self.name = "io." + str(name)
        self.serv = service.service(self.name, self.on_msg)
        self.ios = {}

    def exit(self):
        self.serv.exit()

    def on_msg(self, msg):
       if msg.get_message_name() == "subscribe":
            n = msg.get_parameters()["name"]
            for io in ios:
                io.add_subscription(n)
