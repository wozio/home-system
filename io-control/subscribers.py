import logging
import traceback
import discovery
import yami
import yagent

class subscribers:
  
    def __init__(self):
        self.subscriptions = {}
        discovery.register(self.on_service_availability)

    def add(self, s):
        # first search if this service is not already in dict
        for i, os in self.subscriptions.iteritems():
            if os == s:
                # it is already there, return subscription id
                return i

        # search for free subscription id and add service when found
        i = 0
        while i in self.subscriptions:
            i += 1
        self.subscriptions[i] = s
        return i

    def remove(self, id):
        if id in self.subscriptions:
            logging.debug("Removing service subscription '%s[%d]'", self.subscriptions[id], id)
            self.subscriptions.pop(id)

    def send(self, msg, params):
        to_remove = []

        for i, s in self.subscriptions.iteritems():
            logging.debug("sending '%s' to '%s[%d]'", msg, s, i)
            try:
                yagent.agent.send(discovery.get(s), s,
                  msg, params)
            except yami.YAMIError as e:
                logging.error("error while sending '%s' to '%s[%d]': %s", msg, s, i, traceback.format_exc())
                to_remove.append(i)
            
        for i in to_remove:
            self.remove(i)

    def send_to(self, id, msg, params):
        if id in self.subscriptions:
          s = self.subscriptions[id]
          logging.debug("sending '%s' to '%s[%d]'", msg, s, id)
          try:
              yagent.agent.send(discovery.get(s), s,
                msg, params)
          except yami.YAMIError as e:
              logging.error("error while sending '%s' to '%s[%d]': %s", msg, s, i, traceback.format_exc())
              self.remove(id)

    def on_service_availability(self, s, available):
        if not available:
          to_remove = []
          for i, os in self.subscriptions.iteritems():
              if s == os:
                  to_remove.append(i)

          for i in to_remove:
              self.remove(i)
