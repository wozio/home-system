#!/usr/bin/env python

import logging
import traceback
import threading
import setting
import display
import service
import discovery
import yami
import yagent
import subscribers



class ioservice:

    def __init__(self, name, displays_, settings_):
        self.name = name
        self.settings = {}
        self.displays = {}
        self.subscriptions = subscribers.subscribers()
        self.change_callback = None
        self.history_threads = {}
        self.history_threads_abort = {}
        self.lock = threading.Lock()

        logging.info("Created ioservice '%s'", name)

        for s in settings_:
            new_setting = setting.Setting(s["name"], s["data"])
            new_setting.subscribe(self.on_change)
            self.settings[s["name"]] = new_setting

        for d in displays_:
            new_display = display.Display(d["name"], d["data"])
            new_display.subscribe(self.on_change)
            self.displays[d["name"]] = new_display

        self.serv = service.service("io-control.service." + name, self.on_msg)

    def exit(self):
        for t in self.history_threads:
            pass
        self.serv.exit()

    def subscribe(self, callback):
        self.change_callback = callback
    
    def on_change(self, disp_or_setting):

        if self.change_callback != None:
            self.change_callback(self)

        # sending change to subscribers
        params = self.prepare()
        self.subscriptions.send("service_change", params)

    def prepare(self):
        params = yami.Parameters()
        params["name"] = self.name

        if len(self.settings) > 0:
            settings_params = []
            for st in self.settings.itervalues():
                settings_params.append(st.prepare())
            params["settings"] = settings_params

        if len(self.displays) > 0:
            displays_params = []
            for d in self.displays.itervalues():
                displays_params.append(d.prepare())
            params["displays"] = displays_params

        return params

    def send_history(self, id):
        try:
            # prepare history for sending
            displays_histories = {}
            msg_number = 0
            for d in self.displays.itervalues():
                history = d.prepare_history()
                # split in chunks per 100
                histories = [history[i:i + 100] for i in xrange(0, len(history), 100)]
                msg_number += len(histories)
                displays_histories[d.name] = histories

            logging.debug("sending history to subscription %d in %d messages", id, msg_number)

            # send information how many messages it will take
            with self.lock:
                if self.history_threads_abort[id] == True:
                    logging.debug("sending of history to subscription %d aborted", id)
                    raise
                params = yami.Parameters()
                params["name"] = self.name
                params["msg_number"] = msg_number
                self.subscriptions.send_to(id, "service_history_info", params)

            # now sending messages
            j = 0
            for d, histories in displays_histories.iteritems():
                params = yami.Parameters()
                params["name"] = self.name
                params["display_name"] = d
                for h in histories:
                    with self.lock:
                        if self.history_threads_abort[id] == True:
                            logging.debug("sending of history to subscription %d aborted", id)
                            raise
                        params["history"] = h
                        self.subscriptions.send_to(id, "service_history", params)
        except:
            logging.warning("Exception thrown while sending history to subscription %d: %s", id, traceback.format_exc())

        logging.debug("sending of history to subscription %d ended", id)

    def on_msg(self, msg):
        try:
            if msg.get_message_name() == "subscribe":
                s = msg.get_parameters()["service"]
                id = self.subscriptions.add(s)
                logging.debug("service '%s' subscribed for service '%s' change notification with id %d", s, self.name, id)
                
                # reply contains id of the subscription, current service state and history of displays
                params = yami.Parameters()
                params["id"] = id
                params["service"] = self.prepare()

                msg.reply(params)

                with self.lock:
                    t = threading.Thread(target=self.send_history, args=(id,))
                    self.history_threads_abort[id] = False
                    self.history_threads[id] = t
                    t.start()

            elif msg.get_message_name() == "unsubscribe":
                i = msg.get_parameters()["id"]
                if i in self.history_threads_abort:
                    with self.lock:
                        self.history_threads_abort[i] = True
                    self.history_threads[i].join()
                    with self.lock:
                        self.history_threads_abort.pop(i)
                        self.history_threads.pop(i)
                self.subscriptions.remove(i)

        except Exception as e:
            logging.error(traceback.format_exc())
            raise e
  
