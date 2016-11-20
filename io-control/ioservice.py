#!/usr/bin/env python

import logging
import traceback
import setting
import display
import service
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

    def on_msg(self, msg):
        try:
            if msg.get_message_name() == "subscribe":
                s = msg.get_parameters()["service"]
                i = self.subscriptions.add(s)
                logging.debug("service '%s' subscribed for service '%s' change notification with id %d", s, self.name, i)
                
                # reply contains id of the subscription, current service state and history of displays
                params = yami.Parameters()
                params["id"] = i
                params["service"] = self.prepare()
                if len(self.displays) > 0:
                    display_history = []
                    for d in self.displays.itervalues():
                        display_history.append(d.prepare_history())
                    params["history"] = display_history

                msg.reply(params)

            elif msg.get_message_name() == "unsubscribe":
                i = msg.get_parameters()["id"]
                self.subscriptions.remove(i)

        except Exception as e:
            logging.error(traceback.format_exc())
            raise e
  
