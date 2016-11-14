#!/usr/bin/env python

import logging
import setting
import display
import service

class ioservice:

  def __init__(self, name, displays_, settings_):
    self.name = name
    self.settings = {}
    self.displays = {}
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

  def __del__(self):
    self.serv = None
    pass

  def subscribe(self, callback):
    self.change_callback = callback
  
  def on_change(self, disp_or_setting):
    if self.change_callback != None:
      self.change_callback(self)

  def on_msg(self, msg):
    logging.debug("Received message: %s", msg.get_message_name())
    pass
