#!/usr/bin/env python

import logging
import ioservice
import configuration

Ioservices = {}
callbacks = []

def init():
  for s in configuration.services:
    new_service = ioservice.ioservice(s["name"],
      s["displays"], s["settings"])
    new_service.subscribe(ioservice_change)
    Ioservices[s["name"]] = new_service

def exit():
  for s in Ioservices.itervalues():
    s.exit()
  
def subscribe(callback):
  callbacks.append(callback)
  
def ioservice_change(service):
  for c in callbacks:
    c(service)
