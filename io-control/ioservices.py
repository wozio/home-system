#!/usr/bin/env python

import logging
import ioservice
import configuration

ioservices = {}

def init():
    for s in configuration.services:
        ioservices[s["name"]] = ioservice.ioservice(s["name"], s["settings"])

def exit():
    pass
