#!/usr/bin/env python

import logging
import ioservice
import configuration

Ioservices = {}

def init():
    for s in configuration.services:
        Ioservices[s["name"]] = ioservice.ioservice(s["name"],
            s["displays"], s["settings"])

def exit():
    pass
