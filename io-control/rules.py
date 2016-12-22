#!/usr/bin/env python

import logging
import rule
import configuration

rules = {}

def init():
    logging.debug("Rules init")

    for r in configuration.rules:
        rules[r["name"]] = rule.rule(r["name"], r["rule"], r["inputs"], r["outputs"])

def exit():
    pass
