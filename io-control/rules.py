#!/usr/bin/env python

import logging
import rule
import configuration

rules = {}

def init():
    logging.debug("Rules init")

    for r in configuration.rules:
        rules[r["name"]] = rule.rule(r["name"], r["human_readable_name"], r["rule"], r["inputs"])

def exit():
    pass
