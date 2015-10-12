#!/usr/bin/env python

import logging
import yagent
import discovery
import outputs
import inputs
import rules
import ioservices
import mainservice

def init(daemonize):
  logger = logging.getLogger()
  logger.setLevel(logging.DEBUG)
  formatter = logging.Formatter('[%(levelname)s] %(asctime)s %(filename)s: %(lineno)d: %(message)s')

  fh = logging.FileHandler('iocontrol.log')
  fh.setFormatter(formatter)
  logger.addHandler(fh)

  if not daemonize:
    ch = logging.StreamHandler()
    ch.setFormatter(formatter)
    logger.addHandler(ch)

  logging.info("Starting Home System IO Control")

  discovery.init()
  outputs.init()
  inputs.init()
  rules.init()
  ioservices.init()
  mainservice.init()

def exit():
  logging.info("Home System IO Control quitting")

  yagent.agent = None
  mainservice.exit()
  discovery.exit()

