#!/usr/bin/env python

import logging
import logging.handlers
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
  formatter = logging.Formatter('[%(asctime)s] [%(levelname)s] [%(thread)d] [%(filename)s:%(lineno)d] %(message)s')

  fh = logging.handlers.RotatingFileHandler('/var/log/home-system/iocontrol.log', maxBytes=1*1024*1024, backupCount=1)
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
  inputs.exit()
  mainservice.exit()
  discovery.exit()
