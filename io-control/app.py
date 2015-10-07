#!/usr/bin/env python

import logging
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

  fh = logging.FileHandler('io-control.log')
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

  mainservice.exit()
  ioservices.exit()
  rules.exit()
  inputs.exit()
  outputs.exit()
  discovery.exit()
