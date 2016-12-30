import traceback
import unittest
import time
import random
import threading
import service
import yami
import yagent
import subprocess
import discovery

class test_io:

    def __init__(self):
        self.name = "test.client"
        self.s = None
        self.e = None
        self.subscribed = threading.Condition()

        self.serv = service.service(self.name, self.on_msg)

    def exit(self):
        self.serv.exit()

    def send_value(self, v):
        if self.s and self.e:
            try:
                params = yami.Parameters()
                params["name"] = self.name
                params["id"] = 1
                params["type"] = 0 #temperature input
                params["state"] = 1
                params["value"] = v
                yagent.agent.send(self.e, self.s,
                    "state_change", params)
            except yami.YAMIError as e:
                self.s = None
                self.e = None

    def on_msg(self, msg):
        try:
            if msg.get_message_name() == "subscribe":
                self.s = msg.get_parameters()["name"]
                self.e = msg.get_parameters()["endpoint"]
                with self.subscribed:
                    self.subscribed.notifyAll()
                
        except Exception as e:
            logging.error(traceback.format_exc())
            raise e

class test_client:

    def __init__(self):
        self.name = "io.test"
        self.s = None
        self.e = None
        self.service_found = threading.Condition()
        
        self.serv = service.service(self.name, self.on_msg)

        discovery.register(self.on_service_availability)

    def exit(self):
        self.serv.exit()

    def on_msg(self, msg):
        pass

    def on_service_availability(self, s, available):
        if s == "io-control-dev" and available == True:
            with self.service_found:
                self.service_found.notifyAll()

class TestStringMethods(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        discovery.init()
        # create test io object
        cls.input = test_io()
        # create test client object
        cls.client = test_client()
        # run process under test
        args = ['python', 'iocontrol.py']
        cls.p = subprocess.Popen(args=args, stdin=subprocess.PIPE)
        #wait for iocontrol to subscribe for io_test
        with cls.input.subscribed:
            cls.input.subscribed.wait()
        with cls.client.service_found:
            cls.client.service_found.wait()

    @classmethod
    def tearDownClass(cls):
        cls.input.exit()
        cls.client.exit()
        discovery.exit()

        def quit_thread():
            print "requesting process to quit"
            cls.p.communicate("q")
        qt = threading.Thread(target=quit_thread)
        qt.start()
        qt.join(2)
        if qt.is_alive():
            cls.p.terminate()
            qt.join()
            raise

    def test_history(self):
        # send 1000 updates
        sent_values = []
        for i in range(1000):
            v = random.uniform(-30, 30)
            sent_values.append(v)
            self.input.send_value(v)

if __name__ == '__main__':
    unittest.main()