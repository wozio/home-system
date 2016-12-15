import json
from websocket import create_connection
for i in range(0,100):
    ws = create_connection("ws://192.168.1.2:8080/access/client")
    login_msg = json.dumps({
        'message': 'login',
        'sequence_number': 0,
        'source': "test_source",
        'target': 'control-server',
        'parameters':{
            'email': "test@test",
            'password': "test"
        }
    })
    print "Sending: %s" % login_msg
    ws.send(login_msg)
    print "Sent"
    print "Receiving..."
    result = ws.recv()
    print "Received '%s'" % result
    ws.close()