'use strict';

angular.module('app.data',[
  'ngWebSocket'
])

.factory('DataSrv', function($websocket, $timeout) {
  
  // firefox WS bug workaround
  $(window).on('beforeunload', function(){
    dataStream.close();
  });
  
  var loc = window.location, new_uri;
  if (loc.protocol === "https:") {
    new_uri = "wss:";
  } else {
    new_uri = "ws:";
  }
  new_uri += "//" + loc.host;
  new_uri += "/access/client/";
  new_uri = "ws://localhost:5000";
  //console.log(new_uri);
  
  var dataStream;
  
  // sequence number and queue of callbacks indexed by sequence number
  var seq = 0;
  var queue = {};
  var on_connect_callback = null;
  var client_id = "";
  
  function connect(){
    console.log("Connecting to: " + new_uri);
    dataStream = $websocket(new_uri);
    
    dataStream.onClose(function() {
      console.log("WebSocket connection closed");
      $timeout(connect, 1000);
    });

    dataStream.onError(function() {
      console.log("WebSocket error");
    });

    dataStream.onOpen(function() {
      console.log("WebSocket connection opened");
      if (on_connect_callback !== null) {
        on_connect_callback();
      }
    });

    dataStream.onMessage(function(message) {
      console.log("Received: " + message.data);
      if (message.data === "ping\0") {
        return;
      }
      var recv_msg = JSON.parse(message.data);
      if (recv_msg.sequence_number !== undefined &&
          queue[recv_msg.sequence_number] !== undefined) {
        if (recv_msg.result !== undefined) {
          if (recv_msg.result === "success") {
            console.log("Received reply for sequence number: " + recv_msg.sequence_number +
              ", RTT: " + (Date.now() - queue[recv_msg.sequence_number].sent_time) + " ms");
            queue[recv_msg.sequence_number].callback({
              success: true,
              data: recv_msg.params
            });
          } else {
            console.log("Received failed result for sequence number: " + recv_msg.sequence_number + ": " + recv_msg.reason);
            queue[recv_msg.sequence_number].callback({
              success: false,
              reason: recv_msg.reason
            });
          }
        }
        $timeout.cancel(queue[recv_msg.sequence_number].timeout);
        delete queue[recv_msg.sequence_number];
      }
    });
  }
  
  function on_timeout(seq){
      console.log("Sequence " + seq + " timed out");
      queue[seq].callback({
        success: false,
        reason: "timed out"
      });
      delete queue[seq];
  }

  var methods = {
    send: function(target, msg, params, reply_callback) {
      var prepared_msg = {
          //service: 'control-server',
        source: client_id,
        target: target,
        message: msg
      }
      if (params) {
        prepared_msg["parameters"] = params;
      }
      if (reply_callback) {
        prepared_msg["expect_reply"] = true;
        prepared_msg["sequence_number"] = seq;
        var n = Date.now();
        queue[seq] = {
          callback: reply_callback,
          timeout: $timeout(
            on_timeout,
            5000,
            true,
            seq),
          sent_time: n
        }
        seq++;
      }
      console.log("Sending: " + JSON.stringify(prepared_msg));
      dataStream.send(JSON.stringify(prepared_msg));
    },
    set_client_id: function(client_id_) {
      client_id = client_id_;
    },
    on_connect: function(on_connect_callback_) {
      on_connect_callback = on_connect_callback_;
    }
  };
  
  connect();

  return methods;
});
