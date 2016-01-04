'use strict';

angular.module('app.data',[
  'ngWebSocket'
])

.factory('DataSrv', function($websocket) {
  
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
  //console.log(new_uri);
  
  var dataStream = $websocket(new_uri);

  // sequence number and queue of callbacks indexed by sequence number
  var seq = 0;
  var queue = {};

  dataStream.onMessage(function(message) {
    console.log("Received: " + message.data);
    var recv_msg = JSON.parse(message.data);
    if (recv_msg.sequence_number !== undefined) {
      if (recv_msg.result !== undefined) {
        if (recv_msg.result === "success") {
          console.log("Received reply for sequence number: " + recv_msg.sequence_number);
          queue[recv_msg.sequence_number](recv_msg.params);
        } else {
          console.log("Received failed result for sequence number: " + recv_msg.sequence_number + ": " + recv_msg.reason);
          queue[recv_msg.sequence_number](null);
        }
      }
    }
  });

  var methods = {
    send: function(msg, params, reply_callback) {
      var prepared_msg = {
          //service: 'control-server',
        service: 'io-control-dev',
        message: msg
      }
      if (params) {
        prepared_msg["params"] = params;
      }
      if (reply_callback) {
        prepared_msg["expect_reply"] = true;
        prepared_msg["sequence_number"] = seq;
        queue[seq] = reply_callback;
        seq++;
      }
      console.log("Sending: " + JSON.stringify(prepared_msg));
      dataStream.send(JSON.stringify(prepared_msg));
    }
  };

  return methods;
});
