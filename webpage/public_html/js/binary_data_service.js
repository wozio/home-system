'use strict';

angular.module('app.binarydata', [
])

.factory('BinaryDataSrv', function ($timeout) {

  // firefox WS bug workaround
  $(window).on('beforeunload', function () {
    if (connected) {
      dataStream.close();
    }
  });

  var loc = window.location, newUri;
  if (loc.protocol === "https:") {
    newUri = "wss:";
  } else {
    newUri = "ws:";
  }
  newUri += "//" + loc.host;
  newUri += "/access/client/";
  //newUri = "ws://localhost:5000";
  //console.log(newUri);

  var connected = false;
  var clientId = null;
  var dataStream = null;
  var dataCallback = null;

  function connect() {
    if (clientId === null) {
      return;
    }
    var uri = newUri + clientId;
    console.log("Binary connecting to: " + uri);
    dataStream = new WebSocket(uri);
    dataStream.binaryType = 'arraybuffer';

    dataStream.onclose = function () {
      console.log("Binary WebSocket connection closed");
      connected = false;
      dataStream = null;
      if (clientId !== null) {
        $timeout(connect, 1000);
      }
    };

    dataStream.onopen = function () {
      console.log("Binary WebSocket connection opened");
      connected = true;
    };

    dataStream.onmessage = function (message) {
      if (message.data === "ping\0") {
        return;
      }
      console.log("Received " + message.data.byteLength + " bytes");

      var dv = new DataView(message.data);
      for (var i = 0; i < message.data.byteLength, i++){
        console.log(dv.getUint8(i));
      }

      if (dataCallback) {
        dataCallback(message.data)
      }
    };
  }

  function send_internal(data) {
    if (connected) {
      console.log("Sending " + data.byteLength + " bytes");
      dataStream.send(data);
    }
  }

  var methods = {
    connect(id) {
      clientId = null;
      if (connected) {
        console.log("Binary WebSocket disconnecting");
        dataStream.close();
        dataStream = null;
      }
      clientId = id;
      connect();
    },

    disconnect() {
      clientId = null;
      if (connected) {
        console.log("Binary WebSocket disconnecting");
        dataStream.close();
        dataStream = null;
      }
    },

    send: function(data) {
      send_internal(data);
    },

    register: function(callback) {
      dataCallback = callback;
    },

    unregister: function() {
      dataCallback = null;
    },
  };

  connect();

  return methods;
});
