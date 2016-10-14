'use strict';

angular.module('app.binarydata', [
])

.factory('BinaryDataSrv', function ($timeout, $rootScope) {

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

  $rootScope.speed = 0.0;
  var data_counter = 0.0;
  function check_data() {
    $timeout(function () {
      $rootScope.speed = data_counter*4 / 1048576;
      data_counter = 0.0;
      check_data();
    }, 250);
  };
  check_data();

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
      if (message.data.byteLength > 0) {
        //console.log("Received " + message.data.byteLength + " bytes");
        data_counter += message.data.byteLength;

        if (dataCallback) {
          dataCallback(message.data)
        }
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
