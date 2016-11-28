'use strict';

angular.module('app.data',[
  'ngCookies'
])

.factory('DataSrv', function ($cookies, $timeout, $rootScope, $location) {
  
  // firefox WS bug workaround
  $(window).on('beforeunload', function(){
    dataStream.close();
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
  
  var dataStream;
  
  // sequence number and queue of callbacks indexed by sequence number
  var seq = 0;
  var queue = {};
  
  // incoming message subcribers
  var incoming = {};
  
  // service availability subscribers
  var service_callbacks = new Map();
  
  //connection related variables
  var clientId = "";
  var connected = false;
  var connectionCallback = null;
  
  // authorization related variables
  $rootScope.loggedIn = false;
  var loggingIn = false;
  
  function connect(){
    console.log("Connecting to: " + newUri);
    dataStream = new WebSocket(newUri);
    //dataStream.binaryType = 'arraybuffer';
    
    dataStream.onclose = function() {
      console.log("WebSocket connection closed");
      $rootScope.$apply(function () {
        $rootScope.error = true;
        $rootScope.errorSlogan = "Disconnected from WebSocket.";
      });
      connected = false;
      $rootScope.loggedIn = false;
      dataStream = null;
      $timeout(connect, 1000);
      for (var s in queue) {
        if (queue.hasOwnProperty(s)) {
          $timeout.cancel(queue[s].timeout);
          queue[s].callback({
              success: false,
              reason: "Disconnected from WebSocket"
            });
        }
      }
      queue = {};
    };

    dataStream.onopen = function() {
      console.log("WebSocket connection opened");
      $rootScope.$apply(function () {
        $rootScope.error = false;
        $rootScope.errorSlogan = "";
      });
      connected = true;
      if (connectionCallback) {
        connectionCallback();
        connectionCallback = null;
      }
      // now we can try to authorize
      if (!$rootScope.loggedIn && !loggingIn) {
        methods.check(function(result){
        });
      }
    };

    dataStream.onmessage = function(message) {
      //console.log("Received " + message.data.byteLength + " bytes");
      
      //var dataView = new DataView(message.data);
      //var decoder = new TextDecoder("UTF-8");
      //var data_str = decoder.decode(dataView);

      var data_str = message.data;

      if (data_str === "ping\0") {
        return;
      }
      
      //console.log(data_str);
      
      var recv_msg = JSON.parse(data_str);
      $rootScope.error = false;
      $rootScope.errorSlogan = "";
      if (recv_msg.sequence_number !== undefined) {
        if (queue[recv_msg.sequence_number] !== undefined && recv_msg.result !== undefined) {
          // this is reply message
          console.log("Received reply for sequence number: " + recv_msg.sequence_number +
            ", RTT: " + (Date.now() - queue[recv_msg.sequence_number].sent_time) + " ms");
          if (recv_msg.result === "success") {
            $rootScope.error = false;
            $rootScope.errorSlogan = "";
            //console.log(recv_msg.params)
            queue[recv_msg.sequence_number].callback({
              success: true,
              data: recv_msg.params
            });
          } else {
            console.log("Rejected: " + recv_msg.reason);
            if (connected) {
              $rootScope.error = true;
              $rootScope.errorSlogan = "Message rejected with '" + recv_msg.reason + "' reason";
            }
            queue[recv_msg.sequence_number].callback({
              success: false,
              reason: recv_msg.reason
            });
          }
          $timeout.cancel(queue[recv_msg.sequence_number].timeout);
          delete queue[recv_msg.sequence_number];
        } else if (recv_msg.message !== undefined) {
          console.log("Received message '" + recv_msg.message + "'");
          // this is incoming message expecting reply
          if (incoming[recv_msg.message] !== undefined) {
            incoming[recv_msg.message](recv_msg, function(recv_msg, result, paramsOrReason) {
              if ($rootScope.loggedIn) {
                if (result.success) {
                  var prepared_msg = {
                    source: clientId,
                    target: recv_msg.source,
                    sequence_number: recv_msg.sequence_number,
                    result: "success"
                  }
                  if (paramsOrReason) {
                    prepared_msg["parameters"] = paramsOrReason;
                  }
                  console.log("Sending successfull reply for '" + recv_msg.message + "' message with sequence id = " + recv_msg.sequence_number);
                  dataStream.send(JSON.stringify(prepared_msg));
                } else {
                  var prepared_msg = {
                    source: clientId,
                    target: recv_msg.source,
                    sequence_number: recv_msg.sequence_number,
                    result: "failed",
                    reason: paramsOrReason
                  }
                  console.log("Sending failed reply for '" + recv_msg.message + "' message with sequence id = " + recv_msg.sequence_number + " and reason " + paramsOrReason);
                  dataStream.send(JSON.stringify(prepared_msg));
                }
              }
            });
          } else {
            console.log("Received message '" + recv_msg.message + "' for which there is no registered receiver");
          }
        }
      } else if (recv_msg.message !== undefined) {
        console.log("Received one way message '" + recv_msg.message + "'");
        // incoming one way message
        if (incoming[recv_msg.message] !== undefined) {
          incoming[recv_msg.message](recv_msg);
        } else {
          console.log("Received one way message '" + recv_msg.message + "' for which there is no registered receiver");
        }
      }
      $rootScope.$digest();
    };
  }
  
  function on_timeout(seq){
      console.log("Sequence " + seq + " timed out");
      if (connected){
        $rootScope.error = true;
        $rootScope.errorSlogan = "Communication timed out.";
      }
      queue[seq].callback({
        success: false,
        reason: "timed out"
      });
      delete queue[seq];
  }
  
  function send_internal(target, msg, params, reply_callback){
    if (connected) {
      var prepared_msg = {
          //service: 'control-server',
        source: clientId,
        target: target,
        message: msg
      }
      if (params) {
        prepared_msg["parameters"] = params;
      }
      var seq_str = "";
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
        seq_str = " (Sequence " + seq + ")";
        seq++;
      }
      console.log("Sending '" + msg + "' to '" + target + "'" + seq_str);
      dataStream.send(JSON.stringify(prepared_msg));
    } else {
      if (reply_callback) {
        reply_callback({
          success: false,
          reason: "disconnected"
        });
      }
    }
  }

  var methods = {
    send: function(target, msg, params, reply_callback) {
      // first check if user is logged in, if it is send message, reject otherwise
      methods.check(function (result) {
        if (result.success) {
          send_internal(target, msg, params, reply_callback);
        } else {
          if (reply_callback){
            reply_callback({
              success: false,
              reason: "User not logged in"
            });
          }
        }
      });
    },
    
    // check if user is logged in and try to login from cookies if it is not
    check: function(callback) {
      console.log("Check user");
      if ($rootScope.loggedIn) {
        callback({ success: true });
      } else {
        var user = $cookies.getObject('user') || {};
        if (user.email && user.password) {
          methods.login(user.email, user.password, function(result){
            callback(result);
          });
        } else {
          callback({ success: false });
        }
      }
    },
    
    // login the user with credentials provided in arguments
    login: function(email, password, callback) {
      console.log("logging in: " + email);
      loggingIn = true;

      function login_int() {
        send_internal("control-server", "login", {
          email: email,
          password: password
        }, function (result) {
          if (!result.success) {
            console.log("Failed to login with result: " + result.reason);
            $rootScope.loggedIn = false;
            $rootScope.name = "";
            loggingIn = false;
            clientId = "";
          } else {
            console.log("successfully logged in with client id: " + result.data.client_id);
            $rootScope.loggedIn = true;
            $rootScope.name = result.data.name;
            loggingIn = false;
            $cookies.putObject('user', {
              email: email,
              password: password
            });
            clientId = result.data.client_id;
          }
          callback(result);
        }, 1000);
      };

      if (connected) {
        login_int();
      } else {
        connectionCallback = function () {
          login_int();
        }
      }
    },
  
    logout: function () {
      $rootScope.loggedIn = false;
      $cookies.remove('user');
      clientId = "";
      BinaryDataSrv.disconnect();
    },
    
    register: function(message, callback) {
      incoming[message] = callback;
    },
    
    unregister: function(message) {
      delete incoming[message];
    },
    
    getClientId: function(){
      return clientId;
    },
    
    registerServiceAvailability: function(callback){
      var i = 0;
      while (service_callbacks.get(i) !== undefined){
        i++;
      }
      service_callbacks.set(i, callback);
      for (var service of available_services){
        callback(service, true);
      }
      return i;
    },
    
    unregisterServiceAvailability: function(SubscriptionId){
      service_callbacks.delete(SubscriptionId);
    }
  };
  
  methods.register("logout_complete", function() {
    methods.logout();
    $location.path("/login");
  });
  
  var available_services = new Set();
  
  methods.register("service_availability", function(message) {
    if (message.params.available === true) {
      console.log("Service available: " + message.params.service);
      available_services.add(message.params.service);
      for (var callback of service_callbacks.values()) {
        callback(message.params.service, true);
      }
    } else {
      console.log("Service not available: " + message.params.service);
      available_services.delete(message.params.service);
      for (var callback of service_callbacks.values()) {
        callback(message.params.service, false);
      }
    }
  });
  
  connect();

  return methods;
});
