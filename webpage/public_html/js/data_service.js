'use strict';

angular.module('app.data',[
  'ngCookies'
])

.factory('DataSrv', function ($cookies, $timeout, $rootScope, $location) {

  // to detect when do not try to reconnect and not display any error messages
  var unloading = false;
  
  $(window).on('beforeunload', function(){
    unloading = true;
    dataStream.close();
  });

  // creating uri  
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
  var connected = false;
  
  // authorization related variables
  var clientId = "";
  var loggedIn = false;
  var loggingIn = false;
  
  // connect function which tries to establish WebSocket connection
  // and register to events
  function connect(){
    console.log("Connecting to: " + newUri);
    dataStream = new WebSocket(newUri);
    
    dataStream.onclose = function() {
      console.log("WebSocket connection closed");
      if (!unloading){
        connected = false;
        loggedIn = false;
        dataStream = null;
        $rootScope.$emit("data:disconnected");
        $timeout(connect, 1000);
        // remove all messages in queue waiting for answer
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
        $rootScope.$digest();
      }
    };

    dataStream.onopen = function(){
      console.log("WebSocket connection opened");
      connected = true;
      $rootScope.$emit("data:connected");
      // now we can try to authorize
      if (!loggedIn && !loggingIn){
        methods.check(function(result){
        });
      }
      $rootScope.$digest();
    };

    dataStream.onmessage = function(message) {
      var data_str = message.data;

      if (data_str === "ping\0") {
        return;
      }
      
      //console.log(data_str);
      
      var recv_msg = JSON.parse(data_str);
      if (recv_msg.sequence_number !== undefined) {
        if (queue[recv_msg.sequence_number] !== undefined && recv_msg.result !== undefined) {
          // there is sequence number and result so this is reply message
          console.log("(Sequence " + recv_msg.sequence_number + ") received reply, RTT: " +
            (Date.now() - queue[recv_msg.sequence_number].sent_time) + " ms");
          if (recv_msg.result === "success") {
            //console.log(recv_msg.params)
            queue[recv_msg.sequence_number].callback({
              success: true,
              data: recv_msg.params
            });
          } else {
            console.log("Rejected: " + recv_msg.reason);
            queue[recv_msg.sequence_number].callback({
              success: false,
              reason: recv_msg.reason
            });
          }
          $timeout.cancel(queue[recv_msg.sequence_number].timeout);
          delete queue[recv_msg.sequence_number];
        } else if (recv_msg.message !== undefined) {
          console.log("Received message '" + recv_msg.message + "'");
          // there is message name and sequence number so this is incoming message expecting reply
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
      if (loggedIn) {
        send_internal(target, msg, params, reply_callback);
      } else {
        if (reply_callback){
          reply_callback({
            success: false,
            reason: "User not logged in"
          });
        }
      }
    },
    
    // check if user is logged in and try to login from cookies if it is not
    check: function(callback) {
      console.log("Check user");
      if (loggedIn) {
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
      if (connected) {
        console.log("logging in: " + email);
        loggingIn = true;
        $rootScope.$emit("user:loggingIn");
        send_internal("control-server", "login", {
          email: email,
          password: password
        }, function (result) {
          if (!result.success) {
            console.log("Failed to login with result: " + result.reason);
            loggedIn = false;
            loggingIn = false;
            clientId = "";
            $rootScope.$emit("user:loggedOut");
          } else {
            console.log("successfully logged in with client id: " + result.data.client_id);
            loggedIn = true;
            loggingIn = false;
            $cookies.putObject('user', {
              email: email,
              password: password
            });
            name = result.data.name;
            clientId = result.data.client_id;
            $rootScope.$emit("user:loggedIn", {name: name, clientId: clientId});
          }
          if (callback)
            callback(result);
        }, 1000);
      };
    },
  
    logout: function () {
      loggedIn = false;
      $cookies.remove('user');
      clientId = "";
      $rootScope.$emit("user:loggedOut");
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
    },

  };
  
  methods.register("logout_complete", function() {
    methods.logout();
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
