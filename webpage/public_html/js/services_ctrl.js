'use strict';

/* Controllers */

var myHomeControllers = angular.module('myHome', [
	'ngWebSocket'
])

myHomeControllers.factory('MyData', function($websocket) {
  var loc = window.location, new_uri;
  if (loc.protocol === "https:") {
      new_uri = "wss:";
  } else {
      new_uri = "ws:";
  }
  new_uri += "//" + loc.host;
  new_uri += "/access/client/";
  console.log(new_uri);
  var dataStream = $websocket(new_uri);

  var services = [];

  dataStream.onMessage(function(message) {
    //console.log("Received: " + message.data);
    recvServices = JSON.parse(message.data).services;
    services.splice(0, services.length);
    for (i = 0; i < recvServices.length; i++) { 
      services.push(recvServices[i]);
    }
    //console.log(services);
  });

  // firefox WS bug workaround
  $(window).on('beforeunload', function(){
    dataStream.close();
  });

  var methods = {
    services: services,
    get: function() {
      dataStream.send(JSON.stringify({
        service: 'io-control-dev',
        message: 'get_services',
        expect_reply: true
      }));
    },
    set_setting: function(service, setting, settingValue) {
      console.log("Sending set setting request");
      dataStream.send(JSON.stringify({
        service: "io-control-dev",
        message: "set_setting_value",
        expect_reply: false,
        parameters: {
        	service: service,
        	setting: setting,
        	value: settingValue
        }
      }));
    }
  };

  return methods;
})

myHomeControllers.controller('ServicesCtrl', function ($scope, MyData) {
  $scope.MyData = MyData;
  MyData.get();
  window.setInterval(function(){ MyData.get(); }, 5000);
  
  $scope.updateSetting = function(service, setting, settingValue){
	  console.log(service + " " + setting + " " + settingValue);
	  MyData.set_setting(service, setting, settingValue);
  };
  
});
