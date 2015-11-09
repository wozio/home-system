angular.module('app', [
      'ngWebSocket'
])
.factory('MyData', function($websocket) {
  var loc = window.location, new_uri;
  if (loc.protocol === "https:") {
      new_uri = "wss:";
  } else {
      new_uri = "ws:";
  }
  new_uri += "//" + loc.host;
  new_uri += "/access";
  console.log(new_uri);
  var dataStream = $websocket(new_uri);

  var services = [];

  dataStream.onMessage(function(message) {
    recvServices = JSON.parse(message.data).services;
    services.splice(0, services.length);
    for (i = 0; i < recvServices.length; i++) { 
      services.push(recvServices[i]);
    }
    console.log(services);
  });

  var methods = {
    services: services,
    get: function() {
      dataStream.send(JSON.stringify({
        service: 'control-server',
        message: 'get_services',
        expect_reply: true
      }));
    }
  };

  return methods;
})
.controller('ServicesCtrl', function ($scope, MyData) {
  $scope.MyData = MyData;
  MyData.get();
});
