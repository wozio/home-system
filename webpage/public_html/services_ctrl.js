angular.module('app', [
      'ngWebSocket'
])
.factory('MyData', function($websocket) {
  var dataStream = $websocket('ws://192.168.1.2:8080/access');

  var collection = [];

  dataStream.onMessage(function(message) {
    recvServices = JSON.parse(message.data).services;
    collection.splice(0, collection.length);
    for (i = 0; i < recvServices.length; i++) { 
      collection.push(recvServices[i]);
    }
  });

  var methods = {
    collection: collection,
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
