'use strict';

angular.module('app.epg',[
  'app.data',
  'app.binarydata'
])

.controller('EpgCtrl', [
  '$scope', 'DataSrv', 'BinaryDataSrv', '$interval',
  function ($scope, DataSrv, BinaryDataSrv, $interval) {
	  console.log("EpgCtrl");
	$scope.viewLoading = false;
    var srv = "test-source";
    var get = function() {
      DataSrv.send(srv, "get_channels", null, function(result) {
        if (result.success) {
          $scope.channels = result.data.channel;
          $scope.names = result.data.name;
        } else {
          console.log("Communication failed with reason: " + result.reason);
        }
      });
    };
    
    //get();
    
    //var interval = $interval(function(){ get(); }, 5000);
    
    var sessionId = -1;
    var serviceAvailabilitySubscrId = DataSrv.registerServiceAvailability(function(service, available){
      if (service === srv) {
        if (available === true) {
          DataSrv.send(srv, "create_session", {
            "destination": DataSrv.getClientId(),
            "channel": 1
          }, function (result) {
            if (result.success) {
              console.log("Session created with id " + parseInt(result.data.id));
              sessionId = parseInt(result.data.id);
              BinaryDataSrv.connect(DataSrv.getClientId());
            } else {
            }
          });
        }
      }
    });
    
    $scope.$on("$destroy", function(){
      //$interval.cancel(interval);
      DataSrv.unregisterServiceAvailability(serviceAvailabilitySubscrId);
    });
  }
]);
