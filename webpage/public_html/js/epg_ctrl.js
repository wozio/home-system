'use strict';

angular.module('app.epg',[
  'app.data'
])

.controller('EpgCtrl', [
  '$scope', 'DataSrv', '$interval',
  function ($scope, DataSrv, $interval) {
	  console.log("EpgCtrl");

    var srv = "tv";
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
      if (service === "tv") {
        if (available === true) {
          DataSrv.send(srv, "create_session", {
            "destination":DataSrv.getClientId(),
            "channel":1
          }, function(result){
            $scope.viewLoaded();
            if (result.success) {
              sessionId = parseInt(result.data.session);
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
