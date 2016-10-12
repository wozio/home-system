'use strict';

angular.module('app.epg',[
  'app.data'
])

.controller('EpgCtrl', [
  '$scope', '$rootScope', 'DataSrv', '$interval',
  function ($scope, $rootScope, DataSrv, $interval) {

    $scope.viewLoading = true;

    var srv = "tv";
    var get = function() {
      $rootScope.error = false;
      $rootScope.loading = true;
      DataSrv.send(srv, "get_channels", null, function(result) {
        if (result.success) {
          $scope.channels = result.data.channel;
          $scope.names = result.data.name;
        } else {
          console.log("Communication failed with reason: " + result.reason);
          $rootScope.error = true;
          $rootScope.errorSlogan = "Communication with Home failed, reason: " + result.reason;
        }
        $rootScope.loading = false;
      });
    };
    
    //get();
    
    //var interval = $interval(function(){ get(); }, 5000);
    
    var sessionId = -1;
    var serviceAvailabilitySubscrId = DataSrv.registerServiceAvailability(function(service, available){
      console.log("S:" + service + " A:" + available);
      if (service === "tv") {
        if (available === true) {
          DataSrv.send(srv, "create_session", {
            "destination":DataSrv.getClientId(),
            "channel":1
          }, function(result){
            if (result.success) {
              sessionId = parseInt(result.data.session);
            } else {
              $scope.viewLoading = false;
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
