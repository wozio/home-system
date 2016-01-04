'use strict';

angular.module('app.services',[
  'app.data'
])

.controller('ServicesCtrl', [
  '$scope', '$rootScope', 'DataSrv',
  function ($scope, $rootScope, DataSrv) {
    DataSrv.send("get_services", null, function(result, recv_data) {
      if (result === "success") {
        $scope.services = recv_data.services;
      } else if (result === "timeout") {
          console.log("Communication timed out");
      } else if (result === "failed") {
        console.log("Communication failed with reason: " + recv_data);
      }
      $rootScope.isRouteLoading = false;
    });
    //window.setInterval(function(){ DataSrv.get(); }, 5000);
    
    $scope.updateSetting = function(service, setting, settingValue){
  	  console.log(service + " " + setting + " " + settingValue);
  	  DataSrv.send(service, setting, settingValue);
    };
  }
]);
