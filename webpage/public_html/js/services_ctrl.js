'use strict';

angular.module('app.services',[
  'app.data'
])

.controller('ServicesCtrl', [
  '$scope', '$rootScope', 'DataSrv',
  function ($scope, $rootScope, DataSrv) {
    DataSrv.send("get_services", null, function(result) {
      if (result.success) {
        $scope.services = result.data.services;
      } else {
        console.log("Communication failed with reason: " + result.reason);
        $rootScope.error = true;
        $rootScope.errorSlogan = "Communication with Home failed, reason: " + result.reason;
      }
      $rootScope.loading = false;
    });
      //window.setInterval(function(){ DataSrv.get(); }, 5000);
      
    $scope.updateSetting = function(service, setting, settingValue){
  	  console.log(service + " " + setting + " " + settingValue);
  	  DataSrv.send(service, setting, settingValue);
    };
  }
]);
