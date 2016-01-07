'use strict';

angular.module('app.services',[
  'app.data'
])

.controller('ServicesCtrl', [
  '$scope', '$rootScope', 'DataSrv', '$interval',
  function ($scope, $rootScope, DataSrv, $interval) {
    var get = function() {
      $rootScope.error = false;
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
    }
    
    get();
    
    //var interval = $interval(function(){ get(); }, 5000);
    
    $scope.$on("$destroy", function(){
      $interval.cancel(interval);
    });
      
    $scope.updateSetting = function(service, setting, settingValue){
  	  console.log(service + " " + setting + " " + settingValue);
  	  DataSrv.send("set_setting_value", {
  	    service: service,
  	    setting: setting,
  	    value: settingValue
  	  });
    };
  }
]);
