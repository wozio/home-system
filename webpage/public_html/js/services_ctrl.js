'use strict';

angular.module('app.services',[
  'app.data'
])

.controller('ServicesCtrl', [
  '$scope', 'DataSrv', '$interval',
  function ($scope, DataSrv, $interval) {
    var srv = "io-control-dev";
    var get = function() {
      DataSrv.send(srv, "get_services", null, function(result) {
        if (result.success) {
          $scope.services = result.data.services;
        }
      });
    };
    
    get();
    
    var interval = $interval(function(){ get(); }, 5000);
    
    $scope.$on("$destroy", function(){
      $interval.cancel(interval);
    });
      
    $scope.updateSetting = function(service, setting, settingValue){
  	  console.log(service + " " + setting + " " + settingValue);
  	  DataSrv.send(srv, "set_setting_value", {
  	    service: service,
  	    setting: setting,
  	    value: settingValue
  	  });
    };
  }
]);
