'use strict';

angular.module('app.services',[
  'app.data'
])

.controller('ServicesCtrl', [
  '$scope', 'DataSrv', '$interval',
  function ($scope, DataSrv, $interval) {
    var subscriptionId = -1;
    var srv = "io-control-dev";
    var get = function() {
      DataSrv.send(srv, "get_services", null, function(result) {
        if (result.success) {
          //$scope.services = result.data.services;
        }
      });
    };
    
    DataSrv.register("services_change", function(message) {
      $scope.services = message.params.services;
    });
    
    DataSrv.send("io-control-dev", "subscribe_services", {"service":DataSrv.getClientId()}, function(result){
      subscriptionId = parseInt(result.data.id);
    });
    
    //get();
    
    //var interval = $interval(function(){ get(); }, 5000);
    
    $scope.$on("$destroy", function(){
      //$interval.cancel(interval);
      DataSrv.send("io-control-dev", "unsubscribe_services", {"id":subscriptionId});
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
