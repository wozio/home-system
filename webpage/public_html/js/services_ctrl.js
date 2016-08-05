'use strict';

angular.module('app.services',[
  'app.data'
])

.controller('ServicesCtrl', [
  '$scope', 'DataSrv',
  function ($scope, DataSrv) {
    $scope.viewLoading = true;
    var subscriptionId = -1;
    var srv = "io-control-dev";
    
    DataSrv.register("services_full", function(message) {
      $scope.viewLoading = false;
      $scope.services = message.params.services;
    });
    
    DataSrv.register("services_change", function(message) {
      for (var i = 0; i < $scope.services.length; i++){
        if ($scope.services[i].name === message.params.name){
          $scope.services[i] = message.params;
        }
      }
    });
    
    DataSrv.send("io-control-dev", "subscribe_services", {"service":DataSrv.getClientId()}, function(result){
      if (result.success) {
        subscriptionId = parseInt(result.data.id);
      } else {
        $scope.viewLoading = false;
      }
    });
    
    $scope.$on("$destroy", function(){
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
