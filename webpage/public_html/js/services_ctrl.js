'use strict';

angular.module('app.services',[
  'app.data'
])

.controller('ServicesCtrl',[
  '$scope', 'DataSrv',
  function ($scope, DataSrv) {
	  console.log("ServicesCtrl");
    var subscriptionId = -1;
    var srv = "io-control-dev";
    
    DataSrv.register("services_full", function(message) {
      $scope.services = message.params.services;
    });
    
    DataSrv.register("services_change", function(message) {
      for (var i = 0; i < $scope.services.length; i++){
        if ($scope.services[i].name === message.params.name){
          $scope.services[i] = message.params;
        }
      }
    });
    
    var serviceAvailabilitySubscrId = DataSrv.registerServiceAvailability(function(service, available){
      if (service === srv) {
        if (available === true) {
          DataSrv.send(srv, "subscribe_services", {"service":DataSrv.getClientId()}, function(result){
            $scope.viewLoaded();
            if (result.success) {
              subscriptionId = parseInt(result.data.id);
            }
          });
        }
      }
    });
    
    $scope.$on("$destroy", function(){
      DataSrv.unregisterServiceAvailability(serviceAvailabilitySubscrId);
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
