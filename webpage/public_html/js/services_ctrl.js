'use strict';

angular.module('app.services',[
  'app.data'
])

.controller('ServicesCtrl', [
  '$scope', '$rootScope', 'DataSrv',
  function ($scope, $rootScope, DataSrv) {
    DataSrv.send("get_services", null, function(recv_data) {
      if (recv_data)
        $scope.services = recv_data.services;
      $rootScope.isRouteLoading = false;
    });
    //window.setInterval(function(){ DataSrv.get(); }, 5000);
    
    $scope.updateSetting = function(service, setting, settingValue){
  	  console.log(service + " " + setting + " " + settingValue);
  	  DataSrv.send(service, setting, settingValue);
    };
  }
]);
