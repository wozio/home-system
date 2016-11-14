'use strict';

angular.module('app.service',[
  'app.data'
])

.controller('ServiceCtrl',[
  '$scope', '$routeParams', 'DataSrv',
  function ($scope, $routeParams, DataSrv) {
    var subscriptionId = null;
    var srv = "io-control.service." + $routeParams.serviceId;
    console.log("ServiceCtrl (" + $routeParams.serviceId + ")");
    $scope.service = {
        name: $routeParams.serviceId
    };

    DataSrv.register("service_full", function(message) {
      $scope.service = message.params.service;
    });
    
    DataSrv.register("service_change", function(message) {
    });
    
    var serviceAvailabilitySubscrId = DataSrv.registerServiceAvailability(function(service, available){
      console.log(service + " is " + available);
      if (service === srv) {
        if (available === true) {
          DataSrv.send(srv, "register_service", {
              "service":DataSrv.getClientId()
            }, function(result){
            if (result.success) {
              subscriptionId = parseInt(result.data.id);
            } else {
              subscriptionId = null;
              $scope.viewLoading = false;
            }
          });
        }
      }
    });

    $scope.$on("$destroy", function(){
      DataSrv.unregisterServiceAvailability(serviceAvailabilitySubscrId);
      if (subscriptionId !== null){
        DataSrv.send(srv, "unsubscribe_service", {"id":subscriptionId});
      }
    });
  }
]);
