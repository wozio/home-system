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
      $scope.service = message.params;
    });
    
    var serviceAvailabilitySubscrId = DataSrv.registerServiceAvailability(function(service, available){
      if (service === srv) {
        if (available === true) {
          console.log("Service '" + srv + "' is available, subscribing for service change notification");
          DataSrv.send(srv, "subscribe", {
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
