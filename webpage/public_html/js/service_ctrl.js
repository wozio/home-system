'use strict';

angular.module('app.service',[
  'app.data'
])

.controller('ServiceCtrl',[
  '$scope', '$routeParams', 'DataSrv',
  function ($scope, $routeParams, DataSrv) {
    var subscriptionId = null;
    var srv = "io-control.service." + $routeParams.serviceId;
    var srvName = $routeParams.serviceId;
    console.log("ServiceCtrl (" + srvName + ")");
    $scope.service = {
        name: $routeParams.serviceId
    };
    $scope.chartOptions = [];
    $scope.chartChange = [];
    $scope.chartData = [];

    $scope.dataLoading = true;
    $scope.dataProgress = 0;
    var dataReceived = 0;
    var dataTotal = 0;

    var serviceAvailabilitySubscrId = DataSrv.registerServiceAvailability(function(service, available){
      if (service === srv) {
        if (available === true) {
          console.log("Service '" + srv + "' is available, subscribing for service change notification");
          DataSrv.send(srv, "subscribe", {
              "service":DataSrv.getClientId()
          }, function(result){
            $scope.viewLoaded();
            if (result.success) {
              subscriptionId = parseInt(result.data.id);
              console.log("Subscribed with id="+subscriptionId);

              // creating charts, empty for now, data will be initially written
              // on service_history and then updated after each service_change
              var displays = result.data.service.displays;
              for (var j = 0; j < displays.length; j++){
                
                var newChartOptions = {
                  "name": displays[j].name,
                  "type": displays[j].type
                }
                $scope.chartOptions.push(newChartOptions);
                $scope.chartData.push([]);
                $scope.chartChange.push(0);
              }

              $scope.service = result.data.service;
            } else {
              subscriptionId = null;
            }
          });
        } else {
          subscriptionId = null;
          $scope.service = {
            name: $routeParams.serviceId
          };
          $scope.chartOptions = [];
          $scope.chartChange = [];
          $scope.chartData = [];
        }
      }
    });

    DataSrv.register("service_history_info", function(message) {
      if (message.params.name === srvName){
        console.log(message.params.msg_number);
        dataTotal = message.params.msg_number;
      }
    });

    DataSrv.register("service_history", function(message) {
      if (message.params.name === srvName){
        // update progress bar
        dataReceived++;
        if (dataTotal > 0){
          $scope.dataProgress = dataReceived / dataTotal * 100;
        }
        // search for matching display
        for (var j = 0; j < $scope.chartOptions.length; j++){
          if ($scope.chartOptions[j].name === message.params.display_name){
            for (var i = 0; i < message.params.history.length; i++){
              if (message.params.history[i].state === 1){
                var val = message.params.history[i].value;
                var valueDate = new Date(message.params.history[i].time * 1000);
                $scope.chartData[j].push({
                  date: valueDate,
                  value: val
                });
              }
            }
            //$scope.chartChange[j]++;
          }
        }
        if (dataReceived === dataTotal){
          $scope.dataLoading = false;
          for (var j = 0; j < $scope.chartChange.length; j++){
            $scope.chartChange[j]++;
          }
        }
      }
    });

    DataSrv.register("service_change", function(message) {
      if (message.params.name === srvName){
        $scope.service = message.params;
        for (var j = 0; j < message.params.displays.length; j++){
          if (message.params.displays[j].state == 1){
            for (var i = 0; i < $scope.chartOptions.length; i++){
              if ($scope.chartOptions[i].name === message.params.displays[j].name){
                var newDate = new Date();
                $scope.chartData[i].push({
                  date: newDate,
                  value:  message.params.displays[j].value
                });
                $scope.chartChange[i]++;
                break;
              }
            }
          }
        }
      }
    });

    $scope.$on("$destroy", function(){
      DataSrv.unregisterServiceAvailability(serviceAvailabilitySubscrId);
      if (subscriptionId !== null){
        DataSrv.send(srv, "unsubscribe", {"id":subscriptionId});
      }
    });
  }
]);

