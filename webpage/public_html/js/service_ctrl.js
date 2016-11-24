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

    var serviceAvailabilitySubscrId = DataSrv.registerServiceAvailability(function(service, available){
      if (service === srv) {
        if (available === true) {
          console.log("Service '" + srv + "' is available, subscribing for service change notification");
          DataSrv.send(srv, "subscribe", {
              "service":DataSrv.getClientId()
            }, function(result){
            if (result.success) {
              
              subscriptionId = parseInt(result.data.id);
              console.log("Subscribed with id="+subscriptionId);

              // creating charts, empty for now, data will be initially written
              // on service_history and then updated after each service_change
              var displays = result.data.service.displays;
              for (var j = 0; j < displays.length; j++){
                
                var newChartOptions = {
                  "name": displays[j].name,
                  "type": "serial",
                  "theme": "light",
                  "marginRight": 40,
                  "autoMarginOffset": 20,
                  "marginTop": 7,
                  "creditsPosition": "top-left",
                  "titles": [{
                      "text": displays[j].name
                  }],
                  "valueAxes": [{
                      "axisAlpha": 0.2,
                      "dashLength": 1,
                      "position": "left"
                  }],
                  "mouseWheelZoomEnabled": true,
                  "graphs": [{
                      "id": "g1",
                      "balloonText": "<div style='margin:3px; font-size:11px;'>[[value]]&#176;C</div>",
                      "valueField": "value",
                      "lineThickness": 2,
                      "type": "step"
                  }],
                  "chartCursor": {
                    "limitToGraph":"g1",
                    "categoryBalloonDateFormat": "JJ:NN:SS, DD MMMM",
                    "cursorPosition": "mouse"
                  },
                  "categoryField": "date",
                  "categoryAxis": {
                      "minPeriod": "ss",
                      "parseDates": true,
                      "axisColor": "#DADADA",
                      "dashLength": 1,
                      "minorGridEnabled": true
                  }
                };
                $scope.chartOptions.push(newChartOptions);
                $scope.chartChange.push(0);
              }

              $scope.service = result.data.service;
              
              
            } else {
              subscriptionId = null;
              $scope.viewLoading = false;
            }
          });
        }
      }
    });

    DataSrv.register("service_history", function(message) {
      if (message.params.name === srvName){
        for (var j = 0; j < message.params.history.length; j++){
                
          var data = []
          for (var i = 0; i < message.params.history[j].history.length; i++){
            if (message.params.history[j].history[i].state === 1){
              var val = message.params.history[j].history[i].value;
              var valueDate = new Date(message.params.history[j].history[i].time * 1000);
              data.push({
                date: valueDate,
                value: val
              });
            }
          }

          $scope.chartOptions[j].dataProvider = data;
          $scope.chartChange[j]++;
        }
      }
    });

    DataSrv.register("service_change", function(message) {
      if (message.params.name === srvName){
        $scope.service = message.params;

        for (var j = 0; j < message.params.displays.length; j++){
          for (var i = 0; i < $scope.chartOptions.length; i++){
            if ($scope.chartOptions[i].name === message.params.displays[j].name){
              var newDate = new Date();
              $scope.chartOptions[i].dataProvider.push({
                date: newDate,
                value:  message.params.displays[j].value
              })
              $scope.chartChange[i]++;
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
