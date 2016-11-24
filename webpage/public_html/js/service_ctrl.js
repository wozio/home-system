'use strict';

angular.module('app.service',[
  'app.data',
  'amChartsDirective'
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
    $scope.chartData = [];

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

              for (var j = 0; j < result.data.history.length; j++){
                
                var data = []
                var max = null;
                var min = null;
                
                for (var i = 0; i < result.data.history[j].history.length; i++){
                  if (result.data.history[j].history[i].state === 1){
                    var val = result.data.history[j].history[i].value;
                    if (max === null){
                      max = val;
                      min = val;
                    } else {
                      if (min > val)
                        min = val;
                      if (max < val)
                        max = val;
                    }
                    var valueDate = new Date(result.data.history[j].history[i].time * 1000);
                    data.push({
                      date: valueDate,
                      value: val
                    });
                  }
                }

                var newChartData = {
                  "name": result.data.history[j].name,
                  "type": "serial",
                  "theme": "light",
                  "marginRight": 40,
                  "autoMarginOffset": 20,
                  "marginTop": 7,
                  "data": data,
                  "creditsPosition": "top-left",
                  "titles": [{
                      "text": result.data.history[j].name
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
                  "chartScrollbar": {
                      "autoGridCount": true,
                      "graph": "g1",
                      "scrollbarHeight": 30
                  },
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
                console.log("max=" + max + " min=" + min);
                var diff = max - min;
                if (diff < 3){
                  newChartData.valueAxes[0].maximum = Math.ceil(max + (3 - diff)/2);
                  newChartData.valueAxes[0].minimum = Math.floor(min - (3 - diff)/2);
                  console.log("max=" + newChartData.valueAxes[0].maximum + " min=" + newChartData.valueAxes[0].minimum);
                }
                

                $scope.chartData.push(newChartData);
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

    DataSrv.register("service_change", function(message) {
      if (message.params.name === srvName){
        $scope.service = message.params;

        for (var j = 0; j < message.params.displays.length; j++){
          for (var i = 0; i < $scope.chartData.length; i++){
            if ($scope.chartData[i].name === message.params.displays[j].name){
              var newDate = new Date();
              $scope.chartData[i].data.push({
                date: newDate,
                value:  message.params.displays[j].value
              })
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
