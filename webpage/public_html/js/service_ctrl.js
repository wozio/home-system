'use strict';

angular.module('app.service',[
  'app.data'
])

.controller('ServiceCtrl',[
  '$scope', '$routeParams', 'DataSrv',
  function ($scope, $routeParams, DataSrv) {
    var subscriptionId = null;
    var chart = null;
    var chartData = null;
    var ignoreZoomed = false;
    var srv = "io-control.service." + $routeParams.serviceId;
    var srvName = $routeParams.serviceId;
    console.log("ServiceCtrl (" + srvName + ")");
    $scope.service = {
        name: $routeParams.serviceId
    };

    DataSrv.register("service_change", function(message) {
      if (message.params.name === srvName){
        $scope.service = message.params;
        if (chart !== null) {
          var data = new Date();
          chartData.push({
            date: data,
            value:  message.params.displays[0].value.toFixed(1)
          });
          ignoreZoomed = true;
          chart.validateData();
        }
      }
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
              $scope.service = result.data.service;

              //console.log(result.data.history[0].history);
              chartData = new Array();
              for (var i = 0; i < result.data.history[0].history.length; i++){
                var data = new Date(result.data.history[0].history[i].time * 1000);
                chartData.push({
                  date: data,
                  value: result.data.history[0].history[i].value.toFixed(1)
                });
              }

              chart = AmCharts.makeChart( "chartdiv", {
                "type": "serial",
                "theme": "light",
                "dataProvider": chartData,
                "marginRight": 80,
                "autoMarginOffset": 20,
                "marginTop": 7,
                "valueAxes": [{
                    "axisAlpha": 0.2,
                    "dashLength": 1,
                    "position": "left"
                }],
                "mouseWheelZoomEnabled": true,
                "graphs": [{
                    "id": "g1",
                    "balloonText": "<div style='margin:5px; font-size:19px;'>[[value]]&#176;C</div>",
                    "title": "red line",
                    "valueField": "value",
                }],
                "chartScrollbar": {
                    "autoGridCount": true,
                    "graph": "g1",
                    "scrollbarHeight": 40
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
                },
                "export": {
                    "enabled": true
                }
              });
              chart.addListener("zoomed", function(event) {
                if (ignoreZoomed) {
                  ignoreZoomed = false;
                  return;
                }
                chart.zoomStartDate = event.startDate;
                chart.zoomEndDate = event.endDate;
              });

              chart.addListener("dataUpdated", function(event) {
                chart.zoomToDates(chart.zoomStartDate, chart.zoomEndDate);
              });

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
        DataSrv.send(srv, "unsubscribe", {"id":subscriptionId});
      }
    });
  }
]);
