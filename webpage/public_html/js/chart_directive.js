(function (app, ng) {
  'use strict';

  app.directive('chart', [function() {
    return {
      restrict: 'E',
      scope: {
        options: "=",
        change: '=',
        data: "="
      },
      replace: true,
      template: '<div style="width:100%;height:500px;margin-bottom:30px;display:none;"></div>',
      link: function(scope, element) {
        // first generate id for chart
        var guid = function guid() {
          function s4() {
            return Math.floor((1 + Math.random()) * 0x10000)
                .toString(16)
                .substring(1);
          }

          return s4() + s4() + '-' + s4() + '-' + s4() + '-' +
              s4() + '-' + s4() + s4() + s4();
        };
        var id = guid();
        element.attr('id', id);

        // now fill options
        var newChartOptions = {
          "dataProvider": scope.data,
          "type": "serial",
          "theme": "light",
//          "processTimeout": 5,
          "marginRight": 40,
          "autoMarginOffset": 20,
          "marginTop": 7,
          "creditsPosition": "top-left",
          "valueAxes": [{
              "axisAlpha": 0.2,
              "dashLength": 1,
              "position": "left"
          }],
          "graphs": [{
              "id": "g1",
              "balloonText": "<div style='margin:3px; font-size:11px;'>[[value]]&#176;C</div>",
              "valueField": "value",
              "lineThickness": 2
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
          },
          "chartScrollbar": {
            "graph":"g1",
            "gridAlpha":0,
            "color":"#888888",
            "scrollbarHeight":30,
            "backgroundAlpha":0,
            "selectedBackgroundAlpha":0.1,
            "selectedBackgroundColor":"#888888",
            "graphFillAlpha":0,
            "autoGridCount":true,
            "selectedGraphFillAlpha":0,
            "graphLineAlpha":0.2,
            "graphLineColor":"#c2c2c2",
            "selectedGraphLineColor":"#888888",
            "selectedGraphLineAlpha":1

          }
        };
        if (scope.options.type === "state"){
          newChartOptions.graphs[0].type = "step";
          newChartOptions.graphs[0].balloonText = "<div style='margin:3px; font-size:11px;'>[[value]]</div>";
          newChartOptions.valueAxes[0].integersOnly = true;
        }

        var ignoreZoomed = false;
        var zoomStartDate = null;
        var zoomEndDate = null;

        // create chart
        var chart = AmCharts.makeChart(id, newChartOptions);

        // add listeners
        chart.addListener("zoomed", function(event) {
          if (ignoreZoomed) {
            ignoreZoomed = false;
            return;
          }
          zoomStartDate = event.startDate;
          zoomEndDate = event.endDate;
        });

        chart.addListener("dataUpdated", function(event) {
          if (zoomStartDate !== null){
            chart.zoomToDates(zoomStartDate, zoomEndDate);
          }
        });

        // watch on change indicator which will be changed when
        // data is changed
        scope.$watch('change', function (nv, ov) {
          if (nv !== ov) {
            chart.dataProvider = scope.data;
            if (scope.data.length > 0){
              ignoreZoomed = true;
              element.css("display", "block");
              chart.validateData();
            } else {
              element.css("display", "none");
            }
          }
        });
      }
    };
  }]);
})(angular.module('app.chart', []), angular);

