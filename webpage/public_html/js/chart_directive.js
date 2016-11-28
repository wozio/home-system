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
      template: '<div style="width:100%;height:500px;"></div>',
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
          }
        };
        if (scope.options.type === "state"){
          newChartOptions.graphs[0].type = "step";
          newChartOptions.graphs[0].balloonText = "<div style='margin:3px; font-size:11px;'>[[value]]</div>";
          newChartOptions.valueAxes[0].integersOnly = true;
        }

        // create chart
        var chart = AmCharts.makeChart(id, newChartOptions);

        // watch on change indicator which will be changed when
        // data is changed
        scope.$watch('change', function (nv, ov) {
          if (nv !== ov) {
            chart.dataProvider = scope.data;
            chart.validateData();
          }
        });
      }
    };
  }]);
})(angular.module('app.chart', []), angular);

