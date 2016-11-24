(function (app, ng) {
  'use strict';

  app.directive('chart', [function() {
    return {
      restrict: 'E',
      scope: {
        options: "=",
        change: '='
      },
      replace: true,
      template: '<div style="width:100%;height:500px;"></div>',
      link: function(scope, element) {
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
          scope.chart = AmCharts.makeChart(id, scope.options);

        scope.$watch('change', function (nv, ov) {
          if (nv !== ov) {
            scope.chart.dataProvider = scope.options.dataProvider;
            scope.chart.validateData();
          }
        });
      }
    };
  }]);
})(angular.module('app.chart', []), angular);

