(function (app, ng) {
  'use strict';

  app.directive('highlighter', ['$timeout', function($timeout) {
    return {
      restrict: 'A',
      scope: {
        model: '=highlighter'
      },
      link: function(scope, element) {
        var timer = null;
        scope.$watch('model', function (nv, ov) {
          if (nv !== ov) {
            if (timer === null){
              $timeout.cancel(timer);
            }
            // apply class
            element.addClass('highlight');

            // auto remove after some delay
            timer = $timeout(function () {
              element.removeClass('highlight');
            }, 500);
          }
        });
      }
    };
  }]);
})(angular.module('app.highlighter', []), angular);

