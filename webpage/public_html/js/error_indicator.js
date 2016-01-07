'use strict';

angular
    .module('app.error', [])

    .directive('errorIndicator', [
        '$rootScope',
        function($rootScope) {
          return {
            restrict : 'E',
            templateUrl : "error.html",
            link : function(scope, elem, attrs) {
              scope.error = false;
              scope.errorSlogan = "";
            }
          };
        }
    ]);