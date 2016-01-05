'use strict';

angular
    .module('app.error', [])

    .directive('errorIndicator', [
        '$rootScope',
        function($rootScope) {
          return {
            restrict : 'E',
            template : "<div ng-if='error' class='alert alert-danger'>{{errorSlogan}}</div>",
            link : function(scope, elem, attrs) {
              scope.error = false;
              scope.errorSlogan = "";
            }
          };
        }
    ]);