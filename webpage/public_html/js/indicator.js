'use strict';

angular
    .module('app.indicator', [])

    .directive('indicator', [
        '$rootScope',
        function($rootScope) {
          return {
            restrict : 'E',
            templateUrl: 'indicator.html',
            link : function(scope, elem, attrs) {
              scope.loading = false;
              scope.error = false;
              scope.errorSlogan = "";

              $rootScope.$on('$routeChangeStart', function() {
                scope.error = false;
                scope.errorSlogan = "";
                scope.loading = true;
              });
            }
          };
        }
    ]);