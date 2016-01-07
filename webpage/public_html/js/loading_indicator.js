'use strict';

angular
    .module('app.loading', [])

    .directive('loadingIndicator', [
        '$rootScope',
        function($rootScope) {
          return {
            restrict : 'E',
            templateUrl: 'loading.html',
            link : function(scope, elem, attrs) {
              scope.loading = false;

              $rootScope.$on('$routeChangeStart', function() {
                scope.error = false;
                scope.errorSlogan = "";
                scope.loading = true;
              });
            }
          };
        }
    ]);