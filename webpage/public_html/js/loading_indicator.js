'use strict';

angular
    .module('app.loading', [])

    .directive('loadingIndicator', [
        '$rootScope',
        function($rootScope) {
          return {
            restrict : 'E',
            template : "<div ng-if='loading' class='alert alert-info'>Loading...</div>",
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