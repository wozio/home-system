'use strict';

angular.module('app.main', [
])

.controller('MainCtrl', [
  '$scope', '$rootScope', 'DataSrv', '$interval',
  function ($scope, $rootScope, DataSrv, $interval) {
    $scope.viewLoading = true;

    $rootScope.$on('$routeChangeStart', function () {
      $scope.viewLoading = true;
    });
  }
]);
