'use strict';

angular.module('app.main', [
])

.controller('MainCtrl', [
  '$scope', '$rootScope', 'DataSrv', '$interval',
  function ($scope, $rootScope, DataSrv, $interval) {
    $scope.viewLoading = true;
	console.log("MainCtrl");

    $rootScope.$on('$routeChangeStart', function () {
		console.log("routeChangeStart");
      $scope.viewLoading = true;
    });
	$rootScope.$on('$routeChangeEnd', function () {
		console.log("routeChangeEnd");
      $scope.viewLoading = false;
    });
  }
]);
