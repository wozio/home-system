'use strict';

angular.module('app.main', [
])

.controller('MainCtrl', [
  '$scope', '$rootScope', 'DataSrv',
  function ($scope, $rootScope, DataSrv) {
    $scope.viewLoading = true;
	  console.log("MainCtrl");

    $rootScope.$on('$routeChangeStart', function () {
		  console.log("routeChangeStart");
      $scope.viewLoading = true;
    });

    $scope.viewLoaded = function(){
      $scope.viewLoading = false;
    }
  }
]);
