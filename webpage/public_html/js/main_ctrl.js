'use strict';

angular.module('app.main', [
])

.controller('MainCtrl', [
  '$scope', '$rootScope', 'DataSrv',
  function ($scope, $rootScope, DataSrv) {
    $scope.dataProgress = 0;
	  console.log("MainCtrl");

    $rootScope.$on('$routeChangeStart', function () {
		  console.log("routeChangeStart");
      $scope.dataProgress = 0;
    });

    $scope.viewLoaded = function(){
      $scope.dataProgress = 100;
    }

    $scope.viewDataProgress = function(progress){
      $scope.dataProgress = progress;
    }
  }
]);
