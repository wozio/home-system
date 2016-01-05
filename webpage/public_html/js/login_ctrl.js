'use strict';
  
angular.module('app.login',[
  'app.auth'
])
  
.controller('LoginCtrl',
  ['$scope', '$rootScope', '$location', 'AuthSrv',
  function ($scope, $rootScope, $location, AuthSrv) {
    $rootScope.loading = false;
    $scope.dataLoading = false;
    
    $scope.login = function (email, password) {
      console.log(email + ": " + password);
      $rootScope.error = false;
      $rootScope.loading = true;
      $scope.dataLoading = true;
      
      AuthSrv.login($scope.email, $scope.password, function(result) {
        $scope.dataLoading = false;
      	if(result.success) {
      	  $location.path('/');
        } else {
          $rootScope.loading = false;
          $rootScope.error = true;
          $rootScope.errorSlogan = result.reason;
        }
      });
    };
  }]
);