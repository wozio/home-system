'use strict';
  
angular.module('app.login',[
  'app.data'
])
  
.controller('LoginCtrl',
  ['$scope', '$location', 'DataSrv',
  function ($scope, $location, DataSrv) {
    $scope.dataLoading = false;
    
    $scope.login = function (email, password) {
      console.log(email + ": " + password);
      $scope.dataLoading = true;
      
      DataSrv.login($scope.email, $scope.password, function(result) {
        $scope.dataLoading = false;
      	if(result.success) {
      	  $location.path('/');
        }
      });
    };
  }]
);