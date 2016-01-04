'use strict';
  
angular.module('app.login',[
                          'app.auth'
])
  
.controller('LoginCtrl',
  ['$scope', '$rootScope', '$location', 'AuthServ',
   function ($scope, $rootScope, $location, AuthServ) {
        // reset login status
	  AuthServ.ClearCredentials();
	  
        $scope.login = function (email, password) {
        	console.log(email + ": " + password);
            $scope.dataLoading = true;
            
            AuthServ.Login($scope.email, $scope.password, function(response) {
            	if(response.success) {
            		AuthServ.SetCredentials($scope.email, $scope.password);
                    $location.path('/');
                } else {
                    $scope.error = response.message;
                    $scope.dataLoading = false;
                }
            });
        };
    }]
);