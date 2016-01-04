'use strict';
  
angular.module('app.auth',[])
  
.factory('AuthServ',
    ['$cookies', '$rootScope', '$timeout',
    function ($cookies, $rootScope, $timeout) {
        var service = {};
 
        service.Login = function (email, password, callback) {
 
            /* Dummy authentication for testing, uses $timeout to simulate api call
             ----------------------------------------------*/
            $timeout(function(){
                var response = { success: email === 'test@test' && password === 'test' };
                if(!response.success) {
                    response.message = 'Email or password is incorrect';
                }
                callback(response);
            }, 1000);
 
 
            /* Use this for real authentication
             ----------------------------------------------*/
            //$http.post('/api/authenticate', { username: username, password: password })
            //    .success(function (response) {
            //        callback(response);
            //    });
 
        };
  
        service.SetCredentials = function (email, password) {
  
            $rootScope.globals = {
                currentUser: {
                	email: email,
                    password: password
                }
            };
  
            $cookies.putObject('globals', $rootScope.globals);
        };
  
        service.ClearCredentials = function () {
            $rootScope.globals = {};
            $cookies.remove('globals');
        };
  
        return service;
    }]
);