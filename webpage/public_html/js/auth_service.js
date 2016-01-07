'use strict';
  
angular.module('app.auth',[
  'ngCookies',
  'app.data'
])
  
.factory('AuthSrv', [
  '$cookies', '$timeout', '$rootScope', 'DataSrv',
  function ($cookies, $timeout, $rootScope, DataSrv) {
    var service = {};
        
    // check if user is logged in and try to login from cookies if it is not
    service.check = function(callback) {
      if ($rootScope.user === undefined) {
        var user = $cookies.getObject('user') || {};
        if (user.email) {
          service.login(user.email, user.password, function(result){
            callback(result);
            return;
          });
        }
      }
      callback({ success: false });
    }; 

    // login the user with credentials provided in arguments
    service.login = function(email, password, callback) {
      console.log("logging in: " + email);
      $timeout(function() {
        var result = { success: email === 'test@test' && password === 'test' };
        if (!result.success) {
          result.message = 'Email or password is incorrect';
          delete $rootScope.user;
        } else {
          $rootScope.user = {
            email: email,
            password: password
          }
          $cookies.putObject('user', $rootScope.user);
        }
        callback(result);
      }, 1000);
    };
  
    service.logout = function () {
      delete $rootScope.user;
      $cookies.remove('user');
    };

    return service;
  }]
);