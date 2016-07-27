'use strict';
  
angular.module('app.auth',[
  'ngCookies',
  'app.data'
])
  
.factory('AuthSrv', [
  '$cookies', '$rootScope', 'DataSrv',
  function ($cookies, $rootScope, DataSrv) {
    var service = {};
    
    // check if user is logged in and try to login from cookies if it is not
    service.check = function(callback) {
      console.log("Check user");
      if ($rootScope.user === undefined) {
        var user = $cookies.getObject('user') || {};
        if (user.email) {
          service.login(user.email, user.password, function(result){
            console.log("login callback in check");
            callback(result);
          });
        } else {
          callback({ success: false });
        }
      } else {
        callback({ success: true });
      }
    };

    // login the user with credentials provided in arguments
    service.login = function(email, password, callback) {
      console.log("logging in: " + email);
      DataSrv.send("control-server", "login", {
        email: email,
        password: password
      }, function(result) {
        if (!result.success) {
          result.reason = 'Unable to login: ' + result.reason;
          delete $rootScope.user;
          DataSrv.set_client_id("");
        } else {
          console.log("successfully logged in with client id: " + result.data.client_id);
          $rootScope.user = {
            email: email,
            password: password,
            user: result.data.user,
            name: result.data.name
          }
          $cookies.putObject('user', $rootScope.user);
          DataSrv.set_client_id(result.data.client_id);
        }
        callback(result);
      }, 1000);
    };
  
    service.logout = function () {
      delete $rootScope.user;
      $cookies.remove('user');
      DataSrv.set_client_id("");
    };
    
    return service;
  }]
);