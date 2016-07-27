'use strict';

/* App Module */

angular.module('app', [
  'ngRoute',
  'app.login',
  'app.services',
  'app.auth',
  'app.epg'
])

.config(['$routeProvider',
  function($routeProvider) {
    $routeProvider.
      when('/', {
        templateUrl: 'services.html',
        controller: 'ServicesCtrl',
        resolve: {
          factory: checkUser
        }
      }).
      when('/epg', {
        templateUrl: 'epg.html',
        controller: 'EpgCtrl',
        resolve: {
          factory: checkUser
        }
      }).
      when('/login', {
        templateUrl: 'login.html',
        controller: 'LoginCtrl',
        resolve: {
          factory: checkUserForLogin
        }
      }).
      otherwise({
        redirectTo: '/login'
      });
  }
]);

var checkUser = function ($q, $rootScope, $location, AuthSrv) {
  var deferred = $q.defer();
  AuthSrv.check(function (result) {
    if (result.success) {
      console.log("result success");
      deferred.resolve(true);
    } else {
      console.log("result not success");
      deferred.reject();
      $location.path("/login");
    }
  });
  return deferred.promise;
};

var checkUserForLogin = function ($q, $rootScope, $location, AuthSrv) {
  var deferred = $q.defer();
  AuthSrv.check(function (result) {
    if (result.success) {
      console.log("result success, redirecting to root page");
      deferred.resolve(true);
      $location.path("/");
    } else {
      console.log("result not success, staying on login page");
      deferred.resolve(true);
    }
  });
  return deferred.promise;
};