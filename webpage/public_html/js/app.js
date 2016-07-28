'use strict';

/* App Module */

angular.module('app', [
  'ngRoute',
  'app.login',
  'app.services',
  'app.data',
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

var checkUser = function ($q, DataSrv) {
  var deferred = $q.defer();
  DataSrv.check(function (result) {
    if (result.success) {
      console.log("result success");
      deferred.resolve(true);
    } else {
      console.log("result not success");
      deferred.reject();
      // DataSrv redirects to login view if logging in is not successful
    }
  });
  return deferred.promise;
};

var checkUserForLogin = function ($q, $location, DataSrv) {
  var deferred = $q.defer();
  DataSrv.check(function (result) {
    if (result.success) {
      console.log("result success, redirecting to root view");
      deferred.reject();
      $location.path("/");
    } else {
      console.log("result not success, staying on login view");
      deferred.resolve(true);
    }
  });
  return deferred.promise;
};