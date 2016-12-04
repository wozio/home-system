'use strict';

/* App Module */

angular.module('app', [
  'ngRoute',
  'app.main',
  'app.login',
  'app.services',
  'app.service',
  'app.data',
  'app.epg',
  'app.chart'
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
      when('/service/:serviceId', {
        templateUrl: 'service.html',
        controller: 'ServiceCtrl',
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

var checkUser = function ($q, $location, DataSrv) {
  var deferred = $q.defer();
  DataSrv.check(function (result) {
    if (result.success) {
      deferred.resolve(true);
    } else {
      deferred.reject();
      $location.path("/login");
    }
  });
  return deferred.promise;
};

var checkUserForLogin = function ($q, $location, DataSrv) {
  var deferred = $q.defer();
  DataSrv.check(function (result) {
    if (result.success) {
      deferred.reject();
      $location.path("/");
    } else {
      deferred.resolve(true);
    }
  });
  return deferred.promise;
};
