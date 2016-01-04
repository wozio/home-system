'use strict';

/* App Module */

angular.module('app', [
  'ngRoute',
  'ngCookies',
  'app.login',
  'app.services',
  'app.loading'
])

.config(['$routeProvider',
  function($routeProvider) {
    $routeProvider.
      when('/', {
        templateUrl: 'services.html',
        controller: 'ServicesCtrl'
      }).
      when('/login', {
        templateUrl: 'login.html',
        controller: 'LoginCtrl'
      }).
      otherwise({
        redirectTo: '/login'
      });
  }])
.run(['$rootScope', '$location', '$cookies',
      function ($rootScope, $location, $cookies) {
          // keep user logged in after page refresh
          $rootScope.globals = $cookies.getObject('globals') || {};
          if ($rootScope.globals.currentUser) {
              console.log("logged in: " + $rootScope.globals.currentUser.email);
          }
    
          $rootScope.$on('$locationChangeStart', function (event, next, current) {
              // redirect to login page if not logged in
              if ($location.path() !== '/login' && !$rootScope.globals.currentUser) {
                  $location.path('/login');
              }
          });
      }]);