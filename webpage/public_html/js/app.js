'use strict';

/* App Module */

angular.module('app', [
  'ngRoute',
  'app.main',
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
        controller: 'ServicesCtrl'
      }).
      when('/services', {
        templateUrl: 'services.html',
        controller: 'ServicesCtrl'
      }).
      when('/service/:serviceId', {
        templateUrl: 'service.html',
        controller: 'ServiceCtrl'
      }).
      when('/media', {
        templateUrl: 'epg.html',
        controller: 'EpgCtrl'
      }).
      otherwise({
        redirectTo: '/'
      });
  }
]);
