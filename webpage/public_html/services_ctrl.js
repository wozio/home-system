var servicesApp = angular.module('servicesApp', []);

servicesApp.controller('ServicesCtrl', function ($scope, $http) {
  $http.get('/HSA/execute?name=io-control-dev&msg=get_services').success(function(data) {
    $scope.services = data.services;
  });
});
