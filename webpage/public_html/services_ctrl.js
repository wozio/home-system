var phonecatApp = angular.module('phonecatApp', []);

phonecatApp.controller('PhoneListCtrl', function ($scope, $http) {
  $http.get('/HSA/execute?name=io-control-dev&msg=get_services').success(function(data) {
    console.log(data);
    $scope.services = data.services;
  });
});