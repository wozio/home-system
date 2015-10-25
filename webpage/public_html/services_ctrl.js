var servicesApp = angular.module('servicesApp', []);

servicesApp.controller('ServicesCtrl', function ($scope, $http) {
  $http.get('/HSA/execute?name=io-control-dev&msg=get_services').success(function(data) {
    $scope.services = data.services;
    
    $scope.updateSetting = function(service, setting, value) {
      $http.post('/HSA/execute?name=io-control-dev&msg=set_setting_value', {
          service: service.name,
          setting: setting.name,
          value: value,
          test_array: [
            "asdf",
            1234,
            1.4,
            true
          ],
          testObj: {
            fafa: "fafunia"
          }
        }).success(function(data) {
      });
    };
  });
});
