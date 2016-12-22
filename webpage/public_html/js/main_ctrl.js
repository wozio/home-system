'use strict';

angular.module('app.main', [
])

.controller('MainCtrl', [
  '$scope', '$rootScope', 'DataSrv',
  function ($scope, $rootScope, DataSrv) {

    $scope.dataProgress = 0;
    $scope.connected = true; // initial value true to avoid showing dialog box on initial loading
    $scope.loggedIn = true;  // -"-
    $scope.loggingIn = false;
    $scope.user = null;
    $scope.loading = true;

    // utility function for registering for event
    var registerEvent = function(event, callback) {
      var listener = $rootScope.$on(event, callback);
      $scope.$on('$destroy', listener);
    }

    // registering events
    registerEvent('$routeChangeStart', function () {
      $scope.loading = true;
      $scope.dataProgress = 0;
    });

    registerEvent('data:disconnected', function () {
      $scope.connected = false;
    });

    registerEvent('data:connected', function () {
      $scope.connected = true;
    });

    registerEvent('user:loggedIn', function (event, user) {
      $scope.loggingIn = false;
      $scope.loggedIn = true;
      $scope.user = user;
      console.log("loggedIn: " + user.name);
    });

    registerEvent('user:loggedOut', function () {
      $scope.loggingIn = false;
      $scope.loggedIn = false;
      $scope.user = null;
      $scope.loading = false;
      console.log("loggedOut");
    });

    registerEvent('user:loggingIn', function () {
      $scope.loggingIn = true;
      console.log("loggingIn");
    });

    $scope.viewLoaded = function(){
      $scope.loading = false;
      $scope.dataProgress = 100;
    };

    $scope.viewDataProgress = function(progress){
      $scope.dataProgress = progress;
    };

    $scope.requestLogin = function(loginData){
      DataSrv.login(loginData.email, loginData.password);
    }
  }
]);
