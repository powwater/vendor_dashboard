$(document).on('shiny:sessioninitialized', function (e) {
  var mobile = window.matchMedia("only screen and (max-width: 768px)").matches;
  Shiny.onInputChange('is_mobile_device', mobile);
});
