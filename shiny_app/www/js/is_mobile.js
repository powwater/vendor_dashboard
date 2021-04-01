$(document).on('shiny:sessioninitialized', function (e) {
  var mobile = window.matchMedia("only screen and (max-width: 768px)").matches;
  Shiny.setInputValue('is_mobile_device', mobile);
});
