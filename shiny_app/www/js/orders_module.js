function orders_table_module_js(ns_prefix) {

  $("#" + ns_prefix + "orders_table").on("click", ".info_btn", function () {
    Shiny.setInputValue(ns_prefix + "order_id_to_info", this.id, {
      priority: "event",
    });
    $(this).tooltip("hide");
  });

  $("#" + ns_prefix + "awaiting_orders_table").on("click", ".accept_btn", function () {
    Shiny.setInputValue(ns_prefix + "order_id_to_accept", this.id, {
      priority: "event",
    });
    $(this).tooltip("hide");
  });

  $("#" + ns_prefix + "awaiting_orders_table").on("click", ".decline_btn", function () {
    Shiny.setInputValue(ns_prefix + "order_id_to_decline", this.id, {
      priority: "event",
    });
    $(this).tooltip("hide");
  });


  var src = 'ka-ching.mp3';
  var audio = new Audio(src);
  Shiny.addCustomMessageHandler(
    "ka_ching",
    function(message) {
      audio.play();
    }
  )

}
