function customers_table_module_js(ns_prefix) {
  $("#" + ns_prefix + "customers_table").on("click", ".info_btn", function () {
    Shiny.setInputValue(ns_prefix + "customer_id_to_map", this.id, {
      priority: "event",
    });
    $(this).tooltip("hide");
  });
}
