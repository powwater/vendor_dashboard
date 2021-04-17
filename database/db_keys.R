app_config <- config::get()

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = app_config$db$host,
  dbname = app_config$db$dbname,
  user = app_config$db$user,
  password = app_config$db$password
)

data_dict <- sqlpetr::sp_get_dbms_data_dictionary("orders", conn)

catalog <- sp_pg_catalog(connection = con)

tables <- tbl(con, dbplyr::in_schema("information_schema", "tables"))
table_constraints <- tbl(con, dbplyr::in_schema("information_schema", "table_constraints"))
key_column_usage <- tbl(con, dbplyr::in_schema("information_schema", "key_column_usage"))
referential_constraints <- tbl(con, dbplyr::in_schema("information_schema", "referential_constraints"))
constraint_column_usage <- tbl(con, dbplyr::in_schema("information_schema", "constraint_column_usage"))

keys <- tables %>%
  left_join(table_constraints, by = c(
    "table_catalog" = "table_catalog",
    "table_schema" = "table_schema",
    "table_name" = "table_name"
  )) %>%
  # table_constraints %>%
  filter(constraint_type %in% c("FOREIGN KEY", "PRIMARY KEY")) %>%
  left_join(key_column_usage,
            by = c(
              "table_catalog" = "table_catalog",
              "constraint_catalog" = "constraint_catalog",
              "constraint_schema" = "constraint_schema",
              "table_name" = "table_name",
              "table_schema" = "table_schema",
              "constraint_name" = "constraint_name"
            )
  ) %>%
  # left_join(constraint_column_usage) %>% # does this table add anything useful?
  select(table_name, table_type, constraint_name, constraint_type, column_name, ordinal_position) %>%
  arrange(table_name) %>%
  collect()

glimpse(keys)
