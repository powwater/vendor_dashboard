
#' Function to read an entire database table.
#'
#' \code{db_read_table} is a wrapper for \code{DBI::dbReadTable}.
#'
#' @param con Database connection.
#'
#' @param table Table to read.
#'
#' @return Tibble.
#'
#' @export
db_read_table <- function(con, table)
  dplyr::as_tibble(DBI::dbReadTable(con, table))


#' Function to list all variables/columns/fields in a database table.
#'
#' \code{db_list_variables} is a wrapper for \code{DBI::dbListFields}.
#'
#' @param con Database connection.
#'
#' @param table Table to list variables/columns/fields.
#'
#' @return Character vector.
#'
#' @export
db_list_variables <- function(con, table) DBI::dbListFields(con, table)


#' Function to list results for a database connection.
#'
#' @param con Database connection.
#'
#' @export
db_list_results <- function(con) DBI::dbListResults(con)[[1]]


#' Function to clear results for a database connection.
#'
#' @param con Database connection.
#'
#' @export
db_clear_results <- function(con) DBI::dbClearResult(db_list_results(con))


#' Function to commit transactions for a database connection.
#'
#' @param con Database connection.
#'
#' @export
db_commit <- function(con) DBI::dbCommit(con)


#' Function to list database tables.
#'
#' @param con Database connection.
#'
#' @return Character vector.
#'
#' @export
db_list_tables <- function(con) DBI::dbListTables(con)


#' Function to test if database contains a table.
#'
#' @param con Database connection.
#' @param table A table name.
#'
#' @return Logical vector.
#'
#' @export
db_table_exists <- function(con, table) DBI::dbExistsTable(con, table)

db_name <- function(con, extension = TRUE) {
  x <- db_get(con, "SELECT CURRENT_DATABASE()")[, 1]
  if (!extension) x <- stringr::str_split_fixed(x, "\\.", 2)[, 1]
  return(x)
}

#' Function to get the size of a database.
#'
#' For SQLite databases, the file size is returned but for PostgreSQL databases,
#' the `pg_database` table is queried.
#'
#' @author Stuart K. Grange
#'
#' @param con Database connection.
#'
#' @param unit Measurement unit. Default is \code{"mb"} for megabytes.
#'
#' @return Numeric vector.
#'
#' @export
db_size <- function(con, unit = "mb") {

  # Parse
  unit <- stringr::str_to_lower(unit)

  # Build query, requires name
  sql_select <- stringr::str_c("SELECT pg_database_size('", db_name(con), "')")

  # Query
  x <- db_get(con, sql_select)[, , drop = TRUE]

  # Convert unit
  if (unit == "mb") x <- x / 1e+06
  if (unit == "gb") x <- x / 1e+09

  # Return
  return(x)

}

#' Function to get the names of database table and produce a data frame with
#' zero rows.
#'
#' \code{db_table_names} is useful when preparing a data frame for a SQL insert.
#'
#' @param con Database connection.
#'
#' @param table Database table.
#'
#' @param as_tibble Should the return be a tibble?
#'
#' @author Stuart K. Grange
#'
#' @return Data frame with zero rows.
#'
#' @export
db_table_names <- function(con, table, as_tibble = FALSE) {

  # Get database table names with one observation
  df <- db_get(
    con,
    stringr::str_c("SELECT * FROM ", table, " LIMIT 1"),
    warn = FALSE
  )

  # Postgres returns 0 0 data frame
  if (nrow(df) == 0 & ncol(df) == 0) {

    # Get names
    names <- db_list_variables(con, table)

    # Create a data frame
    df <- read.csv(text = "", col.names = names)

  }

  # Remove data if present
  df <- df[-1, ]

  # To tibble
  if (as_tibble) df <- as_tibble()

  return(df)

}

#' Function to run a \code{TRUNCATE TABLE} statement for a database table.
#'
#' Beware that this function will remove the contents of a database table, and
#' possibly other tables so use with caution.
#'
#' @author Stuart K. Grange
#'
#' @param con Database connection.
#'
#' @param table Table to remove all data from.
#'
#' @param cascade Should the \code{TRUNCATE} statement be used with
#' \code{CASCADE}? Using \code{cascade} will delete relations which are
#' dependent on \code{table}.
#'
#' @return Invisible.
#'
#' @export
db_truncate_table <- function(con, table, cascade = FALSE) {
  sql <- stringr::str_c("TRUNCATE TABLE ", table)
  if (cascade) sql <- stringr::str_c(sql, " CASCADE")
  db_execute(con, sql)
}

#' Function to drop a table within a database.
#'
#' @param con Database connection.
#'
#' @param table Table to drop.
#'
#' @param cascade Should the \code{DROP TABLE} statement also use a
#' \code{CASCADE} clause to force the table drop?
#'
#' @return Invisible, \code{con}.
#'
#' @export
db_drop_table <- function(con, table, cascade = FALSE) {

  # Check if table exists
  if (!db_table_exists(con, table)) {
    stop("`table` does not exist in database...", call. = FALSE)
  }

  # Build statement
  sql <- stringr::str_c("DROP TABLE ", table)

  # Add cascade to statement
  if (cascade) sql <- stringr::str_c(sql, " CASCADE")

  # Drop
  db_execute(con, sql)

  return(invisible(con))

}

#' Functions to vacuum, analyze, and optimize database tables.
#'
#' @param con Database connection.
#' @param table Database table.
#' @param verbose Should the function give messages?
#'
#' @return Invisible data frame.
db_vacuum <- function(con, table = NA, verbose = FALSE) {

  # Get things pre-vacuum
  date_pre <- Sys.time()
  size_pre <- db_size(con)

  if (verbose) message(str_date_formatted(date_pre), ": Vacuuming database...")

  if (db.class(con) == "postgres") {

    # Defalt to all tables
    if (is.na(table[1])) table <- db_list_tables(con)

    # Do
    db_execute(con, stringr::str_c("VACUUM (VERBOSE) ", table))

  } else if (db.class(con) == "mysql") {

    # Catch the reserved verbs
    table <- stringr::str_c("`", table, "`")

    # Optimise
    db_execute(con, stringr::str_c("OPTIMIZE TABLE ", table))

  } else if (db.class(con) == "sqlite") {

    db_execute(con, "VACUUM")

  }

  # Get things post-vacuum
  date_post <- Sys.time()
  size_post <- db_size(con)

  df <- tibble(
    when = c("pre_vacuum", "post_vacuum"),
    date = c(date_pre, date_post),
    size = c(size_pre, size_post)
  )

  return(invisible(df))

}


# Use MySQL notation
#' @rdname db_vacuum
#' @export
db_optimise <- db_vacuum

# INSERT ------------------------------------------------------------------

#' Function to build \code{INSERT INTO} SQL statements from a data frame.
#'
#' @param table Database table name to insert into.
#' @param df Data frame to use to create statements.
#' @param squish Should whitespace around commas and equal signs be removed to
#' make the SQL statement shorter?
#'
#' @return Character vector with a length of nrow(df).
#'
#' @export
#' @importFrom tidyr unite
build_insert_statements <- function(table, df, squish = FALSE) {

  # Check inputs
  stopifnot(length(table) == 1)

  # Return empty string if input is empty
  if (nrow(df) == 0) return(as.character())

  # No quoting used for names
  insert_into <- str_c(names(df), collapse = ", ")
  insert_into <- str_c("INSERT INTO ", table, " (", insert_into, ")")

  # Prepare data frame
  df <- prepare_data_frame_for_sql(df)

  # Collapse rows and create values piece of sql string
  sql <- tidyr::unite(df, "values", 1:ncol(df), sep = ", ") %>%
    mutate(values = str_c(" VALUES (", values, ")")) %>%
    pull() %>%
    str_c(insert_into, .)

  # Remove some whitespace to make statement smaller
  if (squish) sql <- str_replace_all(sql, ", ", ",")

  return(sql)

}


# UPDATE ------------------------------------------------------------------




#' Function to build SQL \code{WHERE} statements from an input data frame.
#'
#' @param table Database's table to update.
#'
#' @param df Input data frame to generate \code{UPDATE} statements with.
#'
#' @param where Which variables in \code{df} to be used for the \code{WHERE}
#' clause. If not used, only a single row data frame can be used.
#'
#' @param squish Should whitespace around commas and equal signs be removed to
#' make the SQL statement shorter?
#'
#'
#' @return Character vector with a length of \code{nrow(df)}.
#'
#' @export
build_update_statements <- function(table, df, where = NA, squish = FALSE) {

  # Check inputs
  stopifnot(length(table) == 1)

  # Return empty string if input is empty
  if (nrow(df) == 0) return(as.character())

  if (is.na(where[1]) && nrow(df) != 1) {
    stop("If `where` is not used, input must have a single row...", call. = FALSE)
  }

  # Build where clauses
  if (!is.na(where[1])) {

    # Build where clause
    sql_where <- build_sql_pairs(df[, where, drop = FALSE], sep = "and")
    sql_where <- str_c(" WHERE ", sql_where)

    # Drop where variable from data to be used for update statements
    df <- df[, setdiff(names(df), where), drop = FALSE]

  }

  # Create sql pairs containing the data
  sql_update <- build_sql_pairs(df, sep = ",")

  # Build update statement
  sql_update <- str_c("UPDATE ", table, " SET ", sql_update)

  # Add where clauses
  if (!is.na(where[1])) sql_update <- str_c(sql_update, sql_where)

  # Remove some whitespace to make statement smaller
  if (squish) {
    sql_update <- str_replace_all(sql_update, ", ", ",")
    sql_update <- str_replace_all(sql_update, " = ", "=")
  }

  return(sql_update)

}


# UTILS -------------------------------------------------------------------

build_sql_pairs <- function(df, sep) {

  # Parse
  stopifnot(length(sep) == 1)
  sep <- stringr::str_trim(sep)
  sep <- stringr::str_to_upper(sep)

  if (!sep %in% c(",", "AND")) {
    stop("`sep` must be one of 'AND' or ','...", call. = FALSE)
  }

  sep <- ifelse(sep == "AND", " AND ", sep)
  sep <- ifelse(sep == ",", ", ", sep)

  # Format for database table
  df <- prepare_data_frame_for_sql(df)

  # Get named vector for sql keys
  keys <- names(df)
  names(keys) <- keys

  # Use keys and values to make sql pairs
  df <- df %>%
    purrr::map2_dfr(keys, ., ~str_c(.x, " = ", .y)) %>%
    tidyr::unite(., "sql", 1:ncol(.), sep = sep) %>%
    pull()

  return(df)

}


prepare_data_frame_for_sql <- function(df) {

  # Catch single quotes
  df <- dplyr::mutate_if(df, is.character, ~str_replace_all(., "'", "''"))

  # Add single quotes if character
  df <- dplyr::mutate_if(df, is.character, ~str_c("'", ., "'"))

  # Switch nas to sql null
  df <- dplyr::mutate_all(df, ~ifelse(is.na(.), "NULL", .))

  return(df)

}

#' Function to find duplicates in a database table's variable.
#'
#' @param con Database connection.
#' @param table Database table.
#' @param variable Database table's variable to test.
#'
#' @return Data frame.
#'
#' @export
db_duplicates <- function(con, table, variable) {

  # Build query
  sql <- stringr::str_c(
    "SELECT ", variable,
    " FROM ", table,
    " GROUP BY ", variable,
    " HAVING (COUNT(*) > 1)"
  )

  sql <- threadr::str_trim_many_spaces(sql)

  # Query
  df <- db_get(con, sql)

  return(df)

}


#' Function to test if duplicates are contained in a database table's variable.
#'
#' @param con Database connection.
#' @param table Database table.
#' @param variable Database table's variable to test.
#'
#' @return Logical vector with length of 1.
#'
#' @export
db_has_duplicates <- function(con, table, variable) {

  # Get duplicates
  df <- db_duplicates(con, table, variable)

  # Test
  x <- ifelse(nrow(df) != 0, TRUE, FALSE)

  return(x)

}
#' Function to arrange variables in a data frame to match those found in a
#' database table.
#'
#' \code{db_arrange_variables} is useful for preparing data for insert and when
#' look-up tables are being created.
#'
#' @param con Database connection.
#' @param table Table name in database.
#' @param df Data frame.
#'
#' @export
db_arrange_variables <- function(con, table, df)
  plyr::rbind.fill(db_table_names(con, table), df)

# ANALYZE -----------------------------------------------------------------

#' Function to analyse/analyze a database table.
#'
#' @param con Database connection.
#' @param table Database table.
#'
#' @return Invisible.
#'
#' @export
db_analyse <- function(con, table) {

  # Postgres
  if (db.class(con) == "postgres")
    db_send(con, stringr::str_c("ANALYZE ", table))

  # No return

}


# CONNECT -----------------------------------------------------------------

get_config <- function() {

  if (!file.exists("config.yml")) {

    download <- usethis::ui_yeah("No configuration file detected. Download from GDrive?")

    if (download) {
      requireNamespace("googledrive")
      # auth <- googledrive::drive_has_token()
      googledrive::drive_auth()
      googledrive::drive_download(
        file = googledrive::as_id("https://drive.google.com/file/d/115lcGZ-OCtHgaUM_XQYl6IJBBestwV8_/view?usp=sharing"),
        path = "config.yml"
      )
    }
  } else {
    usethis::ui_done("config.yml already in working directory.")
  }

}

#' Function to create a database connection with a \code{JSON} configuration file.
#'
#' \code{db_connect} uses a \code{JSON} configuration file to create a database
#' connection. This configuration file will often exist outside a code package
#' so database credentials are not accidentally transmitted or shared.
#'
#' If only one entry is in the \code{JSON} file, the \code{database} argument is
#' not needed.
#'
#' If \code{db_connect} is attempted to be used with a file which is not
#' \code{JSON}, it will attempt to connect to an SQLite database, but it is
#' recommended that the \code{config} argument is set to \code{FALSE} in this
#' case.
#'
#' MySQL, PostgreSQL, and SQLite connections are currently supported.
#'
#' @param file \code{JSON} file or string containing database connection
#' details. For SQLite databases, use the database's file path.
#'
#' @param database The name of the database within \code{file} to use to create
#' a database connection to. If only one entry is in \code{file}, this argument
#' is not needed and will be ignored if used.
#'
#' @param config A logical to skip the \code{JSON} file configuration and just
#' attempt to connect to \code{file} directly. This is used for SQLite
#' databases which require no configuration.
#'
#' @param foreign_keys A logical for SQLite databases where foreign keys should
#' be enforced. Default is \code{TRUE}. For other database types, this will be
#' ignored.
#'
#' @return Database connection.
#'
#' @export
db_connect <- function(r_config_active = "default") {

  if (!file.exists("config.yml")) get_config()
  Sys.setenv("R_CONFIG_ACTIVE" = r_config_active)
  config <- config::get("db")


  # Connect
  conn <- dbx::dbxConnect(config$url)

  # conn <- DBI::dbConnect(
  #   drv,
  #   user = app_config$db$user,
  #   dbname = app_config$db$dbname,
  #   password = app_config$db$password,
  #   host = app_config$db$host
  # )

  # Also give application name
  db_execute(conn, stringr::str_c(
    "SET application_name = '",
    postgres_application_name(), "'")
  )

  conn

}


#' Function to close a database connection cleanly.
#'
#' @param con An active database connection.
#'
#' @return Invisible.
#'
#' @export
db_disconnect <- function(con) quiet(DBI::dbDisconnect(con))


postgres_application_name <- function() {

  # Get infomation
  list_version <- R.Version()

  # Clean
  version <- list_version$version.string
  version <- stringr::str_remove_all(version, "\\s*\\([^\\)]+\\)")
  version <- stringr::str_remove(version, "version ")

  # Add package name too
  version <- stringr::str_c(version, " with RPostgreSQL")

  return(version)

}


# Execute -----------------------------------------------------------------

#' Function to execute statements on a database.
#'
#' \code{db_execute} is a wrapper for \code{DBI::dbExecute} but is vectorised
#' over \code{statement}.
#'
#' @param con Database connection.
#' @param statement Statement to send to \code{con}.
#' @param ... Other parameters passed on to methods.
#' @param progress Type of progress bar to display.
#'
#' @return Invisible \code{con}.
#'
#' @export
db_execute <- function(con, statement, ..., progress = "none") {

  # # Set progress bar off if verbose
  # if (verbose) progress <- "none"

  # Do
  plyr::l_ply(
    statement,
    function(x) DBI::dbExecute(con, x, ...),
    .progress = progress
  )

  return(invisible(con))

}

#' Function to get row counts from database tables.
#'
#' The \code{row_count} variable will be a numeric value rather than an integer
#' to avoid integer overflow issues for large tables.
#'
#' @author Stuart K. Grange
#'
#' @param con Database connection.
#'
#' @param table Table name in \code{con}. If \code{table} is \code{NA}, all tables
#' in \code{con} will be queried.
#'
#' @param estimate Only for PostgreSQL, should the table count estimate be used?
#'
#' @param verbose Should the function give messages?
#'
#' @return Tibble.
#'
#' @export
db_count_rows <- function(con, table = NA, estimate = FALSE, verbose = FALSE) {

  # If no table is selected, do them all
  if (is.na(table[1])) table <- db_list_tables(con)

  # Check database
  if (length(table) == 0) stop("Database has no tables...", call. = FALSE)

  # Do
  df <- purrr::map_dfr(
    table,
    ~db_count_rows_worker(
      con,
      table = .x,
      estimate = estimate,
      verbose = verbose
    )
  )

  return(df)

}


# Function to get the row counts
db_count_rows_worker <- function(con, table, estimate, verbose) {

  if (verbose) message(date_message(), "Counting rows in `", table, "`...")

  if (estimate) {

    # Will only work for postgres
    sql <- str_c(
      "SELECT reltuples::bigint AS row_count
      FROM pg_class
      WHERE relname = '", table, "'"
    )

  } else {

    # Create statement, use real so 32 bit integers are not a limitation
    sql <- str_c(
      "SELECT CAST(COUNT(*) AS REAL) AS row_count
      FROM ",
      table
    )

    # Do not use the cast function here
    if (db.class(con) == "mysql") {
      sql <- str_c(
        "SELECT COUNT(*) AS row_count
         FROM ", table
      )
    }

  }

  # Use statement
  df <- tryCatch({
    db_get(con, stringr::str_squish(sql))
  }, error = function(e) {
    tibble(row_count = NA)
  })

  # Add table and order variables
  df <- tibble(table, row_count = df$row_count)

  return(df)

}

#' Function to return some high level details about a database.
#'
#' @param con Database connection.
#' @export
db_details <- function(con) {

  # Used twice
  tables <- db_list_tables(con)

  # Build tibble
  tibble(
    system = threadr::hostname(),
    date = as.numeric(lubridate::now()),
    class = db.class(con),
    size = db_size(con, unit = "mb"),
    table_count = length(tables),
    table_names = stringr::str_c(tables, collapse = "; ")
  )

}


#' Function to return some high level details about a database.
#'
#' @param con Database connection.
#'
#' @param table Database table to insert details into.
#'
#' @param print Should \code{table} be printed post-insert?
#'
#' @author Stuart K. Grange.
#'
#' @return Invisible \code{con}.
#'
#' @examples
#' \dontrun{
#'
#' # Get some information about the database and insert into table
#' db_details_insert(con)
#'
#' }
#'
#' @export
db_details_insert <- function(con, table = "database_details", print = FALSE) {

  # Get details
  df <- db_details(con)

  # Insert into database
  db_insert(con, table, df, replace = FALSE)

  # Read and print table after insert
  if (print) {
    db_read_table(con, table) %>%
      print(n = Inf)
  }

  return(invisible(con))

}

#' Function to get/fetch data from a database with a statement.
#'
#' \code{db_get} is a wrapper for \code{DBI::dbGetQuery}.
#'
#' @param con Database connection.
#' @param statement Statement to send to \code{con}.
#' @param warn Should the function return warnings?
#'
#' @return Tibble.
#'
#' @export
db_get <- function(con, statement, warn = TRUE) {

  if (warn) {

    df <- DBI::dbGetQuery(con, statement)

  } else {

    df <- suppressWarnings(
      DBI::dbGetQuery(con, statement)
    )

  }

  # Always a tibble
  df <- dplyr::as_tibble(df)

  return(df)

}

#' Function to list data types of database tables.
#'
#' \code{db_list_data_types} does not support MySQL databases at present.
#'
#' @author Stuart K. Grange
#'
#' @param con Database connection.
#'
#' @param table Tables to get data types for. If not used, all tables will be
#' returned.
#'
#' @return Data frame.
#'
#' @export
db_list_data_types <- function(con, table = NA) {

  if (is.na(table[1])) {

    # Make sql
    sql <- stringr::str_c(
      "SELECT table_name,
         column_name,
         data_type
         FROM information_schema.columns"
    )

  } else {

    # For sql
    table <- str_sql_quote(table)

    # Make sql
    sql <- stringr::str_c(
      "SELECT table_name,
        column_name,
        data_type
        FROM information_schema.columns
        WHERE table_name IN (", table, ")"
    )

  }

  # Clean statement
  sql <- stringr::str_squish(sql)

  # Query
  df <- db_get(con, sql)

  return(df)

}

str_sql_quote <- function(x, collapse = ",") {
  x <- stringr::str_c("'", x, "'")
  if (!is.na(collapse))
    x <- stringr::str_c(x, collapse = collapse)
  return(x)
}


