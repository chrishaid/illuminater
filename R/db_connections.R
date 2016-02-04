#' @title connect to illuminat database or warehouse
#'
#' @description Simple wrapper function that establishes a conect to a database
#' that is suitable for using with \code{\link[dplyr]{dplyr}}.
#' @param server the IP address of the data base server
#' @param database the name of the database to be connected to
#' @param port  the port number on which the database server listens
#' @param user  a user name
#' @param password the user's password
#' @param db_type one of either "warehouse" or "direct".  "Warehouse" connects to
#' a SQL server database.  "Direct" connects to site specific PostgreSQL instance
#' maintained by Illuminate. You'll need to get our computer's (i.e. the machine
#' running using this R package) IP adress whilelisted by Illuminate.  One benefit of
#' the "warehouse" setting is that you can connect other databases (say, a PowerSchool
#' mirror).
#'
#'
#'
#' @export
connect_illuminate <- function(server,
                               database,
                               port,
                               user,
                               password,
                               db_type = "warehouse") {

  # Check that db_typ
  if (!db_type %in% c("warehouse", "direct")) stop("db_type can only be 'warehouse' or 'direct'.")

  if (db_type == "warehouse") {
      ill_db <- RSQLServer:::src_sqlserver(server = server,
                                          database = database,
                                          properties = list(user = user,
                                                            password = password
                                                            )
                                          )
  } else {
    ill_db <- dplyr::src_postgres(
      server = server,
      database = database,
      user = user,
      password = password
      )
  }
  # Return
  class(ill_db) <- c(class(ill_db), "src_illuminater")

  ill_db
}

#' @export
is.src_illuminater <- function(x) inherits(x, "src_illuminater")

