
# TODO: add ? parameters
# TODO: check Roxygen2 statements
# TODO: vignette
# TODO: register= heuristic
# TODO: packaging

# extract all occcurrences of pat
# @param x character vector
# @param pat regular expression
# @param simplify if TRUE (default) unlist, remove duplicates and sort
# @return char vec of matches (or list of char vecs if simplify is FALSE)
extract_pat <- function(x, pat = "[[:alnum:]._]+", simplify = TRUE) {
  m <- gregexec(pat, x)
  L <- regmatches(x, m)
  if (simplify) sort(unique(unlist(L))) else L
}

# Open a new connection
conn <- function(db, verbose) {
  if (db == ":memory:" || db == "") {
    if (verbose) message("dbConnect(duckdb())")
    dbConnect(duckdb(), read_only = TRUE)
  } else {
    path <- normalizePath(db, mustWork = FALSE)
    if (verbose) message("dbConnect(duckdb(), path = '", path, "')")
    dbConnect(duckdb(), path = path)
  }
}

# check if dbSendQuery result is fetchable
is_fetchable <- function(res) {
  nofetch <- res@stmt_lst$type != "SELECT" && 
    res@stmt_lst$type != "RELATION" && 
    res@stmt_lst$return_type != "QUERY_RESULT"
  !nofetch
}

# run sql statements
run <- function(con, x, verbose) {
  out <- NULL
  for(s in x) {
      message("# setting up for dbSendQuery")
      res <- dbSendQuery(con, s)
      if (is_fetchable(res)) {
        if (verbose) {
	  message("res <- dbSendQuery(con, '", s, "')")
          message("dbFetch(res)")
	}
        out <- dbFetch(res)
      } else if (verbose) message("dbSendQuery(con, '", s, "')")
  }
  out
}

# Remove tables and close connection
disconn <- function(con, names, verbose) {
  for (nam in names) {
    if (verbose) message("dbRemoveTable(con, '", nam, "')")
    dbRemoveTable(con, nam)
  }
  if (verbose) message("dbDisconnect(con)")
  dbDisconnect(con)
}

#' Run duckdb SQL statement(s) against R data frames.
#'
#' @param x character vector of sql statements.
#' @param verbose TRUE or FALSE.
#' @param db "" or ":memory:" mean use in memory database; otherwise, provide
#'   the name of an existing directory to use as an on disk database.  It will
#'   be created if it does not already exist.
#' @param register if TRUE then tables are registered (like views) rather than 
#'  copied to data base.  Default is TRUE.
#' @param envir environment in which to look for data frames.
#' @return Result of last sql statement in x. (If no sql statement specified 
#'  then if db is a previously opened duckdb connection object then close it 
#'  and if not then open a new connection and return it.)
#' Commonly x is specified and all other arguments are left at default.
#' @seealso
#' \url{https://raw.githubusercontent.com/ggrothendieck/sqldk/refs/heads/main/README} 
#' \url{https://raw.githubusercontent.com/ggrothendieck/sqldk/refs/heads/main/INSTALL}
#' @examples
#' sqldk("select * from iris limit 3")
#' @export
sqldk <- function(x, verbose = FALSE, db = "", register = FALSE,
  envir = parent.frame()) {

  # if no sql is input: 
  #  if db is a connection close it else open a connection & return it
  # if sql is input:
  #  if db is a connection then use it else open a connection & close it at end

  # is db a connection? (T/F)
  is_con <- inherits(db, "duckdb_connection") # is db a connection? (T/F)
  
  if (missing(x) || !length(x) || all(!nzchar(x))) { # no sql present
    ret <- if (is_con) disconn(db, names(dfs), verbose) else conn(db, verbose)
    return(ret) 
  } else { # sql present
    con <- if (is_con) db
        else { 
	  on.exit(disconn(con, names(dfs), verbose)); conn(db, verbose) 
        }
  }

  # write or register data frames
  dfs <- x |>
    extract_pat() |>
    mget(envir, "any", NA, inherits = TRUE) |>
    Filter(is.data.frame, x = _)
  for(nam in names(dfs)) {
    if (register) {
      if (verbose) message("duckdb_register(con, '", nam, "', ", nam, ")")
      duckdb_register(con, nam, dfs[[nam]])
    } else {
      if (verbose) message("dbWriteTable(con, '", nam, "', ", nam, ")")
      dbWriteTable(con, nam, dfs[[nam]])
    }
  }

  run(con, x, verbose)

}


