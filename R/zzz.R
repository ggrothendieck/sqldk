#' @import DBI
#' @import duckdb
NULL

#' The sqldk package
#' 
#' sqldk is a package to run duckdb SQL statements on R data frames.
#' 
#' There is a single user accessible function, sqldk.  It is usually called 
#' with a single argument which is a character string or vector specifying 
#' SQL statement(s).
#' 
#' See the \url{https://duckdb.org/docs} for information on duckdb itself.
#'
#' @examples
#' DF <- data.frame(a = 1:3, b = 4:6)
#' sqldk("select a, b, a + b as c from DF where a > 1")
"_PACKAGE"
