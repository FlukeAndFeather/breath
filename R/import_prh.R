#' Import a PRH into the CATS database
#'
#' @param prh a PRH object
#' @param cats_db connection to CATS database (see \code{\link[DBI]{dbConnect}})
#' @param overwrite overwrite deployment in database? (FALSE by default)
#' @param verbose log progress to console? (FALSE by default)
#'
#' @return TRUE on success
#' @export
#'
#' @examples
#' bw180905_53 <- read_prh_nc("bw180905-53", "/Volumes/COPYCATSdat/CATS/")
#' db_path <- "/Volumes/COPYCATSdat/CATS/cats.sqlite"
#' cats_db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
#' import_prh(bw180905_53, cats_db, verbose = TRUE)
#' DBI::dbDisconnect(cats_db)
import_prh <- function(prh, cats_db, overwrite = FALSE, verbose = FALSE) {
  stopifnot(is_prh(prh))
  depid <- get_depid(prh)

  if (verbose) {
    futile.logger::flog.threshold(futile.logger::TRACE, name = "breath")
  }
  futile.logger::flog.trace(glue::glue("Start importing {depid}..."))
  stopifnot(is.character(depid) && length(depid) == 1)

  for (sensor in c("A", "M", "G")) {
    for (axis in 1:3) {
      newcol <- paste0(sensor, axis)
      prh[[newcol]] <- prh[[sensor]][, axis]
    }
    prh[[sensor]] <- NULL
  }
  prh$depid <- get_depid(prh)
  class(prh) <- "data.frame"

  # If database is empty, initiate with PRH
  if (length(DBI::dbListTables(cats_db)) == 0) {
    futile.logger::flog.trace("...database empty, initializing...")
    DBI::dbWriteTable(cats_db, "prh", prh)
    DBI::dbWriteTable(cats_db, "tag_guide", attr(prh, "metadata"))
  } else {
    # If deployment is already in database, either overwrite or stop
    db_depids <- DBI::dbGetQuery(cats_db, "SELECT depid FROM tag_guide")
    if (depid %in% db_depids) {
      if (overwrite) {
        futile.logger::flog.trace("...deployment in database and overwrite is TRUE...")
        DBI::dbGetQuery(
          cats_db,
          glue::glue("DROP FROM prh WHERE depid = {depid}")
        )
        DBI::dbGetQuery(
          cats_db,
          glue::glue("DROP FROM tag_guide WHERE depid = {depid}")
        )
        DBI::dbWriteTable(cats_db, "prh", prh, append = TRUE)
        DBI::dbWriteTable(cats_db, "tag_guide", attr(prh, "metadata"), append = TRUE)
      } else {
        if (verbose) {
          futile.logger::flog.threshold(futile.logger::INFO, name = "breath")
        }
        stop("Deployment in database and overwrite = FALSE")
      }
    } else {
      futile.logger::flog.trace("...deployment not in database...")
      # Otherwise, append to existing tables
      DBI::dbWriteTable(cats_db, "prh", prh, append = TRUE)
      DBI::dbWriteTable(cats_db, "tag_guide", attr(prh, "metadata"), append = TRUE)
    }
  }
  futile.logger::flog.trace("...done!")

  if (verbose) {
    futile.logger::flog.threshold(futile.logger::INFO, name = "breath")
  }

  TRUE
}
