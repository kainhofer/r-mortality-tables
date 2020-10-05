#' @include mortalityTable.trendProjection.R
NULL

#' Return a \code{mortalityTable.trendProjection} object with the trend damping removed.
#'
#' @param object The life table object (class inherited from mortalityTable)
#'
#' @examples
#' mortalityTables.load("Austria_Annuities")
#' AVOe2005R.male.undamped = undampenTrend(AVOe2005R.male)
#' AVOe2005R.male.undamped@name = paste(AVOe2005R.male.undamped@name, "no trend dampening")
#'
#' plot(AVOe2005R.male, AVOe2005R.male.undamped,
#'     title = "AVOe 2005R with trend dampening and without", YOB = 2000)
#'
#' @exportMethod undampenTrend
setGeneric("undampenTrend", function(object) standardGeneric("undampenTrend"));

#' @describeIn undampenTrend Return a \code{mortalityTable.trendProjection}
#'             object with the trend damping removed.
setMethod("undampenTrend", "mortalityTable.trendProjection",
          function(object) {
              object@dampingFunction = identity;
              object
          })

#' @describeIn undampenTrend Empty dummy function to handle unassigned variables
setMethod("undampenTrend", "NULL",
          function(object) {
              NULL
          })

