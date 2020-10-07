#' @include mortalityTable.R
NULL

#' Return the cohort life table as a \code{mortalityTable.period} object
#'
#' @param object The life table object (class inherited from mortalityTable)
#' @param YOB The birth year for which the life table should be calculated
#' @param ... Other parameters (currently unused)
#'
#' @examples
#' mortalityTables.load("Austria_Annuities")
#' tb75 = getCohortTable(AVOe2005R.male, YOB = 1975)
#' # The tb75 is a fixed table with no trend any more
#' plot(AVOe2005R.male, tb75, Period = 2017)
#'
#' @exportMethod getCohortTable
setGeneric("getCohortTable", function(object, YOB, ...) standardGeneric("getCohortTable"));

#' @describeIn getCohortTable Return the cohort life table as a
#'             \code{mortalityTable.period} object
setMethod("getCohortTable","mortalityTable",
          function(object, YOB, ...) {
              mortalityTable.period(
                  name = paste(object@name, ", YOB ", YOB),
                  baseYear = YOB,
                  ages = ages(object),
                  deathProbs = deathProbabilities(object, YOB = YOB, ...)
              );
          })


#' @describeIn getCohortTable Return the cohort life table as a
#'             \code{mortalityTable.period} object from the mortalityTable objects stored in the array
setMethod("getCohortTable", "array",
          function(object,  ...) {
              array(
                  lapply(object, getCohortTable,  ...),
                  dim = dim(object), dimnames = dimnames(object))
          })
#' @describeIn getCohortTable  Return the cohort life table as a
#'             \code{mortalityTable.period} object from the mortalityTable objects stored in the list
setMethod("getCohortTable", "list",
          function(object,  ...) {
              lapply(object, getCohortTable, ...)
          })

#' @describeIn getCohortTable Empty dummy function to handle unassigned variables
setMethod("getCohortTable", "NULL",
          function(object,  ...) {
              NULL
          })
