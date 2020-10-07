#' @include mortalityTable.R mortalityTable.period.R mortalityTable.trendProjection.R mortalityTable.improvementFactors.R mortalityTable.mixed.R fillAges.R
NULL

#' Return the (period) death probabilities of the life table for a given
#' observation year
#'
#' @param object The life table object (class inherited from mortalityTable)
#' @param ... Other parameters (currently unused)
#' @param ages Desired age range (if NULL, the probabilities of the age range provided by the table will be returned), missing ages will be filled with NA
#' @param Period  The observation year for which the period death probabilities should be determined
#'
#' @examples
#' mortalityTables.load("Austria_Annuities")
#' periodDeathProbabilities(AVOe2005R.male, Period = 1975)
#' periodDeathProbabilities(AVOe2005R.male, Period = 2017)
#'
#' @exportMethod periodDeathProbabilities
setGeneric("periodDeathProbabilities", function(object, ..., ages = NULL, Period = 1975) standardGeneric("periodDeathProbabilities"));

#' @describeIn periodDeathProbabilities Return the (period) death probabilities
#'             of the life table for a given observation year
setMethod("periodDeathProbabilities", "mortalityTable.period",
          function(object, ..., ages = NULL, Period = 1975) {
              fillAges(
                  object@modification(baseProbabilities(object, ...) * (1 + object@loading)),
                  givenAges = ages(object),
                  neededAges = ages)
          })

#' @describeIn periodDeathProbabilities Return the (period) death probabilities
#'             of the life table for a given observation year
setMethod("periodDeathProbabilities", "mortalityTable.ageShift",
          function (object,  ..., ages = NULL, Period = 1975) {
              # TODO
              qx = baseProbabilities(object, ...) * (1 + object@loading);
              # TODO!!!
              # shift.index = match(YOB, object@shifts, 0);
              # if (shift.index) {}
              fillAges(
                  object@modification(qx),
                  givenAges = ages(object),
                  neededAges = ages)
          })

#' @describeIn periodDeathProbabilities Return the (period) death probabilities
#'             of the life table for a given observation year
setMethod("periodDeathProbabilities", "mortalityTable.trendProjection",
          function(object,  ..., ages = NULL, Period = 1975) {
              qx = baseProbabilities(object, ...) * (1 + object@loading);
              if (is.null(object@trend2) || length(object@trend2) <= 1) {
                  # ages = 0:(length(qx)-1);
                  damping = object@dampingFunction(Period - object@baseYear);
                  finalqx = exp(-object@trend * damping) * qx;
              } else {
                  # dampingFunction interpolates between the two trends:
                  weight = object@dampingFunction(Period);
                  finalqx = qx * exp(
                      -(object@trend * (1 - weight) + object@trend2 * weight) *
                          (Period - object@baseYear))
              }
              fillAges(
                  object@modification(finalqx),
                  givenAges = ages(object),
                  neededAges = ages)
          })

#' @describeIn periodDeathProbabilities Return the (period) death probabilities
#'             of the life table for a given observation year
setMethod("periodDeathProbabilities", "mortalityTable.improvementFactors",
          function(object, ..., ages = NULL, Period = 1975) {
              qx = baseProbabilities(object, ...) * (1 + object@loading);
              impr = calculateImprovements(object, ..., Period = Period)
              fillAges(
                  object@modification(qx * impr),
                  givenAges = ages(object),
                  neededAges = ages)
          })

#' @describeIn periodDeathProbabilities Return the (period) death probabilities
#'             of the life table for a given observation year
setMethod("periodDeathProbabilities", "mortalityTable.mixed",
          function(object,  ..., ages = NULL, Period = 1975) {
              qx1 = periodDeathProbabilities(object@table1, ..., ages = ages, Period = Period);
              qx2 = periodDeathProbabilities(object@table2, ..., ages = ages, Period = Period);
              mixedqx = (object@weight1 * qx1 + object@weight2 * qx2) / (object@weight1 + object@weight2) * (1 + object@loading);
              # We already have the correct ages from the deathProbabilities call above
              object@modification(mixedqx)
          })

