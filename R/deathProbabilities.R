#' @include mortalityTable.R mortalityTable.period.R mortalityTable.ageShift.R mortalityTable.trendProjection.R mortalityTable.improvementFactors.R mortalityTable.mixed.R fillAges.R
NULL

#' Return the (cohort) death probabilities of the life table given the birth year (if needed)
#'
#' @param object The life table object (class inherited from mortalityTable)
#' @param ... Other parameters (currently unused)
#' @param ages Desired age range (if NULL, the probabilities of the age range provided by the table will be returned), missing ages will be filled with NA
#' @param YOB The birth year for which the death probabilities should be calculated
#'
#' @examples
#' mortalityTables.load("Austria_Annuities")
#' deathProbabilities(AVOe2005R.male, YOB = 1975)
#' deathProbabilities(AVOe2005R.male, YOB = 2017)
#'
#' @exportMethod deathProbabilities
setGeneric("deathProbabilities", function(object, ..., ages = NULL, YOB = 1975) standardGeneric("deathProbabilities"));

#' @describeIn deathProbabilities Return the (cohort) death probabilities of the
#'                                life table given the birth year (if needed)
setMethod("deathProbabilities", "mortalityTable.period",
          function(object, ..., ages = NULL, YOB = 1975) {
              fillAges(
                  object@modification(baseProbabilities(object, ...) * (1 + object@loading)),
                  givenAges = ages(object),
                  neededAges = ages
              );
          })

#' @describeIn deathProbabilities Return the (cohort) death probabilities of the
#'                                life table given the birth year (if needed)
setMethod("deathProbabilities","mortalityTable.ageShift",
          function(object,  ..., ages = NULL, YOB = 1975) {
              qx = baseProbabilities(object, ...) * (1 + object@loading);
              shift = ageShift(object, YOB);
              if (shift > 0) {
                  qx = c(qx[(shift + 1):length(qx)], rep(qx[length(qx)], shift));
              } else if (shift < 0) {
                  qx = c(rep(0, -shift), qx[1:(length(qx) - (-shift))])
              }
              fillAges(object@modification(qx), givenAges = ages(object), neededAges = ages)
          })

#' @describeIn deathProbabilities Return the (cohort) death probabilities of the
#'                                life table given the birth year (if needed)
setMethod("deathProbabilities","mortalityTable.trendProjection",
          function(object,  ..., ages = NULL, YOB = 1975) {
              qx = baseProbabilities(object, ...) * (1 + object@loading);
              givenAges = ages(object)
              if (is.null(object@trend2) || length(object@trend2) <= 1) {
                  damping = sapply(
                      givenAges,
                      function(age) { object@dampingFunction(YOB + age - object@baseYear) }
                  );
                  finalqx = exp(-object@trend * damping) * qx;
              } else {
                  # dampingFunction interpolates between the two trends:
                  # The damping functions does NOT give yearly weights,
                  # but rather cumulative weights from the base year until the observation year!
                  weights = sapply(YOB + 0:(length(qx) - 1), object@dampingFunction);
                  finalqx = qx * exp(
                      -(object@trend * (1 - weights) + object@trend2 * weights) *
                          (YOB + 0:(length(qx) - 1) - object@baseYear))
              }
              fillAges(object@modification(finalqx), givenAges = givenAges, neededAges = ages)
          })

#' @describeIn deathProbabilities Return the (cohort) death probabilities of the
#'                                life table given the birth year (if needed)
setMethod("deathProbabilities","mortalityTable.improvementFactors",
          function(object,  ..., ages = NULL, YOB = 1975) {
              qx = baseProbabilities(object, ...) * (1 + object@loading);
              impr = calculateImprovements(object, ..., YOB = YOB)
              fillAges(
                  object@modification(impr * qx),
                  givenAges = ages(object),
                  neededAges = ages)
          })

#' @describeIn deathProbabilities Return the (cohort) death probabilities of the
#'                                life table given the birth year (if needed)
setMethod("deathProbabilities","mortalityTable.mixed",
          function(object,  ..., ages = NULL, YOB = 1975) {
              qx1 = deathProbabilities(object@table1, ..., ages = ages, YOB = YOB);
              qx2 = deathProbabilities(object@table2, ..., ages = ages, YOB = YOB);
              mixedqx = (object@weight1 * qx1 + object@weight2 * qx2)/(object@weight1 + object@weight2) * (1 + object@loading);
              # We already have the correct ages from the deathProbabilities call above
              object@modification(mixedqx)
          })

