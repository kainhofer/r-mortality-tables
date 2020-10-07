#' @include mortalityTable.R mortalityTable.period.R mortalityTable.ageShift.R mortalityTable.trendProjection.R mortalityTable.improvementFactors.R mortalityTable.mixed.R fillAges.R
NULL

#' Return the base death probabilities of the life table
#'
#' @param object The life table object (class inherited from mortalityTable)
#' @param ... Other parameters (currently unused)
#' @param selectionAge Age when selection starts (for tables with the \code{deathProbs} slot being a matrix rather than a single vector and for tables with \code{selectionFactors set})
#'
#' @examples
#' # TODO
#'
#' @exportMethod baseProbabilities
setGeneric("baseProbabilities", function(object, ..., selectionAge = NULL) standardGeneric("baseProbabilities"));

#' @describeIn baseProbabilities Return the (cohort) death probabilities of the
#'                                life table given the birth year (if needed)
setMethod("baseProbabilities", "mortalityTable.period",
          function(object, ..., selectionAge = -Inf) {
              # Offset for selection effects (relative to selectionAge, at least 1), will be maximized with the length of the selection effect later on
              offsets = pmax(1, ages(object) - selectionAge + 1)
              # For selection tables use the selection age to switch from the first column of @deathProbs to the last (ultimate probabilities)
              if (is.vector(object@deathProbs)) {
                  probs = object@deathProbs
              } else {
                  cols = pmin(ncol(object@deathProbs), offsets)
                  # exctract the corresponding columns for each age:
                  # See https://stackoverflow.com/questions/20036255/get-the-vector-of-values-from-different-columns-of-a-matrix
                  probs = object@deathProbs[cbind(seq_along(cols), cols)]
              }
              if (!is.null(object@selectionFactors)) {
                  # Add a terminal 1 just in case, so that after the selection period, no modification occurs
                  sel.factors = c(object@selectionFactors, 1)
                  # For each age, determine the index into the selection factor
                  # (ages below selection Age get index 1) and multiply the base
                  # probability with the corresponding selection factor
                  sel.offset = pmin(length(sel.factors), offsets)
                  probs = sel.factors[sel.offset] * probs
              }
              object@modification(probs * (1 + object@loading))
          })

#' @describeIn baseProbabilities Return the (cohort) death probabilities of the
#'                                life table given the birth year (if needed)
setMethod("baseProbabilities","mortalityTable.mixed",
          function(object,  ..., selectionAge = NULL) {
              qx1 = baseProbabilities(object@table1, ..., selectionAge = selectionAge);
              qx2 = baseProbabilities(object@table2, ..., selectionAge = selectionAge);
              mixedqx = (object@weight1 * qx1 + object@weight2 * qx2)/(object@weight1 + object@weight2) * (1 + object@loading);
              # We already have the correct ages from the baseProbabilities call above
              object@modification(mixedqx)
          })

