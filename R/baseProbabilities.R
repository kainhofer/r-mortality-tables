#' @include mortalityTable.R mortalityTable.period.R mortalityTable.ageShift.R mortalityTable.trendProjection.R mortalityTable.improvementFactors.R mortalityTable.mixed.R fillAges.R
NULL

#' Return the base death probabilities of the life table (with a possible
#' selection effect applied, but without further processing like modifications,
#' margins or trend extrapolation)
#'
#' @param object The life table object (class inherited from mortalityTable)
#' @param ... Other parameters (currently unused)
#' @param selectionAge Age when selection starts (for tables with the \code{deathProbs} slot being a matrix rather than a single vector and for tables with \code{selectionFactors set})
#'
#' @examples
#' baseProbabilities(DAV2004R.male.selekt)
#' baseProbabilities(DAV2004R.male.selekt, selectionAge = 60)
#' baseProbabilities(DAV2004R.male.selekt, selectionAge = 65)
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
                  rws = seq_along(cols)
                  if (object@selectInitialAge) {
                      # if data gives selection by initial age, make sure to
                      # walk along thw same row until the ultimate table is reached
                      rws = rws - cols + 1
                      # For ages before the first attained age of the ultimate table,
                      # use the select probabilities, even before the desired select age
                      cols = cols + pmin(0, rws - 1)
                      rws = pmax(1, rws)
                  }
                  # exctract the corresponding columns for each age:
                  # See https://stackoverflow.com/questions/20036255/get-the-vector-of-values-from-different-columns-of-a-matrix
                  # TODO: Check if any index is outside the existing dimensions!
                  probs = object@deathProbs[cbind(rws, cols)]
              }
              if (!is.null(object@selectionFactors)) {
                  if (is.vector(object@selectionFactors)) {
                      # Add a terminal 1 just in case, so that after the selection period, no modification occurs
                      sel.factors = c(object@selectionFactors, 1)
                      # For each age, determine the index into the selection factor
                      # (ages below selection Age get index 1) and multiply the base
                      # probability with the corresponding selection factor
                    sel.offset = pmin(length(sel.factors), offsets)
                    factors = sel.factors[sel.offset]
                  } else {
                      # Assume that the array / data.frame given as selectionFactors
                      # has the same ages as the base table
                      # Optionally, allow an "age" or "Age" column in the data.frame or array
                      # (of course, remove that column before extracting the factors)
                      sf = cbind(object@selectionFactors, Ultimate = 1)
                      ages = ages(object)
                      if ("age" %in% colnames(sf)) {
                          ages = sf$age
                          sf$age = NULL
                      }
                      if ("Age" %in% colnames(sf)) {
                          ages = sf$Age
                          sf$Age = NULL
                      }
                      # Extract the selection factors just like the base probabilities
                      # were extracted from the array / data.frame
                      offsets = pmax(1, ages - selectionAge + 1)
                      cols = pmin(ncol(sf), offsets)
                      rws = seq_along(cols)
                      if (object@selectInitialAge) {
                          # if data gives selection by initial age, make sure to
                          # walk along thw same row until the ultimate table is reached
                          rws = rws - cols + 1
                          # For ages before the first attained age of the ultimate table,
                          # use the select probabilities, even before the desired select age
                          cols = cols + pmin(0, rws - 1)
                          rws = pmax(1, rws)
                      }
                      # exctract the corresponding columns for each age:
                      # See https://stackoverflow.com/questions/20036255/get-the-vector-of-values-from-different-columns-of-a-matrix
                      # TODO: Check if any index is outside the existing dimensions!
                      factors = as.matrix(sf)[cbind(rws, cols)]

                    # TODO: handle age-specific selection factors (e.g. 1980 CSO 10-year selection factors)

                  }
                  probs = factors * probs
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


