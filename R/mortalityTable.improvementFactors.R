#' @include mortalityTable.R mortalityTable.period.R
NULL

setClassUnion("vectorOrMatrix", c("numeric", "matrix"))

#' Class mortalityTable.improvementFactors - Cohort life table with improvement
#' factors
#'
#' A cohort life table, obtained by an improvment factor projection
#' from a given base table (PODs for a given observation year). The yearly
#' improvement factors are often (in particular in the US) called "scale".
#'
#' @slot baseYear    The base year for the improvements (\code{baseTable}
#'                   describes the death probabilities in this year)
#' @slot improvement Yearly improvement factors per age
#'
#' @examples
#' mortalityTables.load("Austria_Annuities_AVOe2005R")
#' # AVOe 2005R base table with yearly improvements of 3% for age 0, linearly
#' # decreasing to 0% for age 120.
#' tb = mortalityTable.improvementFactors(
#'     ages = ages(AVOe2005R.male),
#'     deathProbs = periodDeathProbabilities(AVOe2005R.male, Period = 2002),
#'     baseYear = 2002,
#'     improvement = 0.03 * (1 - ages(AVOe2005R.male)/121),
#'     name = "AVOe 2005R base with linearly falling improvements (DEMO)"
#' )
#' # Yearly trend is declining:
#' plotMortalityTrend(tb, AVOe2005R.male, Period = 2017, title = "Mortality Trend")
#' # The cohort tables for different birth years:
#' plot(getCohortTable(tb, YOB = 1963), getCohortTable(tb, YOB = 2017))
#'
#' @export mortalityTable.improvementFactors
#' @exportClass mortalityTable.improvementFactors
mortalityTable.improvementFactors = setClass(
    "mortalityTable.improvementFactors",
    slots = list(
        baseYear = "numeric",
        improvement = "vectorOrMatrix"
    ),
    prototype = list(
        baseYear = 2012,
        improvement = rep(0,120)
    ),
    contains = "mortalityTable.period"
)



#' Calculate the improvement factors for the given birth-year and the
#' \code{\linkS4class{mortalityTable.improvementFactors}} object
#'
#' @param object A pension table object (instance of a \code{\linkS4class{mortalityTable.improvementFactors}} class)
#' @param ... Currently unused
#' @param Period Observation period (either \code{Period} or \code{YOB} should be given)
#' @param YOB Year of birth (either \code{Period} or \code{YOB} should be given)
#'
#' @examples
#' pensionTables.load("USA_PensionPlan_RP2014")
#' calculateImprovements(RP2014.male@qx, YOB = 2017)
#'
#' @exportMethod calculateImprovements
setGeneric("calculateImprovements", function(object, ...) standardGeneric("calculateImprovements"));

#' @describeIn calculateImprovements Calculate the total mortality improvement
#' factors relative to the base year for the given birth-year and the
#' \code{\linkS4class{mortalityTable.improvementFactors}} object
setMethod("calculateImprovements", "mortalityTable.improvementFactors",
    function(object, ..., Period = NULL, YOB = 1982) {
        if (is.array(object@improvement)) {
            # All years outside the observation interval use the improvements
            # at the boundaries
            minObservation = strtoi(utils::head(colnames(object@improvement), 1))
            maxObservation = strtoi(utils::tail(colnames(object@improvement), 1))

            if (!missing(Period) && !is.null(Period)) {
                # Period improvements:
                if (Period == object@baseYear) {
                    ags = ages(object)
                    improvements = rep(1, length(ags))
                    names(improvements) = ags
                } else if (Period < object@baseYear) {
                    # Past mortalities
                    years = Period:(object@baseYear - 1)
                    years = sapply(years, function(x) { max(x, minObservation) } )
                    imprY = 1 - object@improvement[,as.character(years), drop = FALSE]
                    improvements = 1 / apply(imprY, 1, prod)
                } else {
                    # Projection into the future
                    years = object@baseYear:(Period - 1)
                    years = sapply(years, function(x) { min(x, maxObservation) } )
                    imprY = 1 - object@improvement[,as.character(years), drop = FALSE]
                    improvements = apply(imprY, 1, prod)
                }
            } else {
                # Generational improvements
                ags = ages(object)
                # For each age, determine the year and the improvement factors
                # until/from the base year and multiply them
                improvements = sapply(ags, function(a) {
                    yr = YOB + a
                    if (yr == object@baseYear) {
                        1
                    } else if (yr < object@baseYear) {
                        # Past mortalities
                        years = yr:(object@baseYear - 1)
                        years = sapply(years, function(x) { max(x, minObservation) } )
                        imprY = 1 - object@improvement[as.character(a), as.character(years)]
                        1 / prod(imprY)
                    } else {
                        # Projection into the future
                        years = object@baseYear:(yr - 1)
                        years = sapply(years, function(x) { min(x, maxObservation) } )
                        imprY = 1 - object@improvement[as.character(a), as.character(years)]
                        prod(imprY)
                    }
                })
                names(improvements) = ags
            }
        } else {
            if (!missing(Period) && !is.null(Period)) {
                improvements = (1 - object@improvement) ^ (Period - object@baseYear);
            } else {
                improvements = (1 - object@improvement) ^ (YOB + ages(object) - object@baseYear);
            }
        }
        improvements
    }
)

