#' @include mortalityTable.R
NULL

setClassUnion("numericArray", c("numeric", "array", "matrix"))
setClassUnion("numericArrayOrNULL", c("numeric", "array", "matrix", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("mortalityTableOrNULL", c("mortalityTable", "NULL"))



#' Class mortalityTable.period - Period life tables
#'
#' A period life table, giving death probabilities for each age, up to
#' maximum age \code{omega}. The \code{baseYear} slot can be used to hold
#' information about the period.
#'
#' @slot ages       The ages corresponding to the entries of the deathProbs
#' @slot deathProbs The one-year death probabilities for the ages
#' @slot exposures  (Optional) exposured used to determine death probabilities
#'                  (can be used as weights for smoothing, for variances, etc.)
#'
#' @examples
#' linTable = mortalityTable.period(name="linear mortality", ages = 0:50, deathProbs = 0:50/50)
#' constTable = mortalityTable.period(name="const. mortality", ages = 0:50,
#'                                    deathProbs = c(rep(0.1, 50), 1))
#' plot(linTable, constTable, title="Comparison of linear and constand death probabilities")
#'
#' # A sample observation table with exposures and raw probabilities
#' obsTable = mortalityTable.period(
#'     name = "trivial observed table",
#'     ages = 0:15,
#'     deathProbs = c(
#'         0.0072, 0.00212, 0.00081, 0.0005, 0.0013,
#'         0.001, 0.00122, 0.00142, 0.007, 0.0043,
#'         0.0058, 0.0067, 0.0082, 0.0091, 0.0075, 0.01),
#'     exposures = c(
#'         150, 222, 350, 362, 542,
#'         682, 1022, 1053, 1103, 1037,
#'         968, 736, 822, 701, 653, 438))
#' plot(obsTable, title = "Observed death probabilities")
#'
#' @export mortalityTable.period
#' @exportClass mortalityTable.period
mortalityTable.period = setClass(
    "mortalityTable.period",
    slots = list(
        ages = "numeric",
        deathProbs = "numericArray",
        exposures = "numericArrayOrNULL",
        selectionFactors = "numericOrNULL",
        tableBeforeSelection = "mortalityTableOrNULL"
    ),
    prototype = list(
        ages = eval(0:120),
        deathProbs = rep(1,120),
        exposures = NULL,
        selectionFactors = NULL,
        tableBeforeSelection = NULL
    ),
    contains = "mortalityTable"
)

#' Generate a mortality table with all probabilities set to zero.
#'
#' @param name The name of the table
#' @param ages The ages of the table
#'
#' @export
mortalityTable.zeroes = function(name = "Zero mortality table", ages = 0:99) {
    mortalityTable.period(name = name, ages = ages, deathProbs = ages * 0)
}

#' Generate a (deterministic) mortality table with only one probability set to 1 (for the given age)
#'
#' @param transitionAge The age where the deterministic transition occurs
#' @param name The name of the table
#' @param ages The ages of the table
#'
#' @export
mortalityTable.once = function(transitionAge, name = "Deterministic mortality table", ages = 0:99) {
    mortalityTable.period(
        name = name,
        ages = ages,
        deathProbs = sapply(ages, function(x) { if (x == transitionAge) 1 else 0})
    )
}

#' Generate a (deterministic) mortality table with all probabilities starting at a given age set to 1
#'
#' @param transitionAge The age where the deterministic transition occurs
#' @param name The name of the table
#' @param ages The ages of the table
#'
#' @export
mortalityTable.onceAndFuture = function(transitionAge, name = "Deterministic mortality table", ages = 0:99) {
    mortalityTable.period(
        name = name,
        ages = ages,
        deathProbs = sapply(ages, function(x) { if (x >= transitionAge) 1 else 0})
    )
}


#' Empty mortality table indicating NA
#'
#' @export
mortalityTable.NA = mortalityTable.period(name = NA_character_, ages = NA_integer_, deathProbs = NA_real_)
