#' @include mortalityTable.ageShift.R
NULL

#' Return the age shift of the age-shifted life table given the birth year
#'
#' @param object The life table object (class inherited from mortalityTable)
#' @param ... Other parameters (currently unused)
#' @param YOB The birth year for which the age shift should be determined.
#'
#' @examples
#' mortalityTables.load("Austria_Annuities")
#' ageShift(AVOe2005R.male.av, YOB=1910)
#' ageShift(AVOe2005R.male.av, YOB=1955)
#' ageShift(AVOe2005R.male.av, YOB=2010)
#' # A table with trend does NOT have any age shift, so NA is returned:
#' ageShift(AVOe2005R.male, YOB=1910)
#'
#' @exportMethod ageShift
setGeneric("ageShift", function(object, YOB=1975, ...) standardGeneric("ageShift"));

#' @describeIn ageShift Age shifts apply only to mortalityTagle.ageShift, so
#'             all other tables return NA.
setMethod("ageShift", "mortalityTable", function(object, YOB, ...) {
    NA
})

#' @describeIn ageShift Return the age shift of the age-shifted life table
#'                      given the birth year
setMethod("ageShift",
          "mortalityTable.ageShift",
          function(object, YOB, ...) {
              shifts = object@ageShifts
              if (NCOL(shifts) == 1) {
                  .Deprecated(new = "ageShift", msg = "Defining age shifts by a single-column data.frame with birth years as rownames is deprecated. Please switch to a dataframe with columns c(\"from\", \"to\", \"shift\")!")
                  # old-style data.frame with years as rownames
                  shift = shifts[[toString(YOB),1]]
                  if (!is.na(shift)) {
                      return(shift)
                  } else {
                      colnames(shifts) = c("shift")
                      shifts$from = as.numeric(rownames(shifts))
                      shifts$to = shifts$from
                  }
              } else if ("YOB" %in% colnames(shifts)) {
                  shifts$from = shifts$YOB
                  shifts$to = shifts$YOB
              }

              for(i in 1:nrow(shifts)) {
                  if (
                      ((shifts[i,"from"] <= YOB) || is.na(shifts[i,"from"])) &&
                      (YOB <= shifts[i, "to"] || is.na(shifts[i, "to"]))
                     ) {
                      return(shifts[[i, "shift"]])
                  }
              }
              warning("Unable to determine age-shift for birth year ", YOB, " with table \"", object@name, "\". Default of 0 is used.")
              return(0)
          })

