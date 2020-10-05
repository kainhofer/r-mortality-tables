#' @include mortalityTable.R mortalityTable.trendProjection.R mortalityTable.improvementFactors.R pensionTable.R
NULL


fitExtrapolationLaw = function(data, ages, data.ages = ages, Dx = NULL, Ex = NULL, qx = NULL, method = "LF2", law = "HP", fit = 75:99, extrapolate = 80:120, fadeIn = 80:90, fadeOut = NULL, verbose = FALSE) {
    # Add the extrapolate ages to the needed ages
    neededAges = union(ages, extrapolate)
    # constrain the fit and fade-in range to given ages
    fit = intersect(ages, fit)
    if (!is.null(fadeIn))
        fadeIn = intersect(ages, fadeIn)
    if (!is.null(fadeOut))
        fadeOut = intersect(ages, fadeOut)

    # Hohe Alter: Fitte Heligman-Pollard im Bereich 75-99
    fitLaw = MortalityLaws::MortalityLaw(
        x = data.ages, Dx = Dx, Ex = Ex, qx = qx,
        law = law, opt.method = method,
        fit.this.x = fit)
    # summary(fitAP.m.75.99)
    # plot(fitAP.m.75.99)
    qPredict = predict(fitLaw, extrapolate)

    weights = rep(0, length(neededAges))
    names(weights) = neededAges
    if (!is.null(fadeIn)) {
        weights[neededAges < min(fadeIn)] = 0
        fadeInLen = length(fadeIn);
        weights[match(fadeIn, neededAges)] = (0:(fadeInLen - 1)) / (fadeInLen - 1)
        weights[neededAges > max(fadeIn)] = 1
    } else if (!is.null(fadeOut)) {
        weights[neededAges < min(fadeOut)] = 1
        fadeOutLen = length(fadeOut);
        weights[match(fadeOut, neededAges)] = ((fadeOutLen - 1):0) / (fadeOutLen - 1)
        weights[neededAges > max(fadeOut)] = 0
    }

    probs = fillAges(qPredict, givenAges = extrapolate, neededAges = neededAges, fill = 0) * weights +
        fillAges(data, givenAges = ages, neededAges = neededAges, fill = 0) * (1 - weights)

    if (verbose) {
        list(probs = probs, law = fitLaw, weights = weights)
    } else {
        probs
    }
}




#' Fit an exponential function exp(-A*(x-x0)) to the last value (f(100) and f'(100) need to coincide):
#'
#' @param data data.frame to which an exponential function should be fit
#' @param idx Index of the position of the fit
#' @param up Whether the fit is forward- or backward-facing
#' @param verbose Whether to include data about the fit in the output
#'
#' @export
fitExpExtrapolation = function(data, idx, up = TRUE, verbose = FALSE) {
    # browser()
    # Anchor point of the extrapolation
    f0 = data[[idx]]
    if (up) {
        A = -(data[[idx]] - data[[idx - 1]]) / f0
    } else {
        A = -(data[[idx + 1]] - data[[idx]]) / f0
    }
    x0 = idx + (log(f0) / A)
    fun.extra = function(x) exp(-A*(x - x0))
    if (up) {
        newdata = c(data[1:idx], sapply((idx + 1):length(data), fun.extra))
    } else {
        newdata = c(sapply(1:(idx - 1), fun.extra), data[idx:length(data)])
    }
    if (verbose) {
        list(data = newdata, A = A, x0 = x0, idx = idx)
    } else {
        newdata
    }
}


#' Sets a new name for the given mortality table or the list/table/array of mortalityTables
#'
#' @param table A life table object (instance of a \code{mortalityTable} class) or a list, table or array of mortalityTable objects
#' @param name New name for the table.
#'
#' @examples
#' mortalityTables.load("Austria_Annuities")
#' mT.setName(AVOe2005R.male, name = "Austrian male Annuity table 2005-R")
#'
#' @export
mT.setName = function(table, name) {
    if (is.array(table)) {
        return(array(
            lapply(table, mT.setName, name = name),
            dim = dim(table), dimnames = dimnames(table))
        )
    } else if (is.list(table)) {
        return(lapply(table, mT.setName, name = name))
    } else if (is.na(c(table))) {
        return(table)
    }
    if (!is(table, "mortalityTable"))
        stop("First argument must be a mortalityTable or a list of mortalityTable objects.")

    table@name = name
    table
}


#' Restrict/expand a mortalityTable to certain ages
#'
#' Restrict the given \code{mortalityTable} object(s) to given ages, potentially filling with NA values to ensure they cover the full desired age range
#'
#' @param table A life table object (instance of a \code{mortalityTable} class) or a list, table or array of mortalityTable objects
#' @param neededAges The vector of ages the returned objects should cover (even if the values are 0 or NA)
#' @param fill The value to use for all ages for which the original table(s) do not have any information
#'
#' @examples
#' mortalityTables.load("Austria_Annuities")
#' # return a table with only ages 100-130, where ages above 120 (not defined
#' # in the original table) are filled with qx=1:
#' mT.fillAges(AVOe2005R.male, neededAges = 100:130, fill = 1)
#'
#' @export
mT.fillAges = function(table, neededAges, fill = 0) {
    if (is.array(table)) {
        return(array(
            lapply(table, mT.fillAges, neededAges = neededAges, fill = fill),
            dim = dim(table), dimnames = dimnames(table))
        )
    } else if (is.list(table)) {
        return(lapply(table, mT.fillAges, neededAges = neededAges, fill = fill))
    } else if (is.na(c(table))) {
        return(table)
    }
    if (!is(table, "mortalityTable"))
        stop("First argument must be a mortalityTable or a list of mortalityTable objects.")

    existingAges = ages(table)
    if (.hasSlot(table, "ages"))
        table@ages = neededAges
    if (.hasSlot(table, "deathProbs"))
        table@deathProbs = fillAges(table@deathProbs, givenAges = existingAges, neededAges = neededAges, fill = fill)
    if (.hasSlot(table, "exposures") && !is.null(table@exposures) && length(table@exposures) > 1)
        table@exposures = fillAges(table@exposures, givenAges = existingAges, neededAges = neededAges, fill = 0)
    if (.hasSlot(table, "trend") && !is.null(table@trend) && length(table@trend) > 1)
        table@trend = fillAges(table@trend, givenAges = existingAges, neededAges = neededAges, fill = 0)
    if (.hasSlot(table, "trend2") && !is.null(table@trend2) && length(table@trend2) > 1)
        table@trend2 = fillAges(table@trend2, givenAges = existingAges, neededAges = neededAges, fill = 0)
    if (.hasSlot(table, "loading") && !is.null(table@loading) && length(table@loading) > 1)
        table@loading = fillAges(table@loading, givenAges = existingAges, neededAges = neededAges, fill = 0)
    if (!is.null(table@data$deaths))
        table@data$deaths = fillAges(table@data$deaths, givenAges = existingAges, neededAges = neededAges, fill = 0)
    if (!is.null(table@data$rawProbs))
        table@data$rawProbs = fillAges(table@data$rawProbs, givenAges = existingAges, neededAges = neededAges, fill = 0)
    table
}

#' Scale all probabilities of the given \code{mortalityTable} object(s) by the given factor
#'
#' @param table A life table object (instance of a \code{mortalityTable} class) or a list, table or array of mortalityTable objects
#' @param factor Scaling factor for the probabilities (1.0 means unchanged)
#' @param name.postfix String to append to the original name of the table
#' @param name New name, overwriting the existing name of the table (takes precedence over \code{name.postfix})
#'
#' @examples
#' mortalityTables.load("Austria_Annuities")
#' mT.scaleProbs(AVOe2005R.male, 1.5) # Add 50% to all death probabilities of the table
#'
#' @export
mT.scaleProbs = function(table, factor = 1.0, name.postfix = "scaled", name = NULL) {
    if (is.array(table)) {
        return(array(
            lapply(table, mT.scaleProbs, factor = factor, name.postfix = name.postfix, name = name),
            dim = dim(table), dimnames = dimnames(table))
        )
    } else if (is.list(table)) {
        return(lapply(table, mT.scaleProbs, factor = factor, name.postfix = name.postfix, name = name))
    } else if (is.na(c(table))) {
        return(table)
    }
    if (!is(table, "mortalityTable"))
        stop("First argument must be a mortalityTable or a list of mortalityTable objects.")

    table@deathProbs = factor * table@deathProbs
    if (is.null(name)) {
        if (!is.null(name.postfix)) {
            name = paste(table@name, name.postfix)
        }
    }
    if (!is.null(name)) {
        table@name = name
    }
    table
}


#' Set/Add a trend vector for the probabilities of the given \code{mortalityTable} object(s). Returns a \code{mortalityTable.trendProjection} object
#'
#' @param table A life table object (instance of a \code{mortalityTable} class) or a list, table or array of mortalityTable objects
#' @param trend Trend vector to be applied to the mortality table
#' @param trendages Ages corresponding to the values of the \code{trend} vector
#' @param baseYear Base year for the trend projection (passed on to \code{mortalityTable.trendProjection})
#' @param dampingFunction Trend damping (passed on to \code{mortalityTable.trendProjection})
#'
#' @export
    mT.setTrend = function(table, trend, trendages = NULL, baseYear = NULL, dampingFunction = identity) {
    if (is.array(table)) {
        return(array(
            lapply(table, mT.setTrend, trend = trend, trendages = trendages, baseYear = baseYear, dampingFunction = dampingFunction),
            dim = dim(table), dimnames = dimnames(table))
        )
    } else if (is.list(table)) {
        return(lapply(table, mT.setTrend, trend = trend, trendages = trendages, baseYear = baseYear, dampingFunction = dampingFunction))
    } else if (is.na(c(table))) {
        return(table)
    }
    if (!is(table, "mortalityTable"))
        stop("First argument must be a mortalityTable or a list of mortalityTable objects.")

    t = mortalityTable.trendProjection(
        table,
        baseYear = if (is.null(baseYear)) table@baseYear else baseYear,
        trend = trend[match(table@ages, if (is.null(trendages)) ages(table) else trendages)],
        dampingFunction = dampingFunction
    )
    t
}
#' @describeIn mT.setTrend Add a trend to the mortality table (returns a mortalityTable.trendProjection obect)
#' @export
mT.addTrend = mT.setTrend



#' Extrapolate a mortality trend exponentially
#'
#' Extrapolate a mortality trend in a \code{mortalityTable} object using an exponential function (i.e. the trend decreases towards 0 exponentially).
#' This is mainly used to extrapolate an observed age-specific trend to very old ages.
#' Existing trend function values above (or below, respectively) the \code{idx} are overwritten.
#'
#' @param table A life table object (instance of a \code{mortalityTable} class) or a list, table or array of mortalityTable objects
#' @param idx Index (typically age) of the position of the fit
#' @param up Whether the fit is forward- or backward-facing (i.e. to old or young ages)
#'
#' @examples
#' mortalityTables.load("Austria_Annuities_AVOe2005R")
#' # extrapolate the trend exponentially from age 95 instead (overwriting the existing trend)
#' avoe2005exp = mT.extrapolateTrendExp(AVOe2005R.male, 95)
#' plotMortalityTrend(mT.setName(avoe2005exp, "AVÖ 2005R with trend extrapolated from age 85 up"),
#'                    AVOe2005R.male, Period = 2020, ages = 60:120)
#' @export
mT.extrapolateTrendExp = function(table, idx, up = TRUE) {
    if (is.array(table)) {
        return(array(
            lapply(table, mT.extrapolateTrendExp, idx = idx, up = up),
            dim = dim(table), dimnames = dimnames(table))
        )
    } else if (is.list(table)) {
        return(lapply(table, mT.extrapolateTrendExp, idx = idx, up = up))
    } else if (is.na(c(table))) {
        return(table)
    }
    if (!is(table, "mortalityTable"))
        stop("First argument must be a mortalityTable or a list of mortalityTable objects.")

    if (.hasSlot(table, "trend") && !is.null(table@trend) && length(table@trend) > 1)
        table@trend = fitExpExtrapolation(table@trend, idx = idx,up = up)
    if (.hasSlot(table, "trend2") && !is.null(table@trend2) && length(table@trend2) > 1)
        table@trend2 = fitExpExtrapolation(table@trend2, idx = idx,up = up)
    table
}


#' Translate base table of a cohort mortality table to a different observation year
#'
#' Translate the base table of a cohort life table to a different observation period,
#' using the existing base table and the trend functions. This only has an effect on
#' cohort life tables (e.g. objects of class \code{mortalityTable.trendProjection}).
#' For all other life tables (period life tables, observed, etc.), this function has no effect.
#'
#' This function also does not modify the resulting death probabilities of the life table
#' object, it just reparameterizes the internal representation of a life table
#' with trend projection factors.
#'
#' This functionality is often needed when publisheing life thables. Typically,
#' the table is derived from a certain observation period, so the resulting base
#' table describes the middle of the observation period. Projetion into the future
#' is then done using trend projection factors starting from that base table.
#' On the other hand, for the published table it is often desired to tabulate
#' not the middle of the observation period, but rather the current year as base
#' year for the extrapolation.
#' For the resulting period or cohort death probabilities, it is irrelevant, which
#' base year is used, as long as the shift to another base year (which includes
#' translating the base mortalities of the base year) is done consistenly with the
#' trend functions. The function \code{mT.translate} ensures this.
#'
#' @param table A life table object (instance of a \code{mortalityTable} class)
#'              or a list, table or array of mortalityTable objects
#' @param baseYear Target base year. The underlying period life table of the
#'                 cohort life table is translated to the desired target base
#'                 year by applying the trend factors of the table, resulting
#'                 in a consistent shift of the internal representation without
#'                 changing the resulting probabilities.
#' @param name (optional) new name for the mortality table
#'
#' @examples
#' mortalityTables.load("Austria_Annuities_AVOe2005R")
#' # The AVOe2005R.male.nodamping has 2001 as the base year. Move its base year
#' # to 2020 without modifying cohort probabilities
#' avoe05r.shifted = mT.translate(AVOe2005R.male.nodamping, 2020, "AVÖ 2005-R, translated to 2020")
#' plotMortalityTables(
#'     getPeriodTable(AVOe2005R.male.nodamping),
#'     getPeriodTable(avoe05r.shifted),
#'     title = "Base tables of the AVÖ 2005R a translated version to 2020")
#' # Even though the base tables are shifted, the resulting probabilities are
#' # unchanged (except for numeric artefacts)
#' abs(periodDeathProbabilities(AVOe2005R.male.nodamping, Period = 2050) -
#'     periodDeathProbabilities(avoe05r.shifted, Period = 2050)) < 0.00000001
#' abs(deathProbabilities(AVOe2005R.male.nodamping, YOB = 2050) -
#'     deathProbabilities(avoe05r.shifted, YOB = 2050)) < 0.00000001
#' @export
mT.translate = function(table, baseYear, name = NULL) {
    if (is.array(table)) {
        return(array(
            lapply(table, mT.translate, baseYear = baseYear, name = name),
            dim = dim(table), dimnames = dimnames(table))
        )
    } else if (is.list(table)) {
        return(lapply(table, mT.translate, baseYear = baseYear, name = name))
    } else if (is.na(c(table))) {
        return(table)
    }
    if (!is(table, "mortalityTable"))
        stop("First argument must be a mortalityTable or a list of mortalityTable objects.")

    table@deathProbs = periodDeathProbabilities(table, Period = baseYear)
    table@baseYear = baseYear
    if (!is.null(name)) {
        table@name = name
    }
    table
}

#' Extrapolate base table of a mortalityTable using an exponential function
#'
#' Extrapolate the base table of a \code{mortalityTable} object using an exponential
#' function (i.e. the death probabilities decreases towards 0 exponentially).
#' While death probabilities trending towards 0 for old ages is not realistic for
#' overall deaths, it can be useful to model causes of death that vanish in older age.
#' It is, however, most useful to extrapolate an observed base table to low ages
#' (e.g. for an insurance portfolio with practically no persons aged below 16). A
#' decline towards 0 for low ages makes sense in this case.
#'
#' The function needs only one age, from which the extrapolation using an exponential
#' function is applied. the strength of the exponential function is derived from the death probability at that age.
#'
#' @param table A life table object (instance of a \code{mortalityTable} class)
#'              or a list, table or array of mortalityTable objects
#' @param age Index (typically age) of the position of the fit
#' @param up Whether the fit is forward- or backward-facing (i.e. to old or young ages)
#'
#' @examples
#' mortalityTables.load("Austria_Census")
#' # use the Austrian population mortalities for ages 18-95 and exponentially
#' # extrapolate them to lower ages
#' AT11.lowAgesExp = mT.extrapolateProbsExp(mort.AT.census.2011.male, 18, up = FALSE)
#' plotMortalityTables(mT.setName(AT11.lowAgesExp, "Ages below 16 are extrapolated exponentially"),
#'                     mort.AT.census.2011.male)
#' @export
mT.extrapolateProbsExp = function(table, age, up = TRUE) {
    if (is.array(table)) {
        return(array(
            lapply(table, mT.extrapolateProbsExp, age = age, up = up),
            dim = dim(table), dimnames = dimnames(table))
        )
    } else if (is.list(table)) {
        return(lapply(table, mT.extrapolateProbsExp, age = age, up = up))
    } else if (is.na(c(table))) {
        return(table)
    }
    if (!is(table, "mortalityTable"))
        stop("First argument must be a mortalityTable or a list of mortalityTable objects.")

    if (.hasSlot(table, "deathProbs")) {
        idx = match(age, ages(table))
        fit = fitExpExtrapolation(table@deathProbs, idx = idx, up = up, verbose = TRUE)
        table@deathProbs = fit$data
        table@data$extrapolationData = c(
            table@data$extrapolationData,
            list(list(law = "Exp", idx = idx, up = up, fit = fit)))
    }
    table
}

#' Fit interpolation law to a mortality table and extrapolate
#'
#' Fit an extrapolation law (from the \code{MortalityLaws} Package to the base
#' table of the mortality table and use it for extrapolation.
#'
#' The fit is done using the \code{MortalityLaws::MortalityLaw} function, with the ages, death counts, exposures and death rates taken from the \code{table} mortality table object. The law and the fitting method can be given in the \code{mT.fitExtrapolationLaw} with
#' the law and the fitting method
#'
#' The age range \code{fit} is used to fit the law, while extrapolation is
#' applied only to ages given in parameter \code{extrapolate}. As fitting
#' does usually not result a smooth transition, a linear fade in or fade out
#' range can also be provided.
#'
#' @param table A life table object (instance of a \code{mortalityTable} class) or a list, table or array of mortalityTable objects
#' @param method The fitting method (passed on to [MortalityLaw])
#' @param law The mortality law fitted to the data(passed on to [MortalityLaw])
#' @param fit Age range to use for the fit
#' @param extrapolate Desired age range of the extrapolation (i.e. only those
#'        ages will be extrapolated and added to the base table)
#' @param fadeIn age range to linearly fade in from the existing base table's values to the extrapolated
#' @param fadeOut age range to linearly fade out from the extrapolated base table's values to the existing
#' @param raw (optional) raw data to use for fitting. If not given, the raw
#'            probabilities of the table (stored in \code{table@data$rawProbs})
#'            or the table's base table (\code{table@deathProbs}) is used by default.
#'
#' @examples
#' mortalityTables.load("Austria_Census")
#' # use Austrian population mortalities for ages 18-95 and exponentially
#' # extrapolate them to lower ages
#' AT11.lowAges = mT.fitExtrapolationLaw(mort.AT.census.2011.male, law = "opperman",
#'                                       fit = 5:15, extrapolate = 0:15,
#'                                       fadeIn = NULL, fadeOut = 5:15)
#' AT11.oldAges = mT.fitExtrapolationLaw(mort.AT.census.2011.male, law = "HP",
#'                                       fit = 75:90, extrapolate = 75:120)
#' plotMortalityTables(mT.setName(AT11.lowAges, "Low ages fitted (ages 5-15 used)"),
#'                     mT.setName(AT11.oldAges, "old ages fitted (ages 75-90 used)"),
#'                     mort.AT.census.2011.male)
#' @export
mT.fitExtrapolationLaw = function(table, method = "LF2", law = "HP",
                                  fit = 75:99, extrapolate = 80:120,
                                  fadeIn = 80:90, fadeOut = NULL, raw = NULL) {
    if (!is(table, "mortalityTable"))
        stop("First argument must be a mortalityTable.")
    ages = ages(table)
    # if (!is.null(table@exposures) && !is.na(table@exposures)) {
        # Ex = table@exposures
        # qx = table@deathProbs
        # if (!is.null(table@data$deaths)) {
        #     Dx = table@data$deaths
        # } else {
        #     Dx = table@deathProbs * Ex
        # }
    # } else {
        # Ex = rep(1, length(ages))
        # Dx = table@deathProbs
        # qx = table@deathProbs
    # }
    if (!is.null(raw)) {
        rawData = raw
    } else if (!is.null(table@data$rawProbs)) {
        rawData = table@data$rawProbs
    } else {
        rawData = table@deathProbs
    }
    table  = mT.fillAges(table, neededAges = union(ages, extrapolate), fill = 0)
    fitted = fitExtrapolationLaw(
        data = table@deathProbs, ages = ages(table),
        qx = rawData, data.ages = ages,
        method = method, law = law,
        fit = fit, extrapolate = extrapolate,
        fadeIn = fadeIn, fadeOut = fadeOut,
        verbose = TRUE
    )
    # Store all fit parameters in the data slot of the mortality table
    table@data$extrapolationData = c(
        table@data$extrapolationData,
        list(list(law = law, method = method, fit = fit,
                  extrapolate = extrapolate, fadeIn = fadeIn, fadeOut = fadeOut,
                  fit = fitted)))
    table@deathProbs = fitted$probs

    table
}

#' Set additional information (year, description, type of risk, sex, etc.) for the pension table.
#'
#' A mortalityTable can store additional information to be used e.g. as additional
#' dimensions in ggplot calls. Typically, these information include sex, base
#' population, observation year, type of data (raw, smoothed), country, type of
#' risk, etc. These additional dimensions are stored in the \code{tbl@data} list
#' and will be used by plotMortalityTables and similar functions.
#' \code{pT.setDimInfo} works just like \code{mT.setDimInfo}, except that it sets
#' the information for all sub-tables of the pension table at the same time.
#'
#' @param tbl The \code{pensionTable} object to assign dimensional information
#' @param ... The dimensional information as named arguments. All names except tbl and append are allowed.
#' @param append Whether to append to existing dimensional data (append=TRUE) or
#'               completely replace existing information (append=FALSE)
#'
#' @examples
#' # For examples, please see the \code{mT.setDimInfo} function.
#' @export
pT.setDimInfo = function(tbl, ..., append = TRUE) {
    if (is.array(tbl)) {
        return(array(
            lapply(tbl, pT.setDimInfo, ..., append = append),
            dim = dim(tbl), dimnames = dimnames(tbl))
        )
    } else if (is.list(tbl)) {
        return(lapply(tbl, pT.setDimInfo, ..., append = append))
    } else if (is.na(c(tbl))) {
        return(tbl)
    }
    if (!is(tbl, "pensionTable"))
        stop("First argument must be a pensionTable or a list of pensionTable objects.")

    if (append) {
        tbl@data[names(list(...))] = list(...)
    } else {
        tbl@data = list(...)
    }

    tbl@qx = mT.setDimInfo(tbl@qx, ..., append = append)
    tbl@ix = mT.setDimInfo(tbl@ix, ..., append = append)
    tbl@qix = mT.setDimInfo(tbl@qix, ..., append = append)
    tbl@rx = mT.setDimInfo(tbl@rx, ..., append = append)
    tbl@apx = mT.setDimInfo(tbl@apx, ..., append = append)
    tbl@qpx = mT.setDimInfo(tbl@qpx, ..., append = append)
    tbl@hx = mT.setDimInfo(tbl@hx, ..., append = append)
    tbl@qwy = mT.setDimInfo(tbl@qwy, ..., append = append)
    tbl@qgx = mT.setDimInfo(tbl@qgx, ..., append = append)
    tbl
}


#' Set additional information (year, description, type of risk, sex, etc.) for the mortality table.
#'
#' A mortalityTable can store additional information to be used e.g. as additional
#' dimensions in ggplot calls. Typically, these information include sex, base
#' population, observation year, type of data (raw, smoothed), country, type of
#' risk, etc. These additional dimensions are stored in the \code{tbl@data} list
#' and will be used by plotMortalityTables and similar functions.
#'
#' @param tbl The \code{mortalityTable} object to assign dimensional information
#' @param ... The dimensional information as named arguments. All names except tbl and append are allowed.
#' @param append Whether to append to existing dimensional data (append=TRUE) or
#'               completely replace existing information (append=FALSE)
#'
#' @examples
#' mortalityTables.load("Austria_Census")
#' mortalityTables.load("Austria_Annuities")
#' # The annuity tables use the population mortality as starting point. Set either
#' # population or anuuitants as dimensional info and use that dimension in a ggplot
#' # Most pre-defined tables already have the most important dimensions (like sex) stored.
#' at01.m = mT.setDimInfo(mort.AT.census.2001.male, population = "Population")
#' at01.f = mT.setDimInfo(mort.AT.census.2001.female, population = "Population")
#' av05r.m = mT.setDimInfo(AVOe2005R.male, population = "Annuitants")
#' plotMortalityTables(at01.m, at01.f, av05r.m) + aes(linetype = population, color = sex)
#' @export
mT.setDimInfo = function(tbl, ..., append = TRUE) {
    if (is.array(tbl)) {
        return(array(
            lapply(tbl, mT.setDimInfo, ..., append = append),
            dim = dim(tbl), dimnames = dimnames(tbl))
        )
    } else if (is.list(tbl)) {
        return(lapply(tbl, mT.setDimInfo, ..., append = append))
    } else if (is(tbl, "pensionTable")) {
        return(pT.setDimInfo(tbl, ..., append = append))
    } else if (is.na(c(tbl))) {
        return(tbl)
    }

    if (!is(tbl, "mortalityTable"))
        stop("First argument must be a mortalityTable or a list of mortalityTable objects.")

    if (append) {
        tbl@data$dim[names(list(...))] = list(...)
    } else {
        tbl@data$dim = list(...)
    }
    tbl
}


#' Extract a sub-table from a pensionTable
#'
#'  This function \code{pT.getSubTable} allows access to the individual components
#'  of a pension table. In contrast to a "normal" mortalityTable, which describes
#'  probablilities for only mortality or a single population, a pension table
#'  describes transition probabilities for other states, too:
#'  \itemize{
#'      \item active population (i.e. not disabled, not retired)
#'      \item disabled population (occupational disability)
#'      \item old-age pensioners
#'      \item widows/widowers
#'  }
#'
#'  The corresponding transition probabilities are:
#'  \describe{
#'      \item{qx}{mortality $q^a_x$ of actives (probability of death)}
#'      \item{ix}{morbidity $i_x$ of actives (probability occupational disability)}
#'      \item{qix}{mortality $q^i_x$ of disabled (probability of death)}
#'      \item{rx}{reactivation $r_x$ of invalids (probability of becoming active again)}
#'      \item{qpx}{mortality $q^p_x$ of old-age pensioners}
#'      \item{qgx}{mortality $q^g_x$ of the whole population (including actives and disabled)}
#'      \item{hx}{probability $h_x$ of leaving a widow/widower when dying at age $x$}
#'      \item{yx}{average age $y(x)$ of surviving widow/widower when dying at age $x$}
#'      \item{qwx}{mortality $q^w_x$ of widows}
#'  }
#'
#'  The function \code{pT.getSubTable} extracts a single transition probability
#'  from the pension table, using the keys given above. The returned object is
#'  also a \code{mortalityTable} object.
#'
#' @param table a \code{pensionTable} object
#' @param subtable the key describing the desired subtable (see above for the full list)
#'
#' @export
pT.getSubTable = function(table, subtable = "qx") {
    if (is.array(table)) {
        return(array(
            lapply(table, pT.getSubTable, subtable = subtable),
            dim = dim(table), dimnames = dimnames(table))
        )
    } else if (is.list(table)) {
        return(lapply(table, pT.getSubTable, subtable = subtable))
    } else if (is.na(c(table))) {
        return(table)
    }
    if (!is(table, "pensionTable"))
        stop("First argument must be a pensionTable or a list of pensionTable objects.")

    if (length(subtable) > 1) {
        return(lapply(subtable, function(st) pT.getSubTable(table, subtable = st)))
    } else {
        if (.hasSlot(table, subtable))
            slot(table, subtable)
        else
            NULL
    }
}

#' Switch over mortalities from one table to another at a given age
#'
#' This function modifies a \code{mortalityTable} by switching moralities at a given
#' age to the mortalities of a second table.
#'
#' This function \code{mT.switchover} modifies the given \code{mortalityTable}
#' and replaces the mortalities starting from a given age by the mortalities
#' of a second table. By default, the transition from the original table to the
#' secondary table is a simple 0/1-switch at the given age \code{at}. This is done
#' internally by using \code{weights= (age >= at)}.
#'
#' By giving custom weights, one can also implement a smooth transition to the
#' secondary table. The weights are used as simple factors of a linear combination
#' of the two tables.
#'
#' @param table The \code{mortalityTable} to modify (transition the probabilities to the secondary table)
#' @param to The secondary \code{mortalityTable} containing the target probabilities
#' @param at The age at which to switch over to the secondary table (if \code{weights} are given, the \code{at} argument is ignored).
#' @param weights (optional) transition weights for transitioning the probabilities from the primary to the secondary table (as a linear combination).
#'
#' @examples
#' mortalityTables.load("Austria_Census")
#' mort.AT.switchover = mT.switchover(mort.AT.census.2011.male, mort.AT.census.2011.female, 60)
#' plotMortalityTables(mort.AT.census.2011.male,
#'                     mT.setName(mort.AT.switchover, "Switched to female at age 60"))
#'
#' # A smooth switchover is possible with custom weights
#' mort.AT.switchover.smooth = mT.switchover(mort.AT.census.2011.male, mort.AT.census.2011.female,
#'     weights = c(rep(0, 55), 0:20/20, rep(1, 25)))
#' plotMortalityTables(mort.AT.census.2011.male,
#'     mT.setName(mort.AT.switchover.smooth, "Switched to female smoothly at ages 55-75"))
#'
#' @export
mT.switchover = function(table, to, at, weights = NULL) {
    if (is.array(table)) {
        return(array(
            lapply(table, mT.switchover, to = to, at = at, weights = weights),
            dim = dim(table), dimnames = dimnames(table))
        )
    } else if (is.list(table)) {
        return(lapply(table, mT.switchover, to = to, at = at, weights = weights))
    } else if (is.na(c(table))) {
        return(table)
    }
    if (!is(table, "mortalityTable"))
        stop("First argument must be a mortalityTable or a list of mortalityTable objects.")

    if (is.null(weights)) {
        ags.table = ages(table)
        ags.to = ages(to)
        weights = 1 * (ags.to >= at)
    }
    table@deathProbs = table@deathProbs * (1 - weights) + to@deathProbs * weights
    table
}



#' Round all components of a mortality table to the given number of digits
#'
#' The function mt.round rounds all components (base table, potentially also
#' trend functions or yearly improvement factors) to the given number of
#' numerical digits. For pensionTable objects, the function is applied to all components
#'
#' @param object The mortalityTable object to be rounded (or a list / array of mortalityTable object)
#' @param digits the desired number of significant digits to round to
#'
#' @examples
#' mortalityTables.load("Austria_Census")
#' AT.rounded1 = mT.round(mort.AT.census.2011.male, 1)
#' AT.rounded2 = mT.round(mort.AT.census.2011.male, 2)
#' AT.rounded3 = mT.round(mort.AT.census.2011.male, 3)
#' plotMortalityTables(mort.AT.census.2001.male,
#'                     mT.setName(AT.rounded1, "rounded to 1 digit"),
#'                     mT.setName(AT.rounded3, "rounded to 3 digits"))
#'
#' @exportMethod mT.round
setGeneric("mT.round", function(object, digits = 8) standardGeneric("mT.round"));

#' @describeIn mT.round Round the given mortalityTable to a given number of digits
setMethod("mT.round", "mortalityTable",
          function(object, digits = 8) {
              object
          })
#' @describeIn mT.round Round the given period mortality table to a given number of digits (base table and loadings)
setMethod("mT.round", "mortalityTable.period",
          function(object, digits = 8) {
              o = callNextMethod()
              o@deathProbs = round(o@deathProbs, digits = digits)
              o@loading    = round(o@loading, digits = digits)
              o
          })
#' @describeIn mT.round Round the given mortalityTable with trend projection to a given number of digits (base table, loadings and trend(s))
setMethod("mT.round", "mortalityTable.trendProjection",
          function(object, digits = 8) {
              o = callNextMethod()
              if (!is.null(o@trend) && !all(is.na(o@trend))) {
                  o@trend  = round(o@trend, digits = digits)
              }
              if (!is.null(o@trend2) && !all(is.na(o@trend2))) {
                  o@trend2 = round(o@trend2, digits = digits)
              }
              o
          })
#' @describeIn mT.round Round the given mortalityTable with improvement factors to a given number of digits (base table, loadings and improvement factors)
setMethod("mT.round", "mortalityTable.improvementFactors",
          function(object, digits = 8) {
              o = callNextMethod()
              o@improvement = round(o@improvement, digits = digits)
              if (!is.null(o@loading) && !all(is.na(o@loading))) {
                  o@loading    = round(o@loading, digits = digits)
              }
              o
          })
#' @describeIn mT.round Round the mortalityTables stored in an array to a given number of digits
setMethod("mT.round", "array",
          function(object, digits = 8) {
              array(
                  lapply(object, mT.round, digits = digits),
                  dim = dim(object), dimnames = dimnames(object))
          })
#' @describeIn mT.round Round the mortalityTables stored in a list to a given number of digits
setMethod("mT.round", "list",
          function(object, digits = 8) {
              lapply(object, mT.round, digits = digits)
          })

#' @describeIn mT.round Round all components of a pensionTable to a given number of digits
setMethod("mT.round", "pensionTable",
          function(object, digits = 8) {
              object@qx = mT.round(object@qx, digits = digits)
              object@ix = mT.round(object@ix, digits = digits)
              object@qix = mT.round(object@qix, digits = digits)
              object@rx = mT.round(object@rx, digits = digits)
              object@apx = mT.round(object@apx, digits = digits)
              object@qpx = mT.round(object@qpx, digits = digits)
              object@hx = mT.round(object@hx, digits = digits)
              object@qwy = mT.round(object@qwy, digits = digits)
              object@qgx = mT.round(object@qgx, digits = digits)
              object
          })

#' @describeIn mT.round Empty dummy function that simply returns NULL
setMethod("mT.round", "NULL",
          function(object, digits = 8) {
              NULL
          })



#' Remove all non-essential data (raw data, etc.) from a mortalityTable object
#'
#' The function mt.cleanup removes all non-essential data from a given mortalityTable
#' object.
#'
#' Mortality tables are often generated from raw data, that is smoothed, extrapolated,
#' etc. The mortalityTable class and its implementations can internally store the
#' raw probabilities and the intermediate results and parameters. This method
#' removes those information. All essential information (base table, ages,
#' trend functions, etc.) are preserved.
#'
#' Removed information includes:
#'   * all elements of the \code{object@data} list, except for \code{dim}
#'   * exposures
#'   * names of named age, deathProbs and trend vectors
#'
#' For mortality tables with other mortalityTable components (like pension tables
#' or mixed tables), all components are cleaned.
#'
#' @param object The mortalityTable object to be cleaned.
#'
#' @examples
#' mortalityTables.load("Austria_Census")
#' # Whittaker-Henderson smoothing stores the raw input and the weights in the
#' # \code{data} slot of the table:
#' AT.smoothed = whittaker.mortalityTable(mort.AT.census.2011.male)
#' AT.smoothed@data$rawProbs
#' AT.smoothed@data$whittaker
#'
#' # cleaning up the table removes those non-essential information again:
#' AT.smoothed.clean = mT.cleanup(AT.smoothed)
#' AT.smoothed.clean@data$rawProbs
#' AT.smoothed.clean@data$whittaker
#'
#' @exportMethod mT.cleanup
setGeneric("mT.cleanup", function(object) standardGeneric("mT.cleanup"));

#' @describeIn mT.cleanup Clean up and remove all non-essential data from the mortalityTable object
setMethod("mT.cleanup", "mortalityTable",
          function(object) {
              object@data = list(dim = object@data$dim)
              object
          })
#' @describeIn mT.cleanup Clean up and remove all non-essential data from the mortalityTable.period object
setMethod("mT.cleanup", "mortalityTable.period",
          function(object) {
              o = callNextMethod()
              o@ages = unname(o@ages)
              o@deathProbs = unname(o@deathProbs)
              o@exposures = NULL
              o
          })
#' @describeIn mT.cleanup Clean up and remove all non-essential data from the mortalityTable.trendProjection object
setMethod("mT.cleanup", "mortalityTable.trendProjection",
          function(object) {
              o = callNextMethod()
              o@trend = unname(o@trend)
              o@trend2 = unname(o@trend2)
              o
          })
#' @describeIn mT.cleanup Clean up and remove all non-essential data from the mortalityTable objects stored in the array
setMethod("mT.cleanup", "array",
          function(object) {
              array(
                  lapply(object, mT.cleanup),
                  dim = dim(object), dimnames = dimnames(object))
          })
#' @describeIn mT.cleanup Clean up and remove all non-essential data from the mortalityTable objects stored in the list
setMethod("mT.cleanup", "list",
          function(object) {
              lapply(object, mT.cleanup)
          })

#' @describeIn mT.cleanup Empty dummy function to handle unassigned variables
setMethod("mT.cleanup", "NULL",
          function(object) {
              NULL
          })

#' @describeIn mT.cleanup Clean up and remove all non-essential data from the mortalityTable objects stored in the array
setMethod("mT.cleanup", "pensionTable",
          function(object) {
              object = callNextMethod()
              object@qx = mT.cleanup(object@qx)
              object@ix = mT.cleanup(object@ix)
              object@qix = mT.cleanup(object@qix)
              object@rx = mT.cleanup(object@rx)
              object@apx = mT.cleanup(object@apx)
              object@qpx = mT.cleanup(object@qpx)
              object@hx = mT.cleanup(object@hx)
              object@qwy = mT.cleanup(object@qwy)
              object@qgx = mT.cleanup(object@qgx)
              object
          })


#' Calculate the total mortality of the pension table
#'
#' The function \code{pT.calculateTotalMortality} calculates the overall mortality from the mortality of actives and disabled
#'
#' Since a pension tables describes mortalities of actives and of disabled separately,
#' the overall mortality is a function of these two. The function \code{pT.calculateTortalMortality}
#' calculates this overall mortality in a way that is consistent with the
#' individual transition probabilities of the pension table.
#'
#' In particular, the pension table describes the mortalities of the individual
#' sub-populations of actives, disabled and old-age pensioners. The overall
#' mortality is the mortality that results when one discards the additional information
#' about the state and just observes deaths. Internally, the overall mortality
#' is calculated by starting from 10,000 actives and applying the transition dynamics
#' of the pension table to the sub-populations.
#'
#' For a detailled description, see e.g. the documentation of the Austrian pension
#' table AVÖ 2018-P or the German Heubeck Table DAV 2005-G.
#'
#' @references
#' R. Kainhofer, J. Hirz, A. Schubert. AVÖ 2018-P: Rechnungsgrundlagen für die Pensionsversicherung. Dokumentation der Pensionstafel. AVÖ-Arbeitskreis Rechnungsgrundlagen, 2008. \url{https://avoe.at/rechnungsgrundlagen/pensionskassen/}
#'
#' @param object a \code{pensionTable} object
#' @param ... (unused)
#'
#' @export
pT.calculateTotalMortality = function(object, ...) {
    probs = transitionProbabilities(object, Period = object@baseYear, as.data.frame = TRUE)
    probs$qgALT = probs$qg

    la = utils::head(Reduce('*', (1 - probs$q - probs$i), init = 100000, accumulate = TRUE), -1)
    lg = la
    for (idx in seq_along(lg)) {
        probs$qg[idx] = probs$qi[idx] - la[idx]/lg[idx] * (probs$qi[idx] - probs$q[idx] - probs$i[idx] * 1/2 * probs$qi[idx] / (1 - 1/2*probs$qi[idx]))
        lg[idx + 1] = lg[idx] * (1 - probs$qg[idx])
    }
    lg = utils::head(lg, -1)

    probs$qg
}

#' @describeIn pT.calculateTotalMortality Calculate the total mortality of a
#' pension table and assign it to the \code{qgx} slot of that table.
#'
#' @export
pT.recalculateTotalMortality = function(object, ...) {
    if (is.array(table)) {
        return(array(
            lapply(table, pT.recalculateTotalMortality, ...),
            dim = dim(table), dimnames = dimnames(table))
        )
    } else if (is.list(table)) {
        return(lapply(table, pT.recalculateTotalMortality, ...))
    } else if (is.na(c(table))) {
        return(table)
    }
    if (!is(table, "pensionTable"))
        stop("First argument must be a pensionTable or a list of pensionTable objects.")

    qg = pT.calculateTotalMortality(object)
    object@qgx@deathProbs = qg
    object
}

