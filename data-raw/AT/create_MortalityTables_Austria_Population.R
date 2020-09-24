###############################################################################
#  Skript to generate Austrian census mortality table objects
###############################################################################
library(MortalityTables)
library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(openxlsx)
library(here)
library(rlang)


###############################################################################
### Volkszählungen Österreich
###############################################################################
censusfile = here("data-raw", "AT", "Population", "ausfuehrliche_allgemeine_und_ausgeglichene_sterbetafeln_186871_bis_201012_.xlsx")
censusfile.out = here("data", "mort.AT.census.RData")

mort.AT.census = array(
  data = c(mortalityTable.NA),
  dim = c(3, 13),
  dimnames = list(
    Geschlecht = c("m", "w", "u"),
    Jahr = c(
      "1868/71",
      "1879/82",
      "1889/92",
      "1899/1902",
      "1909/12",
      "1930/33",
      "1949/51",
      "1959/61",
      "1970/72",
      "1980/82",
      "1990/92",
      "2000/02",
      "2010/12"))
)


censtable = function(file, table, sheet, baseYear = 1900, sex = "m", skip = 5, n_max = 102) {
  data = read_excel(file, sheet = sheet, skip = skip, n_max = n_max)
  name = paste0("ÖVSt ", table, " ", dplyr::recode(sex, "m" = "M", "w" = "F", "u" = "U"))
  tbl = mortalityTable.period(
    name = name,
    ages = data$x, deathProbs = data$`q(x)`,
    baseYear = baseYear,
    data = list(
      dim = list(
        table = paste("ÖVSt", table),
        sex = sex,
        collar = "Gesamtbevölkerung",
        type = "Volkssterbetafel",
        country = "Österreich",
        data = "official",
        year = baseYear
      )
    )
  )
  mort.AT.census[[sex, table]] = tbl
  tbl
}

#### Male Tables

mort.AT.census.1869.male = censtable(censusfile, table = "1868/71",   baseYear = 1869, sheet = "1868_71 männlich", sex = "m")
mort.AT.census.1880.male = censtable(censusfile, table = "1879/82",   baseYear = 1880, sheet = "1879_82 männlich", sex = "m")
mort.AT.census.1890.male = censtable(censusfile, table = "1889/92",   baseYear = 1890, sheet = "1889_92 männlich", sex = "m")
mort.AT.census.1900.male = censtable(censusfile, table = "1899/1902", baseYear = 1900, sheet = "1899_1902 männlich", sex = "m")
mort.AT.census.1910.male = censtable(censusfile, table = "1909/12",   baseYear = 1910, sheet = "1909_12 männlich", sex = "m")
mort.AT.census.1931.male = censtable(censusfile, table = "1930/33",   baseYear = 1931, sheet = "1930_33 männlich", sex = "m")
mort.AT.census.1951.male = censtable(censusfile, table = "1949/51",   baseYear = 1951, sheet = "1949_51 männlich", sex = "m")
mort.AT.census.1961.male = censtable(censusfile, table = "1959/61",   baseYear = 1961, sheet = "1959_61 männlich", sex = "m")
mort.AT.census.1971.male = censtable(censusfile, table = "1970/72",   baseYear = 1971, sheet = "1970_72 männlich", sex = "m")
mort.AT.census.1981.male = censtable(censusfile, table = "1980/82",   baseYear = 1981, sheet = "1980_82 männlich", sex = "m")
mort.AT.census.1991.male = censtable(censusfile, table = "1990/92",   baseYear = 1991, sheet = "1990_92 männlich", sex = "m")
mort.AT.census.2001.male = censtable(censusfile, table = "2000/02",   baseYear = 2001, sheet = "2000_2002 männlich", sex = "m")
mort.AT.census.2011.male = censtable(censusfile, table = "2010/12",   baseYear = 2011, sheet = "2010_2012 männlich", sex = "m")

#### Female Tables

mort.AT.census.1869.female = censtable(censusfile, table = "1868/71",   baseYear = 1869, sheet = "1868_71 weiblich", sex = "w")
mort.AT.census.1880.female = censtable(censusfile, table = "1879/82",   baseYear = 1880, sheet = "1879_82 weiblich", sex = "w")
mort.AT.census.1890.female = censtable(censusfile, table = "1889/92",   baseYear = 1890, sheet = "1889_92 weiblich", sex = "w")
mort.AT.census.1900.female = censtable(censusfile, table = "1899/1902", baseYear = 1900, sheet = "1899_1902 weiblich", sex = "w")
mort.AT.census.1910.female = censtable(censusfile, table = "1909/12",   baseYear = 1910, sheet = "1909_12 weiblich", sex = "w")
mort.AT.census.1931.female = censtable(censusfile, table = "1930/33",   baseYear = 1931, sheet = "1930_33 weiblich", sex = "w")
mort.AT.census.1951.female = censtable(censusfile, table = "1949/51",   baseYear = 1951, sheet = "1949_51 weiblich", sex = "w")
mort.AT.census.1961.female = censtable(censusfile, table = "1959/61",   baseYear = 1961, sheet = "1959_61 weiblich", sex = "w")
mort.AT.census.1971.female = censtable(censusfile, table = "1970/72",   baseYear = 1971, sheet = "1970_72 weiblich", sex = "w")
mort.AT.census.1981.female = censtable(censusfile, table = "1980/82",   baseYear = 1981, sheet = "1980_82 weiblich", sex = "w")
mort.AT.census.1991.female = censtable(censusfile, table = "1990/92",   baseYear = 1991, sheet = "1990_92 weiblich", sex = "w")
mort.AT.census.2001.female = censtable(censusfile, table = "2000/02",   baseYear = 2001, sheet = "2000_2002 weiblich", sex = "w")
mort.AT.census.2011.female = censtable(censusfile, table = "2010/12",   baseYear = 2011, sheet = "2010_2012 weiblich", sex = "w")

#### Unisex Tables

mort.AT.census.2011.unisex = censtable(censusfile, table = "2010/12", baseYear = 2011, sheet = "2010_2012 zusammen", sex = "u")

#### Data arrays rather than tables

mort.AT.census.ALL.male = MortalityTables::makeQxDataFrame(mort.AT.census["m", ]);
mort.AT.census.ALL.female = MortalityTables::makeQxDataFrame(mort.AT.census["w", ]);


save(
  mort.AT.census,

  mort.AT.census.1869.male,
  mort.AT.census.1880.male,
  mort.AT.census.1890.male,
  mort.AT.census.1900.male,
  mort.AT.census.1910.male,
  mort.AT.census.1931.male,
  mort.AT.census.1951.male,
  mort.AT.census.1961.male,
  mort.AT.census.1971.male,
  mort.AT.census.1981.male,
  mort.AT.census.1991.male,
  mort.AT.census.2001.male,
  mort.AT.census.2011.male,

  mort.AT.census.1869.female,
  mort.AT.census.1880.female,
  mort.AT.census.1890.female,
  mort.AT.census.1900.female,
  mort.AT.census.1910.female,
  mort.AT.census.1931.female,
  mort.AT.census.1951.female,
  mort.AT.census.1961.female,
  mort.AT.census.1971.female,
  mort.AT.census.1981.female,
  mort.AT.census.1991.female,
  mort.AT.census.2001.female,
  mort.AT.census.2011.female,

  mort.AT.census.2011.unisex,

  mort.AT.census.ALL.male,
  mort.AT.census.ALL.female,

  file = censusfile.out
)




###############################################################################
### jährlich fortgeschriebene Sterbetafeln
###############################################################################

library(reshape2)
library(openxlsx)

abridgedfile = here("data-raw", "AT", "Population", "jaehrliche_sterbetafeln_1947_bis_2019__fuer_oesterreich.xlsx")
abridgedfile.out = here("data", "mort.AT.observed.RData")

wb = openxlsx::loadWorkbook(abridgedfile)

loadSheet = function(wb, sheet = "2017") {
  if (as.numeric(sheet) >= 2002) {
    startRow = 8
    cols = c(1,2,8,14)
    colNames = c("age", "m", "w", "u")
  } else {
    startRow = 13
    cols = c(1,2,8)
    colNames = c("age", "m", "w")
  }

  data = openxlsx::readWorkbook(wb, sheet = sheet, startRow = startRow, colNames = FALSE, rowNames = FALSE, cols = cols) %>%
    `colnames<-`(colNames) %>%
    dplyr::filter(!is.na(m), !is.na(w)) %>%
    dplyr::mutate(age = as.integer(age), year = as.integer(sheet)) %>%
    tidyr::gather(key = sex, value = qx, -age, -year) %>%
    dplyr::select(year, sex, age, qx) %>%
    as_tibble

  data
}

AT.pop.obs = dplyr::bind_rows(sapply(sheets(wb), loadSheet, wb = wb, simplify = FALSE))

obstable = function(data, sex = "m") {
  deathProbs = data %>%
    filter(sex == !!sex) %>%
    reshape2::acast(age ~ year, value.var = "qx") %>%
    as.data.frame

  name = paste0("Österreich ", dplyr::recode(sex, "m" = "Männer", "w" = "Frauen", "u" = "Unisex"), " Beobachtung")
  mortalityTable.observed(
    name = name,
    deathProbs = deathProbs,
    ages = as.integer(rownames(deathProbs)),
    years = as.integer(colnames(deathProbs)),
    data = list(
      dim = list(
        table = "jährlich fortgeschriebene Sterbetafel",
        sex = sex,
        collar = "Gesamtbevölkerung",
        type = "Beobachtung",
        country = "Österreich",
        data = "official",
        year = "1947-2017")
    )
  )
}


mort.AT.observed = array(
  data = c(mortalityTable.NA),
  dim = c(3),
  dimnames = list(Geschlecht = c("m", "w", "u"))
)
mort.AT.observed[["m"]] = obstable(AT.pop.obs, sex = "m")
mort.AT.observed[["w"]] = obstable(AT.pop.obs, sex = "w")
mort.AT.observed[["u"]] = obstable(AT.pop.obs, sex = "u")
mort.AT.observed.male = mort.AT.observed[["m"]]
mort.AT.observed.female = mort.AT.observed[["w"]]
mort.AT.observed.unisex = mort.AT.observed[["u"]]


save(
  mort.AT.observed,
  mort.AT.observed.male,
  mort.AT.observed.female,
  mort.AT.observed.unisex,
  file = abridgedfile.out
)






###############################################################################
### Bevölkerungsprognose bis 2080 (mittleres Szenario)
### Datenquelle: Statistik Austria
###############################################################################

library(openxlsx)

forecastfile = here::here("data-raw", "AT", "Population", "StatistikAustria_qx_Prognose_mittleresSzenario_2014-2080.xlsx")
forecastfile.out = here::here("data", "mort.AT.forecast.RData")

AT.pop.fc.M = openxlsx::read.xlsx(forecastfile, startRow = 1, rows = c(1,3:103), rowNames = TRUE)
AT.pop.fc.F = openxlsx::read.xlsx(forecastfile, startRow = 1, rows = c(1,105:206), rowNames = TRUE)

mort.AT.forecast.male = mortalityTable.observed(
  name = "Österreich Männer (mittl. Sz.)",
  baseYear = 2014,
  deathProbs = AT.pop.fc.M,
  ages = as.numeric(rownames(AT.pop.fc.M)),
  years = as.numeric(colnames(AT.pop.fc.M)),
  data = list(
    dim = list(
      table = "Bevölkerungsprognose Österreich (mittl. Szenario)",
      sex = "m",
      collar = "Gesamtbevölkerung",
      type = "Bevölkerungsprognose",
      country = "Österreich",
      data = "official",
      year = "2014-2080")
  )
)
mort.AT.forecast.female = mortalityTable.observed(
  name = "Österreich Frauen (mittl. Sz.)",
  baseYear = 2014,
  deathProbs = AT.pop.fc.F,
  ages = as.numeric(rownames(AT.pop.fc.F)),
  years = as.numeric(colnames(AT.pop.fc.F)),
  data = list(
    dim = list(
      table = "Bevölkerungsprognose Österreich (mittl. Szenario)",
      sex = "w",
      collar = "Gesamtbevölkerung",
      type = "Bevölkerungsprognose",
      country = "Österreich",
      data = "official",
      year = "2014-2080")
  )
)
mort.AT.forecast = array(
  data = c(mortalityTable.NA),
  dim = c(2),
  dimnames = list(Geschlecht = c("m", "w"))
)
mort.AT.forecast[["m"]] = mort.AT.forecast.male
mort.AT.forecast[["w"]] = mort.AT.forecast.female


###############################################################################
# Forecast using a trend derived from the Statistik Austria data

lambda.forecast = function(qx) {
  logq = log(qx)
  rowMeans(logq[,-ncol(logq)] - logq[,-1])
}

mort.AT.forecast.male.trend = mortalityTable.trendProjection(
  name = "Österreich Männer (mittl. Sz.)",
  baseYear = 2014,
  deathProbs = AT.pop.fc.M[,1],
  trend = lambda.forecast(AT.pop.fc.M),
  ages = as.numeric(rownames(AT.pop.fc.M)),
  data = list(
    dim = list(
      table = "Bevölkerungsprognose Österreich (mittl. Szenario)",
      sex = "m",
      collar = "Gesamtbevölkerung",
      type = "Bevölkerungsprognose",
      country = "Österreich",
      data = "official",
      year = "2014-2080")
  )
)
mort.AT.forecast.female.trend = mortalityTable.trendProjection(
  name = "Österreich Frauen (mittl. Sz.)",
  baseYear = 2014,
  deathProbs = AT.pop.fc.F[,1],
  trend = lambda.forecast(AT.pop.fc.F),
  ages = as.numeric(rownames(AT.pop.fc.F)),
  data = list(
    dim = list(
      table = "Bevölkerungsprognose Österreich (mittl. Szenario)",
      sex = "w",
      collar = "Gesamtbevölkerung",
      type = "Bevölkerungsprognose",
      country = "Österreich",
      data = "official",
      year = "2014-2080")
  )
)


mort.AT.forecast.trend = array(
  data = c(mortalityTable.NA),
  dim = c(2),
  dimnames = list(Geschlecht = c("m", "w"))
)
mort.AT.forecast.trend[["m"]] = mort.AT.forecast.male.trend
mort.AT.forecast.trend[["w"]] = mort.AT.forecast.female.trend


###############################################################################
# Save to data

save(
  mort.AT.forecast,
  mort.AT.forecast.male,
  mort.AT.forecast.female,
  mort.AT.forecast.trend,
  mort.AT.forecast.male.trend,
  mort.AT.forecast.female.trend,

  file = forecastfile.out
)




###############################################################################
### MCMC fit (derived during creation of the AVÖ 2018-P by Jonas Hirz)
### Datenquelle: Statistik Austria, Method: Jonas Hirz
###############################################################################

library(pracma)
MCMCfile = here::here("data-raw", "AT", "Population", "Austria_Population_MCMC2018.csv")
MCMCfile.out = here::here("data", "mort.AT.MCMC.RData")

mort.AT.MCMC.load = function() {
  data = utils::read.csv(MCMCfile, skip = 5, encoding = "UTF-8", check.names = FALSE);
  data.array = data %>% as_tibble %>%
    gather(Variable, Value, -Parameter, -Alter) %>%
    separate(Variable, into = c("Geschlecht", "Jahr"), sep = ", ") %>%
    filter(Jahr == "1980") %>%
    mutate(Jahr = NULL) %>%
    acast(Alter ~ Geschlecht ~ Parameter, value.var = "Value")

  exp(data.array[,,"alpha"])/2
  mort.AT.MCMC = array(
    data = c(mortalityTable.NA),
    dim = c(3),
    dimnames = list(Geschlecht = c("m", "w", "u"))
  )

  MCMC.trend.damping = function(t) { 200 * atan(t / 200) }

  # Parameter für Whittaker-Smoothing:
  d = 2
  lambda = 10
  # TODO: Eta einbauen
  mort.AT.MCMC[["m"]] =  mortalityTable.trendProjection(
    name = "Österreich MCMC Männer",
    ages = as.integer(dimnames(data.array)[[1]]),
    baseYear = 2008,
    deathProbs = exp(whittaker(data.array[,"Mann","alpha"], lambda = lambda, d = d))/2,
    trend = whittaker(-data.array[,"Mann","beta"], lambda = lambda, d = d),
    dampingFunction = MCMC.trend.damping,
    data = list(
      dim = list(
        sex = "m",
        collar = "Gesamtbevölkerung",
        type = "MCMC-Fit 1980-2017",
        country = "Österreich",
        data = "MCMC",
        year = "1980-2017",
        table = "MCMC-Zerlegung Bevölkerungssterblichkeit")
    )
  ) %>%
    mT.fitExtrapolationLaw(law = "HP2", method = "LF2", fit = 80:98, extrapolate = 90:120, fadeIn = 90:99) %>%
    mT.extrapolateTrendExp(idx = 92, up = TRUE)

  mort.AT.MCMC[["w"]] =  mortalityTable.trendProjection(
    name = "Österreich MCMC Frauen",
    ages = as.integer(dimnames(data.array)[[1]]),
    baseYear = 2008,
    deathProbs = exp(whittaker(data.array[,"Frau","alpha"], lambda = lambda, d = d))/2,
    trend = whittaker(-data.array[,"Frau","beta"], lambda = lambda, d = d),
    dampingFunction = MCMC.trend.damping,
    data = list(
      dim = list(
        sex = "w",
        collar = "Gesamtbevölkerung",
        type = "MCMC-Fit 1980-2017",
        country = "Österreich",
        data = "MCMC",
        year = "1980-2017",
        table = "MCMC-Zerlegung Bevölkerungssterblichkeit")
    )
  ) %>%
    mT.fitExtrapolationLaw(law = "HP2", method = "LF2", fit = 80:98, extrapolate = 90:120, fadeIn = 90:99) %>%
    mT.extrapolateTrendExp(idx = 94, up = TRUE)

  mort.AT.MCMC[["u"]] =  mortalityTable.trendProjection(
    name = "Österreich MCMC Unisex",
    ages = as.integer(dimnames(data.array)[[1]]),
    baseYear = 2008,
    deathProbs = exp(whittaker(data.array[,"Unisex","alpha"], lambda = lambda, d = d))/2,
    trend = whittaker(-data.array[,"Unisex","beta"], lambda = lambda, d = d),
    dampingFunction = MCMC.trend.damping,
    data = list(
      dim = list(
        sex = "u",
        collar = "Gesamtbevölkerung",
        type = "MCMC-Fit 1980-2017",
        country = "Österreich",
        data = "MCMC",
        year = "1980-2017",
        table = "MCMC-Zerlegung Bevölkerungssterblichkeit")
    )
  ) %>%
    mT.fitExtrapolationLaw(law = "HP2", method = "LF2",fit = 80:98, extrapolate = 90:120, fadeIn = 90:99) %>%
    mT.extrapolateTrendExp(idx = 98, up = TRUE)

  mort.AT.MCMC
}



mort.AT.MCMC = mort.AT.MCMC.load()

save(
  mort.AT.MCMC,
  file = MCMCfile.out
)





