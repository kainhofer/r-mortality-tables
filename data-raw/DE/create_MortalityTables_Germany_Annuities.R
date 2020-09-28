#############################################################################m#
#  Skript to generate German annuity mortality table objects               ####
#############################################################################m#
library(MortalityTables)
library(here)
library(readxl)




#############################################################################h#
### Sterbetafel 1987R (published by Lühr, 1987)                            ####
#############################################################################h#

Sterbetafel1987R.file = here::here("data-raw", "DE", "Annuities", "Table 1987R.xlsx")
Sterbetafel1987R.file.out = here::here("data", "Sterbetafel1987R.RData")

Sterbetafel1987R.data = readxl::read_excel(Sterbetafel1987R.file, skip = 3)

Sterbetafel1987R.male = mortalityTable.ageShift(
  name = "Sterbeetafel 1987R Männer",
  ages = Sterbetafel1987R.data$Age,
  deathProbs = Sterbetafel1987R.data$Male,
  ageShifts = generateAgeShift(1, YOBs = c(NA, 1909, 1926, 1942, 1959, 1975, 1992, NA)),
  baseYear = 1950,
  data = list(
    dim = list(
      table = "Sterbetafel 1987R",
      sex = "m",
      collar = "Rententafel",
      type = "Rententafel",
      country = "Deutschland",
      data = "age-shifted",
      year = 1950)
  )
);
Sterbetafel1987R.female = mortalityTable.ageShift(
  name = "Sterbeetafel 1987R Frauen",
  ages = Sterbetafel1987R.data$Age,
  deathProbs = Sterbetafel1987R.data$Female,
  ageShifts = generateAgeShift(1, YOBs = c(NA, 1912, 1935, 1945, 1956, 1966, 1976, 1986, 1996, NA)),
  baseYear = 1950,
  data = list(
    dim = list(
      table = "Sterbetafel 1987R",
      sex = "w",
      collar = "Rententafel",
      type = "Rententafel",
      country = "Deutschland",
      data = "age-shifted",
      year = 1950)
  )
);
Sterbetafel1987R = array(
  data = c(mortalityTable.NA),
  dim = c(2),
  dimnames = list(Geschlecht = c("m", "w"))
)

Sterbetafel1987R[["m"]] = Sterbetafel1987R.male
Sterbetafel1987R[["w"]] = Sterbetafel1987R.female

save(Sterbetafel1987R, Sterbetafel1987R.male, Sterbetafel1987R.female, file = Sterbetafel1987R.file.out)






#############################################################################h#
# DAV 1994R exact and age-shifted (Male, Female), 1st-order only           ####
#############################################################################h#

dav1994Rfile = here::here("data-raw", "DE", "Annuities", "DAV_1994R.xlsx")
dav1994Rfile.out = here::here("data", "DAV1994R.RData")
dav1994Rav.file.out = here::here("data", "DAV1994R.av.RData")


dav1994r.data = openxlsx::read.xlsx(dav1994Rfile, sheet = "DAV 1994R", startRow = 3, cols = 1:5)
dav1994r.data.av = openxlsx::read.xlsx(dav1994Rfile, sheet = "DAV 1994R Altersverschiebung", startRow = 3, cols = 1:3)
dav1994r.data.av.shiftM = openxlsx::read.xlsx(dav1994Rfile, sheet = "DAV 1994R Altersverschiebung", startRow = 4, cols = 5:7) %>% `colnames<-`(c("from", "to", "shift"))
dav1994r.data.av.shiftF = openxlsx::read.xlsx(dav1994Rfile, sheet = "DAV 1994R Altersverschiebung", startRow = 4, cols = 9:11) %>% `colnames<-`(c("from", "to", "shift"))

DAV1994R.male = mortalityTable.trendProjection(
  name = "DAV 1994R Männer",
  ages = dav1994r.data$x,
  deathProbs = dav1994r.data$Basis.M,
  trend = dav1994r.data$Trend.M,
  baseYear = 2000,
  data = list(
    dim = list(table = "DAV 1994R", sex = "m", collar = "Rententafel", type = "Rententafel", country = "Deutschland", data = "official", year = 2000)
  )
)

DAV1994R.female = mortalityTable.trendProjection(
  name = "DAV 1994R Frauen",
  ages = dav1994r.data$x,
  deathProbs = dav1994r.data$Basis.F,
  trend = dav1994r.data$Trend.F,
  baseYear = 2000,
  data = list(
    dim = list(table = "DAV 1994R", sex = "w", collar = "Rententafel", type = "Rententafel", country = "Deutschland", data = "official", year = 2000)
  )
)

DAV1994R.male.av = mortalityTable.ageShift(
  name = "DAV 1994R Männer mit Altersverschiebung",
  ages = dav1994r.data.av$x,
  deathProbs = dav1994r.data.av$Basis.M,
  ageShifts = dav1994r.data.av.shiftM,
  data = list(
    dim = list(table = "DAV 1994R", sex = "m", collar = "Rententafel", type = "Rententafel", country = "Deutschland", data = "Altersverschiebung", year = 1955)
  )
)
DAV1994R.female.av = mortalityTable.ageShift(
  name = "DAV 1994R Frauen mit Altersverschiebung",
  ages = dav1994r.data.av$x,
  deathProbs = dav1994r.data.av$Basis.F,
  ageShifts = dav1994r.data.av.shiftF,
  data = list(
    dim = list(table = "DAV 1994R", sex = "w", collar = "Rententafel", type = "Rententafel", country = "Deutschland", data = "Altersverschiebung", year = 1955)
  )
)


DAV1994R = array(
  data = c(mortalityTable.NA),
  dim = c(2),
  dimnames = list(Geschlecht = c("m", "w"))
)

DAV1994R.av = array(
  data = c(mortalityTable.NA),
  dim = c(2),
  dimnames = list(Geschlecht = c("m", "w"))
)

DAV1994R[["m"]] = DAV1994R.male
DAV1994R[["w"]] = DAV1994R.female
DAV1994R.av[["m"]] = DAV1994R.male.av
DAV1994R.av[["w"]] = DAV1994R.female.av

save(
  DAV1994R, DAV1994R.male, DAV1994R.female,
  file = dav1994Rfile.out
)

save(
  DAV1994R.av, DAV1994R.male.av, DAV1994R.female.av,
  file = dav1994Rav.file.out
)






#############################################################################h#
# DAV 2004R exact (Male, Female, unisex)                                   ####
# gender-specific tables also have 2nd-order tables, unisex only 1st-order table
#############################################################################h#

DAV2004Rfile = here::here("data-raw", "DE", "Annuities", "DAV_2004_R.xls")
DAV2004Rfile.out = here::here("data", "DAV2004R.RData")


### Yearly weights of the trend => We need the cumulated weights of trend 1/2!
# DAV2004R.trend.switching = function(T1 = 5, T2 = 10) {
#   function(year) {
#     if (year <= 1999 + T1) {
#       1
#     } else if (year <= 1999 + T2) {
#       1 - (year - 1999 - T1) / (T2 - T1)
#     } else
#       0
#   }
# }
DAV2004R.trend.switching = function(T1 = 5, T2 = 10) {
  function(year) {
    if (year <= 1999 + T1) {
      1
    } else if (year <= 1999 + T2) {
      1 - (year - 1999 - T1) * (year - 1999 - T1 - 1) / ( (T2 - T1) * 2 * (year - 1999))
    } else
      (T1 + T2 + 1) / (2 * (year - 1999))
  }
}


DAV2004r.data = full_join(
  readxl::read_excel(
    DAV2004Rfile, sheet = "Basistafeln", skip = 4,
    # col_types = c(rep("guess", 7), rep("skip", 13)),
    col_names = c(
      "x",
      "qx Sel 2.Ord", "qy Sel 2.Ord", "qx Agg 2.Ord", "qy Agg 2.Ord",
      "qx Sel Bestand", "qy Sel Bestand", "qx Agg Bestand", "qy Agg Bestand",
      "qx Sel 1.Ord", "qy Sel 1.Ord", "qx Agg 1.Ord", "qy Agg 1.Ord"
    )
  ),
  readxl::read_excel(
    DAV2004Rfile, sheet = "Trends", skip = 4,
    col_names = c(
      "x",
      "trendM Start 2.Ord", "trendF Start 2.Ord", "trendM Ziel 2.Ord", "trendF Ziel 2.Ord",
      "trendM Start Bestand", "trendF Start Bestand", "trendM Ziel Bestand", "trendF Ziel Bestand",
      "trendM 1.Ord", "trendF 1.Ord"
    )
  ),
  by = "x"
)

DAV2004r.data.Selection = readxl::read_excel(DAV2004Rfile, sheet = "Selektionsfaktoren", skip = 1, col_types = c("skip", "guess", "guess"), col_names = c("year", "SelectMale", "SelectFemale"))
DAV2004r.data.Selection[2:5,] = DAV2004r.data.Selection[2,]



############################################################################## #

DAV2004R = array(
  data = c(mortalityTable.NA),
  # data = c(NA),
  dim = c(2, 4, 2),
  dimnames = list(Geschlecht = c("m", "w"), Tafel = c("1. Ordnung", "Bestand", "B20", "2. Ordnung"), Typ = c("Aggregat", "Selektion"))
)
DAV2004R[,,] = NA

# expand.grid(list(Geschlecht = c("m", "w"), Tafel = c("1. Ordnung", "Bestand", "B20", "2. Ordnung"), Typ = c("Aggregat", "Selektion")), )
# "m", "Bestand",    "Aggregat"
# "w", "Bestand",    "Aggregat"
# "m", "Bestand",    "Selektion"
# "w", "Bestand",    "Selektion"
# "m", "B20",        "Aggregat"
# "w", "B20",        "Aggregat"
# "m", "B20",        "Selektion"
# "w", "B20",        "Selektion"



#---------------------------------------------------------------------#
# Tafeln 1. Ordnung (keine Trendabschwächung)                     ####
#---------------------------------------------------------------------#

DAV2004R.male = mortalityTable.trendProjection(
  name = "DAV 2004R Männer, Aggregat",
  ages = DAV2004r.data$x,
  baseYear = 1999,
  deathProbs = DAV2004r.data$`qx Agg 1.Ord`,
  trend = DAV2004r.data$`trendM 1.Ord`,
  data = list(
    dim = list(table = "DAV 2004 R", sex = "m", collar = "Aggregat", type = "Rententafel", country = "Deutschland", data = "loaded", year = 1999)
  )
)

DAV2004R.female = mortalityTable.trendProjection(
  name = "DAV 2004R Frauen, Aggregat",
  ages = DAV2004r.data$x,
  baseYear = 1999,
  deathProbs = DAV2004r.data$`qy Agg 1.Ord`,
  trend = DAV2004r.data$`trendF 1.Ord`,
  data = list(
    dim = list(table = "DAV 2004 R", sex = "w", collar = "Aggregat", type = "Rententafel", country = "Deutschland", data = "loaded", year = 1999)
  )
)

DAV2004R.male.selekt = mortalityTable.trendProjection(
  name = "DAV 2004R Männer, Selekt",
  ages = DAV2004r.data$x,
  baseYear = 1999,
  deathProbs = DAV2004r.data$`qx Sel 1.Ord`,
  trend = DAV2004r.data$`trendM 1.Ord`,
  # selection = DAV2004r.data.Selection$SelectMale,
  # TODO: Add deathProbs before begin of selection effects
  data = list(
    dim = list(table = "DAV 2004 R", sex = "m", collar = "Selekt", type = "Rententafel", country = "Deutschland", data = "loaded", year = 1999)
  )
)

DAV2004R.female.selekt = mortalityTable.trendProjection(
  name = "DAV 2004R Frauen, Selekt",
  ages = DAV2004r.data$x,
  baseYear = 1999,
  deathProbs = DAV2004r.data$`qy Sel 1.Ord`,
  trend = DAV2004r.data$`trendF 1.Ord`,
  # selection = DAV2004r.data.Selection$SelectFemale,
  # TODO: Add deathProbs before begin of selection effects
  data = list(
    dim = list(table = "DAV 2004 R", sex = "w", collar = "Selekt", type = "Rententafel", country = "Deutschland", data = "loaded", year = 1999)
  )
)
DAV2004R[["m", "1. Ordnung", "Aggregat"]] = DAV2004R.male
DAV2004R[["w", "1. Ordnung", "Aggregat"]] = DAV2004R.female
DAV2004R[["m", "1. Ordnung", "Selektion"]] = DAV2004R.male.selekt
DAV2004R[["w", "1. Ordnung", "Selektion"]] = DAV2004R.female.selekt



#---------------------------------------------------------------------#
# Tafeln 2. Ordnung (mit Trendabschwächung)                        ####
#---------------------------------------------------------------------#

DAV2004R.male.2Ord = mortalityTable.trendProjection(
  name = "DAV 2004R Männer 2.Ordnung, Aggregat",
  ages = DAV2004r.data$x,
  baseYear = 1999,
  deathProbs = DAV2004r.data$`qx Agg 2.Ord`,
  trend = DAV2004r.data$`trendM Ziel 2.Ord`,
  trend2 = DAV2004r.data$`trendM Start 2.Ord`,
  dampingFunction = DAV2004R.trend.switching(T1 = 5, T2 = 10),
  data = list(
    dim = list(table = "DAV 2004 R 2. Ordnung", sex = "m", collar = "Aggregat", type = "Rententafel", country = "Deutschland", data = "unloaded", year = 1999)
  )
)
# deathProbabilities(DAV2004R.male.2Ord, YOB = 1939, ages = 60:121)
# Vectorize(DAV2004R.trend.switching(T1 = 5, T2 = 10))(1999:2020)


DAV2004R.female.2Ord = mortalityTable.trendProjection(
  name = "DAV 2004R Frauen 2.Ordnung, Aggregat",
  ages = DAV2004r.data$x,
  baseYear = 1999,
  deathProbs = DAV2004r.data$`qy Agg 2.Ord`,
  trend = DAV2004r.data$`trendF Ziel 2.Ord`,
  trend2 = DAV2004r.data$`trendF Start 2.Ord`,
  dampingFunction = DAV2004R.trend.switching(T1 = 5, T2 = 10),
  data = list(
    dim = list(table = "DAV 2004 R 2. Ordnung", sex = "w", collar = "Aggregat", type = "Rententafel", country = "Deutschland", data = "unloaded", year = 1999)
  )
)

DAV2004R.male.2Ord.selekt = mortalityTable.trendProjection(
  name = "DAV 2004R Männer 2.Ordnung, Selekt",
  ages = DAV2004r.data$x,
  baseYear = 1999,
  deathProbs = DAV2004r.data$`qx Sel 2.Ord`,
  trend = DAV2004r.data$`trendM Ziel 2.Ord`,
  trend2 = DAV2004r.data$`trendM Start 2.Ord`,
  dampingFunction = DAV2004R.trend.switching(T1 = 5, T2 = 10),
  # selection = DAV2004r.data.Selection$SelectMale,
  # TODO: Add deathProbs before begin of selection effects
  data = list(
    dim = list(table = "DAV 2004 R 2. Ordnung", sex = "m", collar = "Selekt", type = "Rententafel", country = "Deutschland", data = "unloaded", year = 1999)
  )
)

DAV2004R.female.2Ord.selekt = mortalityTable.trendProjection(
  name = "DAV 2004R Frauen 2.Ordnung, Selekt",
  ages = DAV2004r.data$x,
  baseYear = 1999,
  deathProbs = DAV2004r.data$`qy Sel 2.Ord`,
  trend = DAV2004r.data$`trendF Ziel 2.Ord`,
  trend2 = DAV2004r.data$`trendF Start 2.Ord`,
  dampingFunction = DAV2004R.trend.switching(T1 = 5, T2 = 10),
  # selection = DAV2004r.data.Selection$SelectFemale,
  # TODO: Add deathProbs before begin of selection effects
  data = list(
    dim = list(table = "DAV 2004 R 2. Ordnung", sex = "w", collar = "Selekt", type = "Rententafel", country = "Deutschland", data = "unloaded", year = 1999)
  )
)
DAV2004R[["m", "2. Ordnung", "Aggregat"]] = DAV2004R.male.2Ord
DAV2004R[["w", "2. Ordnung", "Aggregat"]] = DAV2004R.female.2Ord
DAV2004R[["m", "2. Ordnung", "Selektion"]] = DAV2004R.male.2Ord.selekt
DAV2004R[["w", "2. Ordnung", "Selektion"]] = DAV2004R.female.2Ord.selekt




#---------------------------------------------------------------------#
# Tafeln "Bestand" (mit Trendabschwächung)                         ####
#---------------------------------------------------------------------#

DAV2004R.male.Bestand = mortalityTable.trendProjection(
  name = "DAV 2004R Männer Bestand, Aggregat",
  ages = DAV2004r.data$x,
  baseYear = 1999,
  deathProbs = DAV2004r.data$`qx Agg Bestand`,
  trend = DAV2004r.data$`trendM Ziel Bestand`,
  trend2 = DAV2004r.data$`trendM Start Bestand`,
  dampingFunction = DAV2004R.trend.switching(T1 = 5, T2 = 10),
  data = list(
    dim = list(table = "DAV 2004 R Bestand", sex = "m", collar = "Aggregat", type = "Rententafel", country = "Deutschland", data = "loaded", year = 1999)
  )
)

DAV2004R.female.Bestand = mortalityTable.trendProjection(
  name = "DAV 2004R Frauen Bestand, Aggregat",
  ages = DAV2004r.data$x,
  baseYear = 1999,
  deathProbs = DAV2004r.data$`qy Agg Bestand`,
  trend = DAV2004r.data$`trendF Ziel Bestand`,
  trend2 = DAV2004r.data$`trendF Start Bestand`,
  dampingFunction = DAV2004R.trend.switching(T1 = 5, T2 = 10),
  data = list(
    dim = list(table = "DAV 2004 R Bestand", sex = "w", collar = "Aggregat", type = "Rententafel", country = "Deutschland", data = "loaded", year = 1999)
  )
)

DAV2004R.male.Bestand.selekt = mortalityTable.trendProjection(
  name = "DAV 2004R Männer Bestand, Selekt",
  ages = DAV2004r.data$x,
  baseYear = 1999,
  deathProbs = DAV2004r.data$`qx Sel Bestand`,
  trend = DAV2004r.data$`trendM Ziel Bestand`,
  trend2 = DAV2004r.data$`trendM Start Bestand`,
  dampingFunction = DAV2004R.trend.switching(T1 = 5, T2 = 10),
  # selection = DAV2004r.data.Selection$SelectMale,
  # TODO: Add deathProbs before begin of selection effects
  data = list(
    dim = list(table = "DAV 2004 R Bestand", sex = "m", collar = "Selekt", type = "Rententafel", country = "Deutschland", data = "loaded", year = 1999)
  )
)

DAV2004R.female.Bestand.selekt = mortalityTable.trendProjection(
  name = "DAV 2004R Frauen Bestand, Selekt",
  ages = DAV2004r.data$x,
  baseYear = 1999,
  deathProbs = DAV2004r.data$`qy Sel Bestand`,
  trend = DAV2004r.data$`trendF Ziel Bestand`,
  trend2 = DAV2004r.data$`trendF Start Bestand`,
  dampingFunction = DAV2004R.trend.switching(T1 = 5, T2 = 10),
  # selection = DAV2004r.data.Selection$SelectFemale,
  # TODO: Add deathProbs before begin of selection effects
  data = list(
    dim = list(table = "DAV 2004 R Bestand", sex = "w", collar = "Selekt", type = "Rententafel", country = "Deutschland", data = "uoaded", year = 1999)
  )
)
DAV2004R[["m", "Bestand", "Aggregat"]] = DAV2004R.male.Bestand
DAV2004R[["w", "Bestand", "Aggregat"]] = DAV2004R.female.Bestand
DAV2004R[["m", "Bestand", "Selektion"]] = DAV2004R.male.Bestand.selekt
DAV2004R[["w", "Bestand", "Selektion"]] = DAV2004R.female.Bestand.selekt




#---------------------------------------------------------------------#
# Tafeln "B20" (mit Trendabschwächung)                         ####
#---------------------------------------------------------------------#

DAV2004R.male.B20 = mortalityTable.trendProjection(
  name = "DAV 2004R Männer B20, Aggregat",
  ages = DAV2004r.data$x,
  baseYear = 1999,
  deathProbs = DAV2004r.data$`qx Agg Bestand`,
  trend = DAV2004r.data$`trendM Start Bestand`,
  data = list(
    dim = list(table = "DAV 2004 R B20", sex = "m", collar = "Aggregat", type = "Rententafel", country = "Deutschland", data = "loaded", year = 1999)
  )
)

DAV2004R.female.B20 = mortalityTable.trendProjection(
  name = "DAV 2004R Frauen B20, Aggregat",
  ages = DAV2004r.data$x,
  baseYear = 1999,
  deathProbs = DAV2004r.data$`qy Agg Bestand`,
  trend = DAV2004r.data$`trendF Start Bestand`,
  data = list(
    dim = list(table = "DAV 2004 R B20", sex = "w", collar = "Aggregat", type = "Rententafel", country = "Deutschland", data = "loaded", year = 1999)
  )
)

DAV2004R.male.B20.selekt = mortalityTable.trendProjection(
  name = "DAV 2004R Männer B20, Selekt",
  ages = DAV2004r.data$x,
  baseYear = 1999,
  deathProbs = DAV2004r.data$`qx Sel Bestand`,
  trend = DAV2004r.data$`trendM Start Bestand`,
  # selection = DAV2004r.data.Selection$SelectMale,
  # TODO: Add deathProbs before begin of selection effects
  data = list(
    dim = list(table = "DAV 2004 R B20", sex = "m", collar = "Selekt", type = "Rententafel", country = "Deutschland", data = "loaded", year = 1999)
  )
)

DAV2004R.female.B20.selekt = mortalityTable.trendProjection(
  name = "DAV 2004R Frauen B20, Selekt",
  ages = DAV2004r.data$x,
  baseYear = 1999,
  deathProbs = DAV2004r.data$`qy Sel Bestand`,
  trend = DAV2004r.data$`trendF Start Bestand`,
  # selection = DAV2004r.data.Selection$SelectFemale,
  # TODO: Add deathProbs before begin of selection effects
  data = list(
    dim = list(table = "DAV 2004 R B20", sex = "w", collar = "Selekt", type = "Rententafel", country = "Deutschland", data = "uoaded", year = 1999)
  )
)
DAV2004R[["m", "B20", "Aggregat"]] = DAV2004R.male.B20
DAV2004R[["w", "B20", "Aggregat"]] = DAV2004R.female.B20
DAV2004R[["m", "B20", "Selektion"]] = DAV2004R.male.B20.selekt
DAV2004R[["w", "B20", "Selektion"]] = DAV2004R.female.B20.selekt






# expand.grid(list(Geschlecht = c("m", "w"), Tafel = c("1. Ordnung", "Bestand", "B20", "2. Ordnung"), Typ = c("Aggregat", "Selektion")), )
# "m", "Bestand",    "Aggregat"
# "w", "Bestand",    "Aggregat"
# "m", "Bestand",    "Selektion"
# "w", "Bestand",    "Selektion"
# "m", "B20",        "Aggregat"
# "w", "B20",        "Aggregat"
# "m", "B20",        "Selektion"
# "w", "B20",        "Selektion"






#---------------------------------------------------------------------#
# Tafeln mit Altersverschiebung                                    ####
#---------------------------------------------------------------------#

DAV2004r.data.av = readxl::read_excel(
  DAV2004Rfile, sheet = "Grundtafeln", skip = 3,
  col_names = c(
    "x",
    "qx Bestand", "qy Bestand",
    "qx B20", "qy B20",
    "qx 1.Ord", "qy 1.Ord"
  ))

DAV2004r.data.av.MBestand = readxl::read_excel(
  DAV2004Rfile, sheet = "Altersverschiebungen", skip = 3,
  range = "A4:C24", col_names = c("from", "to", "shift"))
DAV2004r.data.av.FBestand = readxl::read_excel(
  DAV2004Rfile, sheet = "Altersverschiebungen", skip = 3,
  range = "E4:G22", col_names = c("from", "to", "shift"))
DAV2004r.data.av.MB20 = readxl::read_excel(
  DAV2004Rfile, sheet = "Altersverschiebungen", skip = 3,
  range = "I4:K26", col_names = c("from", "to", "shift"))
DAV2004r.data.av.FB20 = readxl::read_excel(
  DAV2004Rfile, sheet = "Altersverschiebungen", skip = 3,
  range = "M4:O24", col_names = c("from", "to", "shift"))
DAV2004r.data.av.M1Ord = readxl::read_excel(
  DAV2004Rfile, sheet = "Altersverschiebungen", skip = 3,
  range = "Q4:S29", col_names = c("from", "to", "shift"))
DAV2004r.data.av.F1Ord = readxl::read_excel(
  DAV2004Rfile, sheet = "Altersverschiebungen", skip = 3,
  range = "U4:W27", col_names = c("from", "to", "shift"))

DAV2004R.av = array(
  data = c(mortalityTable.NA),
  # data = c(NA),
  dim = c(2, 3),
  dimnames = list(Geschlecht = c("m", "w"), Tafel = c("1. Ordnung", "Bestand", "B20"))
)
DAV2004R.av[,] = NA



#---------------------------------------------------------------------#
# Tafeln mit AV: 1. Ordnung                                        ####
#---------------------------------------------------------------------#

DAV2004R.male.av = mortalityTable.ageShift(
  name = "DAV 2004R Männer AV",
  ages = DAV2004r.data.av$x,
  baseYear = 1965,
  deathProbs = DAV2004r.data.av$`qx 1.Ord`,
  ageShifts = DAV2004r.data.av.M1Ord,
  data = list(
    dim = list(table = "DAV 2004 R AV", sex = "m", collar = "Altersverschiebung", type = "Rententafel", country = "Deutschland", data = "age-shifted, loaded", year = 1965)
  )
)

DAV2004R.female.av = mortalityTable.ageShift(
  name = "DAV 2004R Frauen AV",
  ages = DAV2004r.data.av$x,
  baseYear = 1965,
  deathProbs = DAV2004r.data.av$`qy 1.Ord`,
  ageShifts = DAV2004r.data.av.F1Ord,
  data = list(
    dim = list(table = "DAV 2004 R AV", sex = "w", collar = "Altersverschiebung", type = "Rententafel", country = "Deutschland", data = "age-shifted, loaded", year = 1965)
  )
)


DAV2004R.av[["m", "1. Ordnung"]] = DAV2004R.male.av
DAV2004R.av[["w", "1. Ordnung"]] = DAV2004R.female.av



#---------------------------------------------------------------------#
# Tafeln mit AV: Bestand                                           ####
#---------------------------------------------------------------------#

DAV2004R.male.Bestand.av = mortalityTable.ageShift(
  name = "DAV 2004R Männer Bestand AV",
  ages = DAV2004r.data.av$x,
  baseYear = 1965,
  deathProbs = DAV2004r.data.av$`qx Bestand`,
  ageShifts = DAV2004r.data.av.MBestand,
  data = list(
    dim = list(table = "DAV 2004 R Bestand AV", sex = "m", collar = "Altersverschiebung", type = "Rententafel", country = "Deutschland", data = "age-shifted, loaded", year = 1965)
  )
)

DAV2004R.female.Bestand.av = mortalityTable.ageShift(
  name = "DAV 2004R Frauen Bestand AV",
  ages = DAV2004r.data.av$x,
  baseYear = 1965,
  deathProbs = DAV2004r.data.av$`qy Bestand`,
  ageShifts = DAV2004r.data.av.FBestand,
  data = list(
    dim = list(table = "DAV 2004 R Bestand AV", sex = "w", collar = "Altersverschiebung", type = "Rententafel", country = "Deutschland", data = "age-shifted, loaded", year = 1965)
  )
)


DAV2004R.av[["m", "Bestand"]] = DAV2004R.male.Bestand.av
DAV2004R.av[["w", "Bestand"]] = DAV2004R.female.Bestand.av



#---------------------------------------------------------------------#
# Tafeln mit AV: B2-                                               ####
#---------------------------------------------------------------------#

DAV2004R.male.B20.av = mortalityTable.ageShift(
  name = "DAV 2004R Männer B20 AV",
  ages = DAV2004r.data.av$x,
  baseYear = 1965,
  deathProbs = DAV2004r.data.av$`qx B20`,
  ageShifts = DAV2004r.data.av.MB20,
  data = list(
    dim = list(table = "DAV 2004 R B20 AV", sex = "m", collar = "Altersverschiebung", type = "Rententafel", country = "Deutschland", data = "age-shifted, loaded", year = 1965)
  )
)

DAV2004R.female.B20.av = mortalityTable.ageShift(
  name = "DAV 2004R Frauen B20 AV",
  ages = DAV2004r.data.av$x,
  baseYear = 1965,
  deathProbs = DAV2004r.data.av$`qy B20`,
  ageShifts = DAV2004r.data.av.FB20,
  data = list(
    dim = list(table = "DAV 2004 R B20 AV", sex = "w", collar = "Altersverschiebung", type = "Rententafel", country = "Deutschland", data = "age-shifted, loaded", year = 1965)
  )
)


DAV2004R.av[["m", "B20"]] = DAV2004R.male.B20.av
DAV2004R.av[["w", "B20"]] = DAV2004R.female.B20.av





Beginn = 1999
Eintrittsalter = 60
ages = Eintrittsalter:121
Aufschubdauer = 0
YOB = Beginn - Eintrittsalter

data.frame(
  x = ages,
  t = YOB + ages,
  Sel2O = deathProbabilities(DAV2004R[["m","2. Ordnung","Selektion"]], YOB = YOB, ages = ages),
  Agg2O = deathProbabilities(DAV2004R[["m","2. Ordnung","Aggregat"]], YOB = YOB, ages = ages),

  SelBestand = deathProbabilities(DAV2004R[["m","Bestand","Selektion"]], YOB = YOB, ages = ages),
  AggBestand = deathProbabilities(DAV2004R[["m","Bestand","Aggregat"]], YOB = YOB, ages = ages),
  AVBestand = deathProbabilities(DAV2004R.av[["m","Bestand"]], YOB = YOB, ages = ages),

  SelB20 = deathProbabilities(DAV2004R[["m","B20","Selektion"]], YOB = YOB, ages = ages),
  AggB20 = deathProbabilities(DAV2004R[["m","B20","Aggregat"]], YOB = YOB, ages = ages),
  AVB20 = deathProbabilities(DAV2004R.av[["m","B20"]], YOB = YOB, ages = ages),

  Sel1Ord = deathProbabilities(DAV2004R[["m","1. Ordnung","Selektion"]], YOB = YOB, ages = ages),
  Agg1Ord = deathProbabilities(DAV2004R[["m","1. Ordnung","Aggregat"]], YOB = YOB, ages = ages),
  AV1Ord = deathProbabilities(DAV2004R.av[["m","1. Ordnung"]], YOB = YOB, ages = ages)

) %>% write.xlsx(file = paste0("DAV2004R_Examples_", YOB, "_Eintritt", Eintrittsalter, "_Aufschub", Aufschubdauer, ".NEW.xlsx"))
deathProbabilities(DAV2004R[["m",1,1]], YOB = YOB)







