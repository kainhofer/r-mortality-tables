#############################################################################m#
#  Skript to generate German annuity mortality table objects               ####
#############################################################################m#
library(MortalityTables)
library(here)
library(readxl)




############################################################################h#
# DAV 1997HUR exact and age-shifted (Male, Female), 1st-order only           ####
#############################################################################h#

dav1997HURfile = here::here("data-raw", "DE", "Annuities", "DAV 1997HUR.xlsx")
dav1997HURfile.out = here::here("data", "DAV1997HUR.RData")
dav1997HURav.file.out = here::here("data", "DAV1997HUR.av.RData")


dav1997HUR.data = read_excel(dav1997HURfile, sheet = "DAV 1997 HUR Basistafeln", skip = 4, col_names = c("x", "qx", "qy", "trendM", "trendF", "qx AV", "qy AV"))
dav1997HUR.data.av.shiftM = read_excel(dav1997HURfile, sheet = "Altersverschiebung", range = "A4:C17", col_names = c("from", "to", "shift"))
dav1997HUR.data.av.shiftF = read_excel(dav1997HURfile, sheet = "Altersverschiebung", range = "E4:G20", col_names = c("from", "to", "shift"))

DAV1997HUR.male = mortalityTable.trendProjection(
  name = "DAV 1997 HUR Männer",
  ages = dav1997HUR.data$x,
  deathProbs = dav1997HUR.data$qx,
  trend = dav1997HUR.data$trendM,
  baseYear = 2000,
  data = list(
    dim = list(table = "DAV 1997 HUR", sex = "m", collar = "MTPL", type = "Rententafel", country = "Deutschland", data = "official", year = 2000)
  )
)

DAV1997HUR.female = mortalityTable.trendProjection(
  name = "DAV 1997 HUR Frauen",
  ages = dav1997HUR.data$x,
  deathProbs = dav1997HUR.data$qy,
  trend = dav1997HUR.data$trendF,
  baseYear = 2000,
  data = list(
    dim = list(table = "DAV 1997 HUR", sex = "w", collar = "MTPL", type = "Rententafel", country = "Deutschland", data = "official", year = 2000)
  )
)

DAV1997HUR.male.av = mortalityTable.ageShift(
  name = "DAV 1997 HUR Männer mit Altersverschiebung",
  ages = dav1997HUR.data$x,
  deathProbs = dav1997HUR.data$`qx AV`,
  ageShifts = dav1997HUR.data.av.shiftM,
  baseYear = 1955,
  data = list(
    dim = list(table = "DAV 1997 HUR", sex = "m", collar = "MTPL", type = "Rententafel", country = "Deutschland", data = "Altersverschiebung", year = 1955)
  )
)
DAV1997HUR.female.av = mortalityTable.ageShift(
  name = "DAV 1997 HUR Frauen mit Altersverschiebung",
  ages = dav1997HUR.data$x,
  deathProbs = dav1997HUR.data$`qy AV`,
  ageShifts = dav1997HUR.data.av.shiftF,
  baseYear = 1955,
  data = list(
    dim = list(table = "DAV 1997 HUR", sex = "w", collar = "MTPL", type = "Rententafel", country = "Deutschland", data = "Altersverschiebung", year = 1955)
  )
)


DAV1997HUR = array(
  data = c(mortalityTable.NA),
  dim = c(2),
  dimnames = list(Geschlecht = c("m", "w"))
)

DAV1997HUR.av = array(
  data = c(mortalityTable.NA),
  dim = c(2),
  dimnames = list(Geschlecht = c("m", "w"))
)

DAV1997HUR[["m"]] = DAV1997HUR.male
DAV1997HUR[["w"]] = DAV1997HUR.female
DAV1997HUR.av[["m"]] = DAV1997HUR.male.av
DAV1997HUR.av[["w"]] = DAV1997HUR.female.av

save(
  DAV1997HUR, DAV1997HUR.male, DAV1997HUR.female,
  file = dav1997HURfile.out
)

save(
  DAV1997HUR.av, DAV1997HUR.male.av, DAV1997HUR.female.av,
  file = dav1997HURav.file.out
)






#############################################################################h#
### DAV 2006-HUR                                                           ####
#############################################################################h#

dav2006hur.file = here::here("data-raw", "DE", "Annuities", "2013-01-25-Herleitung_DAV-Sterbetafel_2006_HUR_final_ANLAGE.xls")
dav2006HURfile.out = here::here("data", "DAV2006HUR.RData")
dav2006HURav.file.out = here::here("data", "DAV2006HUR.RData")

dav2006hur.data = left_join(left_join(
  readxl::read_excel(dav2006hur.file, sheet = "Basistafeln", skip = 4, col_names = c("x", "qx", "qy")),
  readxl::read_excel(dav2006hur.file, sheet = "Trends", skip = 4, col_names = c("x", "trendM", "trendF")),
  by = "x"),
  readxl::read_excel(dav2006hur.file, sheet = "Grundtafeln", skip = 3, col_names = c("x", "qx AV", "qy AV")),
  by = "x"
)
dav2006hur.AltersverschiebungM = readxl::read_excel(dav2006hur.file, range = "Altersverschiebungen!A4:C22", col_names = c("from", "to", "shift"))
dav2006hur.AltersverschiebungF = readxl::read_excel(dav2006hur.file, range = "Altersverschiebungen!E4:G18", col_names = c("from", "to", "shift"))

DAV2006HUR.male = mortalityTable.trendProjection(
  name = "DAV 2006 HUR Männer",
  ages = dav2006hur.data$x,
  deathProbs = dav2006hur.data$qx,
  trend = dav2006hur.data$trendM,
  baseYear = 2001,
  data = list(
    dim = list(table = "DAV 2006 HUR", sex = "m", collar = "MTPL", type = "Rententafel", country = "Deutschland", data = "official", year = 2001)
  )
)

DAV2006HUR.female = mortalityTable.trendProjection(
  name = "DAV 2006 HUR Frauen",
  ages = dav2006hur.data$x,
  deathProbs = dav2006hur.data$qy,
  trend = dav2006hur.data$trendF,
  baseYear = 2001,
  data = list(
    dim = list(table = "DAV 2006 HUR", sex = "w", collar = "MTPL", type = "Rententafel", country = "Deutschland", data = "official", year = 2001)
  )
)

DAV2006HUR.male.av = mortalityTable.ageShift(
  name = "DAV 2006 HUR Männer mit Altersverschiebung",
  ages = dav2006hur.data$x,
  deathProbs = dav2006hur.data$`qx AV`,
  ageShifts = dav2006hur.AltersverschiebungM,
  baseYear = 1946,
  data = list(
    dim = list(table = "DAV 2006 HUR", sex = "m", collar = "MTPL", type = "Rententafel", country = "Deutschland", data = "Altersverschiebung", year = 1946)
  )
)
DAV2006HUR.female.av = mortalityTable.ageShift(
  name = "DAV 2006 HUR Frauen mit Altersverschiebung",
  ages = dav2006hur.data$x,
  deathProbs = dav2006hur.data$`qy AV`,
  ageShifts = dav2006hur.AltersverschiebungF,
  baseYear = 1946,
  data = list(
    dim = list(table = "DAV 2006 HUR", sex = "w", collar = "MTPL", type = "Rententafel", country = "Deutschland", data = "Altersverschiebung", year = 1946)
  )
)

# Second-order Tables use the 2nd-order trend of the DAV 2004-R:
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

DAV2004r.trend = readxl::read_excel(
  here::here("data-raw", "DE", "Annuities", "DAV_2004_R.xls"), sheet = "Trends", skip = 4,
  col_names = c(
    "x",
    "trendM Start 2.Ord", "trendF Start 2.Ord", "trendM Ziel 2.Ord", "trendF Ziel 2.Ord",
    "trendM Start Bestand", "trendF Start Bestand", "trendM Ziel Bestand", "trendF Ziel Bestand",
    "trendM 1.Ord", "trendF 1.Ord"
  )
)

DAV2006HUR.male.2Ord = mortalityTable.trendProjection(
  name = "DAV 2006 HUR Männer",
  ages = dav2006hur.data$x,
  deathProbs = dav2006hur.data$qx,
  trend = DAV2004r.trend$`trendM Ziel 2.Ord`,
  trend2 = DAV2004r.trend$`trendM Start 2.Ord`,
  dampingFunction = DAV2004R.trend.switching(T1 = 5, T2 = 10),
  baseYear = 2001,
  data = list(
    dim = list(table = "DAV 2006 HUR", sex = "m", collar = "MTPL", type = "Rententafel", country = "Deutschland", data = "official", year = 2001)
  )
)

DAV2006HUR.female.2Ord = mortalityTable.trendProjection(
  name = "DAV 2006 HUR Frauen",
  ages = dav2006hur.data$x,
  deathProbs = dav2006hur.data$qy,
  trend = DAV2004r.trend$`trendF Ziel 2.Ord`,
  trend2 = DAV2004r.trend$`trendF Start 2.Ord`,
  dampingFunction = DAV2004R.trend.switching(T1 = 5, T2 = 10),
  baseYear = 2001,
  data = list(
    dim = list(table = "DAV 2006 HUR", sex = "w", collar = "MTPL", type = "Rententafel", country = "Deutschland", data = "official", year = 2001)
  )
)



DAV2006HUR = array(
  data = c(mortalityTable.NA),
  dim = c(2),
  dimnames = list(Geschlecht = c("m", "w"))
)

DAV2006HUR.av = array(
  data = c(mortalityTable.NA),
  dim = c(2),
  dimnames = list(Geschlecht = c("m", "w"))
)

DAV2006HUR[["m"]] = DAV2006HUR.male
DAV2006HUR[["w"]] = DAV2006HUR.female
DAV2006HUR.av[["m"]] = DAV2006HUR.male.av
DAV2006HUR.av[["w"]] = DAV2006HUR.female.av

save(
  DAV2006HUR, DAV2006HUR.male, DAV2006HUR.female,
  file = dav2006HURfile.out
)

save(
  DAV2006HUR, DAV2006HUR.male, DAV2006HUR.female,
  file = dav2006HURav.file.out
)



