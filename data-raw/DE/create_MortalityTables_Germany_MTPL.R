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






#############################################################################h#
# DAV 2004R exact (Male, Female, unisex)                                   ####
# gender-specific tables also have 2nd-order tables, unisex only 1st-order table
#############################################################################h#

DAV2004Rfile = here::here("data-raw", "AT", "Annuities", "DAV2004R.xlsx")
DAV2004RUfile = here::here("data-raw", "AT", "Annuities", "DAV2004R unisex Endtafel.xlsx")
DAV2004Rfile.out = here::here("data", "DAV2004R.RData")
DAV2004R.nodamping.file.out = here::here("data", "DAV2004R.nodamping.RData")

DAV2004r.data = full_join(
  readxl::read_excel(
    DAV2004Rfile, sheet = "DAV 2004R exakt", skip = 4, n_max = 121,
    col_types = c(rep("guess", 7), rep("skip", 13)),
    col_names = c("x", "qx", "qx Gruppe", "trendM", "qy", "qy Gruppe", "trendF")
  ),
  readxl::read_excel(
    DAV2004RUfile, sheet = "DAV 2004R unisex exakt", skip = 4, n_max = 121,
    col_types = c(rep("guess", 4), rep("skip", 9)),
    col_names = c("x", "qu", "qu Gruppe", "trendU")
  ),
  by = "x"
)

DAV2004r.data.2Ord = readxl::read_excel(
  DAV2004Rfile, sheet = "DAV 2004R 2nd Ord", skip = 4, n_max = 121,
  col_types = c(rep("guess", 7), rep("skip", 13)),
  col_names = c("x", "qx", "qx Gruppe", "trendM", "qy", "qy Gruppe", "trendF")
)


############################################################################## #

DAV2004R.trend.damping = function(t) {
  100*atan(t/100)
}
DAV2004R_gen = function(df, nm, probs, trend, sex = "m", data = "loaded", type = "Rententafel") {
    mortalityTable.trendProjection(
      name = nm,
      ages = df$x,
      baseYear = 2001,
      deathProbs = df[[probs]],
      trend = df[[trend]],
      dampingFunction = DAV2004R.trend.damping,
      data = list(
        dim = list(table = "DAV 2005-R", sex = sex, collar = "Rententafel", type = type, country = "Österreich", data = data, year = 2001, table = "DAV 2005-R")
      )
  )
}

DAV2004R.male            = DAV2004R_gen(DAV2004r.data, "DAV 2004R male (exact), loaded",   "qx", "trendM", sex = "m", data = "loaded");
DAV2004R.female          = DAV2004R_gen(DAV2004r.data, "DAV 2004R female (exact), loaded", "qy", "trendF", sex = "w", data = "loaded");
DAV2004R.unisex          = DAV2004R_gen(DAV2004r.data, "DAV 2004R unisex (exact), loaded", "qu", "trendU", sex = "u", data = "loaded");
DAV2004R.male.unloaded   = DAV2004R_gen(DAV2004r.data.2Ord, "DAV 2004R male (exact), unloaded",   "qx", "trendM", sex = "m", data = "unloaded");
DAV2004R.female.unloaded = DAV2004R_gen(DAV2004r.data.2Ord, "DAV 2004R female (exact), unloaded", "qy", "trendF", sex = "w", data = "unloaded");
DAV2004R.male.group      = DAV2004R_gen(DAV2004r.data, "DAV 2004R male group (exact), loaded",   "qx Gruppe", "trendM", sex = "m", data = "loaded, group", type = "Gruppenrententafel");
DAV2004R.female.group    = DAV2004R_gen(DAV2004r.data, "DAV 2004R female group (exact), loaded", "qy Gruppe", "trendF", sex = "w", data = "loaded, group", type = "Gruppenrententafel");
DAV2004R.unisex.group    = DAV2004R_gen(DAV2004r.data, "DAV 2004R unisex group (exact), loaded", "qu Gruppe", "trendU", sex = "u", data = "loaded, group", type = "Gruppenrententafel");
DAV2004R.male.group.unloaded   = DAV2004R_gen(DAV2004r.data.2Ord, "DAV 2004R male group (exact), loaded",   "qx Gruppe", "trendM", sex = "m", data = "loaded, group", type = "Gruppenrententafel");
DAV2004R.female.group.unloaded = DAV2004R_gen(DAV2004r.data.2Ord, "DAV 2004R female group (exact), loaded", "qy Gruppe", "trendF", sex = "w", data = "loaded, group", type = "Gruppenrententafel");

DAV2004R = array(
  data = c(mortalityTable.NA),
  dim = c(3, 2, 2),
  dimnames = list(Geschlecht = c("m", "w", "u"), Collar = c("Einzel", "Gruppe"), Type = c("loaded", "unloaded"))
)

DAV2004R[["m", "Einzel", "loaded"]] =   DAV2004R.male
DAV2004R[["w", "Einzel", "loaded"]] =   DAV2004R.female
DAV2004R[["u", "Einzel", "loaded"]] =   DAV2004R.unisex
DAV2004R[["m", "Gruppe", "loaded"]] =   DAV2004R.male.group
DAV2004R[["w", "Gruppe", "loaded"]] =   DAV2004R.female.group
DAV2004R[["u", "Gruppe", "loaded"]] =   DAV2004R.unisex.group

DAV2004R[,, "unloaded"] = NA
DAV2004R[["m", "Einzel", "unloaded"]] = DAV2004R.male.unloaded
DAV2004R[["w", "Einzel", "unloaded"]] = DAV2004R.female.unloaded
DAV2004R[["m", "Gruppe", "unloaded"]] = DAV2004R.male.group.unloaded
DAV2004R[["w", "Gruppe", "unloaded"]] = DAV2004R.female.group.unloaded

save(
  DAV2004R,
  DAV2004R.male, DAV2004R.female, DAV2004R.unisex,
  DAV2004R.male.unloaded, DAV2004R.female.unloaded,
  DAV2004R.male.group, DAV2004R.female.group, DAV2004R.unisex.group,
  DAV2004R.male.group.unloaded, DAV2004R.female.group.unloaded,
  file = DAV2004Rfile.out
)




DAV2004R.male.nodamping            = mT.setDimInfo(undampenTrend(DAV2004R.male), data = "loaded, no trend damping")
DAV2004R.female.nodamping          = mT.setDimInfo(undampenTrend(DAV2004R.female), data = "loaded, no trend damping");
DAV2004R.unisex.nodamping          = mT.setDimInfo(undampenTrend(DAV2004R.unisex), data = "loaded, no trend damping");
DAV2004R.male.nodamping.unloaded   = mT.setDimInfo(undampenTrend(DAV2004R.male.unloaded), data = "unloaded, no trend damping");
DAV2004R.female.nodamping.unloaded = mT.setDimInfo(undampenTrend(DAV2004R.female.unloaded), data = "unloaded, no trend damping");
DAV2004R.male.nodamping.group      = mT.setDimInfo(undampenTrend(DAV2004R.male.group), data = "loaded, group, no trend damping");
DAV2004R.female.nodamping.group    = mT.setDimInfo(undampenTrend(DAV2004R.female.group), data = "loaded, group, no trend damping");
DAV2004R.unisex.nodamping.group    = mT.setDimInfo(undampenTrend(DAV2004R.unisex.group), data = "loaded, group, no trend damping");
DAV2004R.male.nodamping.group.unloaded      = mT.setDimInfo(undampenTrend(DAV2004R.male.group.unloaded), data = "unloaded, group, no trend damping");
DAV2004R.female.nodamping.group.unloaded    = mT.setDimInfo(undampenTrend(DAV2004R.female.group.unloaded), data = "unloaded, group, no trend damping");


DAV2004R.nodamping = array(
  data = c(mortalityTable.NA),
  dim = c(3, 2, 2),
  dimnames = list(Geschlecht = c("m", "w", "u"), Collar = c("Einzel", "Gruppe"), Type = c("loaded", "unloaded"))
)

DAV2004R.nodamping[["m", "Einzel", "loaded"]] =   DAV2004R.male.nodamping
DAV2004R.nodamping[["w", "Einzel", "loaded"]] =   DAV2004R.female.nodamping
DAV2004R.nodamping[["u", "Einzel", "loaded"]] =   DAV2004R.unisex.nodamping
DAV2004R.nodamping[["m", "Gruppe", "loaded"]] =   DAV2004R.male.nodamping.group
DAV2004R.nodamping[["w", "Gruppe", "loaded"]] =   DAV2004R.female.nodamping.group
DAV2004R.nodamping[["u", "Gruppe", "loaded"]] =   DAV2004R.unisex.nodamping.group

DAV2004R.nodamping[, , "unloaded"] = NA
DAV2004R.nodamping[["m", "Einzel", "unloaded"]] = DAV2004R.male.nodamping.unloaded
DAV2004R.nodamping[["w", "Einzel", "unloaded"]] = DAV2004R.female.nodamping.unloaded
DAV2004R.nodamping[["m", "Gruppe", "unloaded"]] = DAV2004R.male.nodamping.group.unloaded
DAV2004R.nodamping[["w", "Gruppe", "unloaded"]] = DAV2004R.female.nodamping.group.unloaded


save(
  DAV2004R.nodamping,
  DAV2004R.male.nodamping,
  DAV2004R.female.nodamping,
  DAV2004R.unisex.nodamping,
  DAV2004R.male.nodamping.unloaded,
  DAV2004R.female.nodamping.unloaded,
  DAV2004R.male.nodamping.group,
  DAV2004R.female.nodamping.group,
  DAV2004R.unisex.nodamping.group,
  DAV2004R.male.nodamping.group.unloaded,
  DAV2004R.female.nodamping.group.unloaded,

  file = DAV2004R.nodamping.file.out
)


##############################################################################h#
# DAV 2004R with age-shifting (Male, Female, unisex), 1st-order only        ####
##############################################################################h#

DAV2004R.av.file.out = here::here("data", "DAV2004R.av.RData")

DAV2004rAV.base = full_join(
  readxl::read_excel(
    DAV2004Rfile, sheet = "DAV 2004R mit AV", skip = 13, n_max = 121,
    col_types = c(rep("guess", 5), rep("skip", 10)),
    col_names = c("x", "qx", "qx Gruppe", "qy", "qy Gruppe")
  ),
  readxl::read_excel(
    DAV2004RUfile, sheet = "DAV 2004R unisex mit AV", skip = 4, n_max = 121,
    col_types = c(rep("guess", 3), rep("skip", 4)),
    col_names = c("x", "qu", "qu Gruppe")
  ),
  by = "x"
)
DAV2004rAV.shift = full_join(
  readxl::read_excel(
    DAV2004Rfile, sheet = "DAV 2004R mit AV", skip = 13, n_max = 116,
    col_types = c(rep("skip", 6), "guess", rep(c("skip", "guess"), 4)),
    col_names = c("YOB", "shiftM", "shiftMG", "shiftF", "shiftFG")
  ),
  readxl::read_excel(
    DAV2004RUfile, sheet = "DAV 2004R unisex mit AV", skip = 4, n_max = 116,
    col_types = c(rep("skip", 4), rep("guess", 3)),
    col_names = c("YOB", "shiftU", "shiftUG")
  ),
  by = "YOB"
)

DAV2004R_gen.av = function(nm, probs, shft, sex = "m", data = "age-shift, loaded", type = "Rententafel", year = 1965) {
  mortalityTable.ageShift(
    name = nm,
    ages = DAV2004rAV.base$x,
    deathProbs = DAV2004rAV.base[[probs]],
    ageShifts = generateAgeShiftFromTable(DAV2004rAV.shift %>% mutate(shift = .[[shft]])),
    baseYear = year,
    data = list(
      dim = list(table = "DAV 2005-R AV", sex = sex, collar = "Rententafel", type = "Rententafel", country = "Österreich", data = data, year = year)
    )
  )
}

DAV2004R.male.av = DAV2004R_gen.av(
  "DAV 2004R male (age-shifted), loaded",
  "qx", "shiftM", sex = "m", data = "age-shift, loaded");
DAV2004R.female.av = DAV2004R_gen.av(
  "DAV 2004R female (age-shifted), loaded",
  "qy", "shiftF", sex = "w", data = "age-shift, loaded");
DAV2004R.unisex.av = DAV2004R_gen.av(
  "DAV 2004R unisex (age-shifted), loaded",
  "qu", "shiftU", sex = "u", data = "age-shift, loaded", year = 1972);
DAV2004R.male.group.av = DAV2004R_gen.av(
  "DAV 2004R male group (age-shifted), loaded",
  "qx Gruppe", "shiftMG", sex = "m", data = "age-shift, loaded, group", type = "Gruppenrententafel");
DAV2004R.female.group.av = DAV2004R_gen.av(
  "DAV 2004R female group (age-shifted), loaded",
  "qy Gruppe", "shiftFG", sex = "w", data = "age-shift, loaded, group", type = "Gruppenrententafel");
DAV2004R.unisex.group.av = DAV2004R_gen.av(
  "DAV 2004R unisex group (age-shifted), loaded",
  "qu Gruppe", "shiftUG", sex = "u", data = "age-shift, loaded, group",
  type = "Gruppenrententafel", year = 1972)


DAV2004R.av = array(
  data = c(mortalityTable.NA),
  dim = c(3, 2),
  dimnames = list(Geschlecht = c("m", "w", "u"), Collar = c("Einzel", "Gruppe"))
)

DAV2004R.av[["m", "Einzel"]] = DAV2004R.male.av
DAV2004R.av[["w", "Einzel"]] = DAV2004R.female.av
DAV2004R.av[["u", "Einzel"]] = DAV2004R.unisex.av
DAV2004R.av[["m", "Gruppe"]] = DAV2004R.male.group.av
DAV2004R.av[["w", "Gruppe"]] = DAV2004R.female.group.av
DAV2004R.av[["u", "Gruppe"]] = DAV2004R.unisex.group.av



save(
  DAV2004R.av,
  DAV2004R.male.av,
  DAV2004R.female.av,
  DAV2004R.unisex.av,
  DAV2004R.male.group.av,
  DAV2004R.female.group.av,
  DAV2004R.unisex.group.av,

  file = DAV2004R.av.file.out
)


ages = 0:120
data.frame(ages = ages,
           m = deathProbabilities(DAV2004R[["m", "Einzel", "loaded"]], YOB = 1930),
           w = deathProbabilities(DAV2004R[["w", "Einzel", "loaded"]], YOB = 1930),
           mG = deathProbabilities(DAV2004R[["m", "Gruppe", "loaded"]], YOB = 1930),
           wG = deathProbabilities(DAV2004R[["w", "Gruppe", "loaded"]], YOB = 1930),
           m.2Ord = deathProbabilities(DAV2004R[["m", "Einzel", "unloaded"]], YOB = 1930),
           w.2Ord = deathProbabilities(DAV2004R[["w", "Einzel", "unloaded"]], YOB = 1930),
           mG.2Ord = deathProbabilities(DAV2004R[["m", "Gruppe", "unloaded"]], YOB = 1930),
           wG.2Ord = deathProbabilities(DAV2004R[["w", "Gruppe", "unloaded"]], YOB = 1930)
)


