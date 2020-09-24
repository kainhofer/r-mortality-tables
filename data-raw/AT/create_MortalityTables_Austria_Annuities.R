#############################################################################m#
#  Skript to generate Austrian annuity mortality table objects             ####
#############################################################################m#
library(MortalityTables)
library(here)
library(readxl)




#############################################################################h#
### RR67 table                                                             ####
#############################################################################h#

RR67file = here::here("data-raw", "AT", "Annuities", "ÖVM59-61_RR67_Tabellen.xlsx")
RR67file.out = here::here("data", "RR67.RData")

RR67.data = readxl::read_excel(RR67file, sheet = "Tafel", skip = 6, n_max = 76)
RR67 = mortalityTable.period(
  name = "ÖVM 59/61 - RR 67 - 3%",
  ages = RR67.data$x,
  deathProbs = RR67.data$`qx korrigiert`,
  baseYear = 1960,
  data = list(
    dim = list(
      table = "ÖVM 59/61 - RR67",
      sex = "m",
      collar = "Rententafel",
      type = "Rententafel",
      country = "Österreich",
      data = "unloaded",
      year = 1960)
  )
);


save(RR67, file = RR67file.out)





#############################################################################h#
### EROM/EROF 85 and G 1985 (period and age-shifted generation)            ####
#############################################################################h#

eromf85file = here::here("data-raw", "AT", "Annuities", "AT_AVÖ_EROM-EROF.xlsx")
eromfile.out = here::here("data", "EROM-EROF.RData")

eromf.data = readxl::read_excel(eromf85file, sheet = "EROM-F Basistafeln")
eromf.data.av.M = readxl::read_excel(eromf85file, sheet = "EROM-F G AV", skip = 2, n_max = 15) %>% rename(from = von, to = bis, shift = Shift)
eromf.data.av.F = readxl::read_excel(eromf85file, sheet = "EROM-F G AV", skip = 19, n_max = 22) %>% rename(from = von, to = bis, shift = Shift)

EROM85 = mortalityTable.period(
  name = "EROM 85, male",
  ages = eromf.data$Alter,
  deathProbs = eromf.data$`EROM 85`,
  baseYear = 2020,
  data = list(
    dim = list(
      table = "EROM 85",
      sex = "m",
      collar = "Rententafel",
      type = "Rententafel",
      country = "Österreich",
      data = "unloaded",
      year = 2020)
  )
);

EROF85 = mortalityTable.period(
  name = "EROF 85, female",
  ages = eromf.data$Alter,
  deathProbs = eromf.data$`EROF 85`,
  baseYear = 2020,
  data = list(
    dim = list(
      table = "EROF 85",
      sex = "w",
      collar = "Rententafel",
      type = "Rententafel",
      country = "Österreich",
      data = "unloaded",
      year = 2020)
  )
);

EROM.G1950 = mortalityTable.ageShift(
  name = "EROM G 1950, male",
  ages = eromf.data$Alter,
  deathProbs = eromf.data$`EROM G1950`,
  ageShifts = eromf.data.av.M,
  baseYear = 1950,
  data = list(
    dim = list(
      table = "EROM G1950",
      sex = "m",
      collar = "Rententafel",
      type = "Rententafel",
      country = "Österreich",
      data = "age-shifted",
      year = 1950)
  )
);
EROF.G1950 = mortalityTable.ageShift(
  name = "EROF G 1950, female",
  ages = eromf.data$Alter,
  deathProbs = eromf.data$`EROF G1950`,
  ageShifts = eromf.data.av.F,
  baseYear = 1950,
  data = list(
    dim = list(
      table = "EROF G1950",
      sex = "w",
      collar = "Rententafel",
      type = "Rententafel",
      country = "Österreich",
      data = "age-shifted",
      year = "EROF G1950")
  )
);

save(EROM85, EROF85, EROM.G1950, EROF.G1950,
     file = eromfile.out)






#############################################################################h#
# AVÖ 1996R exact (Male, Female), 1st-order only                           ####
#############################################################################h#

avoe1996Rfile = here::here("data-raw", "AT", "Annuities", "AVOe1996R.xlsx")
avoe1996Rfile.out = here::here("data", "AVOe1996R.RData")


avoe1996r.data.m = readxl::read_excel(avoe1996Rfile, sheet = "AVÖ 1996R M", skip = 3)
avoe1996r.data.f = readxl::read_excel(avoe1996Rfile, sheet = "AVÖ 1996R F", skip = 3)

AVOe1996R.trend.switching = function(year) {
  if (year <= 1971) {
    15/(1991 - year)
  } else if (1971 < year && year < 1981) {
    1 + (year - 1981)^2/(year - 1991)/20
  } else if (1981 <= year && year <= 2000) {
    1
  } else if (2000 < year && year < 2010) {
    1 - (year - 2000)^2/(year - 1991)/20
  } else if (year >= 2010) {
    14/(year - 1991)
  }
}

AVOe1996R.male = mortalityTable.trendProjection(
  name = "AVÖ 1996R male",
  ages = avoe1996r.data.m$x, baseYear = 1991,
  deathProbs = avoe1996r.data.m$`qx(1991)` * avoe1996r.data.m$`Abschlag "Einzel"`,
  trend = avoe1996r.data.m$`lx(l)`,
  trend2 = avoe1996r.data.m$`lx(k)`,
  dampingFunction = AVOe1996R.trend.switching,
  data = list(
    dim = list(table = "AVÖ 1996R", sex = "m", collar = "Rententafel", type = "Rententafel", country = "Österreich", data = "official", year = 1991)
  )
)

AVOe1996R.female = mortalityTable.trendProjection(
  name = "AVÖ 1996R female",
  ages = avoe1996r.data.f$x, baseYear = 1991,
  deathProbs = avoe1996r.data.f$`qx(1991)` * avoe1996r.data.f$`Abschlag "Einzel"`,
  trend = avoe1996r.data.f$`lx(l)`,
  trend2 = avoe1996r.data.f$`lx(k)`,
  dampingFunction = AVOe1996R.trend.switching,
  data = list(
    dim = list(table = "AVÖ 1996R", sex = "w", collar = "Rententafel", type = "Rententafel", country = "Österreich", data = "official", year = 1991)
  )
)

AVOe1996R.male.group = mortalityTable.trendProjection(
  name = "AVÖ 1996R male, group",
  ages = avoe1996r.data.m$x, baseYear = 1991,
  deathProbs = avoe1996r.data.m$`qx(1991)` * avoe1996r.data.m$`Abschlag "Gruppen"`,
  trend = avoe1996r.data.m$`lx(l)`,
  trend2 = avoe1996r.data.m$`lx(k)`,
  dampingFunction = AVOe1996R.trend.switching,
  data = list(
    dim = list(table = "AVÖ 1996R", sex = "m", collar = "Gruppenrententafel", type = "Rententafel", country = "Österreich", data = "official", year = 1991)
  )
);

AVOe1996R.female.group = mortalityTable.trendProjection(
  name = "AVÖ 1996R female, group",
  ages = avoe1996r.data.f$x, baseYear = 1991,
  deathProbs = avoe1996r.data.f$`qx(1991)` * avoe1996r.data.f$`Abschlag "Gruppen"`,
  trend = avoe1996r.data.f$`lx(l)`,
  trend2 = avoe1996r.data.f$`lx(k)`,
  dampingFunction = AVOe1996R.trend.switching,
  data = list(
    dim = list(table = "AVÖ 1996R", sex = "w", collar = "Gruppenrententafel", type = "Rententafel", country = "Österreich", data = "official", year = 1991)
  )
);

AVOe1996R.male.av = mortalityTable.ageShift(
  name = "AVÖ 1996R M mit Altersverschiebung (3,25%)",
  ages = ages(AVOe1996R.male),
  deathProbs = deathProbabilities(AVOe1996R.male, YOB = 1950),
  ageShifts = generateAgeShift(initial = 2, YOBs = c(1908, 1922, 1944, 1958, 1974, 1990, 2006, NA)),
  data = list(
    dim = list(table = "AVÖ 1996R", sex = "m", collar = "Rententafel", type = "Rententafel", country = "Österreich", data = "Altersverschiebung", year = "AVÖ 1996-R")
  )
)
AVOe1996R.female.av = mortalityTable.ageShift(
  name = "AVÖ 1996R F Gruppe mit Altersverschiebung (3,25%)",
  ages = ages(AVOe1996R.female),
  deathProbs = deathProbabilities(AVOe1996R.female, YOB = 1950),
  ageShifts = generateAgeShift(initial = 2, YOBs = c(1900, 1932, 1945, 1956, 1968, 1981, 1994, 2008, NA)),
  data = list(
    dim = list(table = "AVÖ 1996R", sex = "w", collar = "Rententafel", type = "Rententafel", country = "Österreich", data = "Altersverschiebung", year = "AVÖ 1996-R")
  )
)

AVOe1996R.male.group.av = mortalityTable.ageShift(
  name = "AVÖ 1996R M mit Altersverschiebung (3,25%)",
  ages = ages(AVOe1996R.male.group),
  deathProbs = deathProbabilities(AVOe1996R.male.group, YOB = 1950),
  ageShifts = generateAgeShift(initial = 2, YOBs = c(1906, 1923, 1944, 1958, 1973, 1989, 2005, NA)),
  data = list(
    dim = list(table = "AVÖ 1996R", sex = "m", collar = "Gruppenrententafel", type = "Rententafel", country = "Österreich", data = "Altersverschiebung", year = "AVÖ 1996-R")
  )
)
AVOe1996R.female.group.av = mortalityTable.ageShift(
  name = "AVÖ 1996R F Gruppe mit Altersverschiebung (3,25%)",
  ages = ages(AVOe1996R.female.group),
  deathProbs = deathProbabilities(AVOe1996R.female.group, YOB = 1950),
  ageShifts = generateAgeShift(initial = 2, YOBs = c(1900, 1933, 1945, 1956, 1968, 1980, 1993, 2006, NA)),
  data = list(
    dim = list(table = "AVÖ 1996R", sex = "w", collar = "Gruppenrententafel", type = "Rententafel", country = "Österreich", data = "Altersverschiebung", year = "AVÖ 1996-R")
  )
)


AVOe1996R = array(
  data = c(mortalityTable.NA),
  dim = c(2, 4),
  dimnames = list(Geschlecht = c("m", "w"), Collar = c("Einzel", "Gruppe", "AV", "AV Gruppe"))
)

AVOe1996R[["m", "Einzel"]] = AVOe1996R.male
AVOe1996R[["w", "Einzel"]] = AVOe1996R.female

AVOe1996R[["m", "Gruppe"]] = AVOe1996R.male.group
AVOe1996R[["w", "Gruppe"]] = AVOe1996R.female.group
AVOe1996R[["m", "AV"]] = AVOe1996R.male.av
AVOe1996R[["w", "AV"]] = AVOe1996R.female.av
AVOe1996R[["m", "AV Gruppe"]] = AVOe1996R.male.group.av
AVOe1996R[["w", "AV Gruppe"]] = AVOe1996R.female.group.av

save(
  AVOe1996R,
  AVOe1996R.male, AVOe1996R.female,
  AVOe1996R.male.group, AVOe1996R.female.group,
  AVOe1996R.male.av, AVOe1996R.female.av,
  AVOe1996R.male.group.av, AVOe1996R.female.group.av,

  file = avoe1996Rfile.out
)






#############################################################################h#
# AVÖ 2005R exact (Male, Female, unisex)                                   ####
# gender-specific tables also have 2nd-order tables, unisex only 1st-order table
#############################################################################h#

avoe2005Rfile = here::here("data-raw", "AT", "Annuities", "AVOe2005R.xlsx")
avoe2005RUfile = here::here("data-raw", "AT", "Annuities", "AVOe2005R unisex Endtafel.xlsx")
avoe2005Rfile.out = here::here("data", "AVOe2005R.RData")
avoe2005R.nodamping.file.out = here::here("data", "AVOe2005R.nodamping.RData")

avoe2005r.data = full_join(
  readxl::read_excel(
    avoe2005Rfile, sheet = "AVÖ 2005R exakt", skip = 4, n_max = 121,
    col_types = c(rep("guess", 7), rep("skip", 13)),
    col_names = c("x", "qx", "qx Gruppe", "trendM", "qy", "qy Gruppe", "trendF")
  ),
  readxl::read_excel(
    avoe2005RUfile, sheet = "AVÖ 2005R unisex exakt", skip = 4, n_max = 121,
    col_types = c(rep("guess", 4), rep("skip", 9)),
    col_names = c("x", "qu", "qu Gruppe", "trendU")
  ),
  by = "x"
)

avoe2005r.data.2Ord = readxl::read_excel(
  avoe2005Rfile, sheet = "AVÖ 2005R 2nd Ord", skip = 4, n_max = 121,
  col_types = c(rep("guess", 7), rep("skip", 13)),
  col_names = c("x", "qx", "qx Gruppe", "trendM", "qy", "qy Gruppe", "trendF")
)


############################################################################## #

AVOe2005R.trend.damping = function(t) {
  100*atan(t/100)
}
AVOe2005R_gen = function(df, nm, probs, trend, sex = "m", data = "loaded", type = "Rententafel") {
    mortalityTable.trendProjection(
      name = nm,
      ages = df$x,
      baseYear = 2001,
      deathProbs = df[[probs]],
      trend = df[[trend]],
      dampingFunction = AVOe2005R.trend.damping,
      data = list(
        dim = list(table = "AVÖ 2005-R", sex = sex, collar = "Rententafel", type = type, country = "Österreich", data = data, year = 2001, table = "AVÖ 2005-R")
      )
  )
}

AVOe2005R.male            = AVOe2005R_gen(avoe2005r.data, "AVÖ 2005R male (exact), loaded",   "qx", "trendM", sex = "m", data = "loaded");
AVOe2005R.female          = AVOe2005R_gen(avoe2005r.data, "AVÖ 2005R female (exact), loaded", "qy", "trendF", sex = "w", data = "loaded");
AVOe2005R.unisex          = AVOe2005R_gen(avoe2005r.data, "AVÖ 2005R unisex (exact), loaded", "qu", "trendU", sex = "u", data = "loaded");
AVOe2005R.male.unloaded   = AVOe2005R_gen(avoe2005r.data.2Ord, "AVÖ 2005R male (exact), unloaded",   "qx", "trendM", sex = "m", data = "unloaded");
AVOe2005R.female.unloaded = AVOe2005R_gen(avoe2005r.data.2Ord, "AVÖ 2005R female (exact), unloaded", "qy", "trendF", sex = "w", data = "unloaded");
AVOe2005R.male.group      = AVOe2005R_gen(avoe2005r.data, "AVÖ 2005R male group (exact), loaded",   "qx Gruppe", "trendM", sex = "m", data = "loaded, group", type = "Gruppenrententafel");
AVOe2005R.female.group    = AVOe2005R_gen(avoe2005r.data, "AVÖ 2005R female group (exact), loaded", "qy Gruppe", "trendF", sex = "w", data = "loaded, group", type = "Gruppenrententafel");
AVOe2005R.unisex.group    = AVOe2005R_gen(avoe2005r.data, "AVÖ 2005R unisex group (exact), loaded", "qu Gruppe", "trendU", sex = "u", data = "loaded, group", type = "Gruppenrententafel");
AVOe2005R.male.group.unloaded   = AVOe2005R_gen(avoe2005r.data.2Ord, "AVÖ 2005R male group (exact), loaded",   "qx Gruppe", "trendM", sex = "m", data = "loaded, group", type = "Gruppenrententafel");
AVOe2005R.female.group.unloaded = AVOe2005R_gen(avoe2005r.data.2Ord, "AVÖ 2005R female group (exact), loaded", "qy Gruppe", "trendF", sex = "w", data = "loaded, group", type = "Gruppenrententafel");

AVOe2005R = array(
  data = c(mortalityTable.NA),
  dim = c(3, 2, 2),
  dimnames = list(Geschlecht = c("m", "w", "u"), Collar = c("Einzel", "Gruppe"), Type = c("loaded", "unloaded"))
)

AVOe2005R[["m", "Einzel", "loaded"]] =   AVOe2005R.male
AVOe2005R[["w", "Einzel", "loaded"]] =   AVOe2005R.female
AVOe2005R[["u", "Einzel", "loaded"]] =   AVOe2005R.unisex
AVOe2005R[["m", "Gruppe", "loaded"]] =   AVOe2005R.male.group
AVOe2005R[["w", "Gruppe", "loaded"]] =   AVOe2005R.female.group
AVOe2005R[["u", "Gruppe", "loaded"]] =   AVOe2005R.unisex.group

AVOe2005R[,, "unloaded"] = NA
AVOe2005R[["m", "Einzel", "unloaded"]] = AVOe2005R.male.unloaded
AVOe2005R[["w", "Einzel", "unloaded"]] = AVOe2005R.female.unloaded
AVOe2005R[["m", "Gruppe", "unloaded"]] = AVOe2005R.male.group.unloaded
AVOe2005R[["w", "Gruppe", "unloaded"]] = AVOe2005R.female.group.unloaded

save(
  AVOe2005R,
  AVOe2005R.male, AVOe2005R.female, AVOe2005R.unisex,
  AVOe2005R.male.unloaded, AVOe2005R.female.unloaded,
  AVOe2005R.male.group, AVOe2005R.female.group, AVOe2005R.unisex.group,
  AVOe2005R.male.group.unloaded, AVOe2005R.female.group.unloaded,
  file = avoe2005Rfile.out
)




AVOe2005R.male.nodamping            = mT.setDimInfo(undampenTrend(AVOe2005R.male), data = "loaded, no trend damping")
AVOe2005R.female.nodamping          = mT.setDimInfo(undampenTrend(AVOe2005R.female), data = "loaded, no trend damping");
AVOe2005R.unisex.nodamping          = mT.setDimInfo(undampenTrend(AVOe2005R.unisex), data = "loaded, no trend damping");
AVOe2005R.male.nodamping.unloaded   = mT.setDimInfo(undampenTrend(AVOe2005R.male.unloaded), data = "unloaded, no trend damping");
AVOe2005R.female.nodamping.unloaded = mT.setDimInfo(undampenTrend(AVOe2005R.female.unloaded), data = "unloaded, no trend damping");
AVOe2005R.male.nodamping.group      = mT.setDimInfo(undampenTrend(AVOe2005R.male.group), data = "loaded, group, no trend damping");
AVOe2005R.female.nodamping.group    = mT.setDimInfo(undampenTrend(AVOe2005R.female.group), data = "loaded, group, no trend damping");
AVOe2005R.unisex.nodamping.group    = mT.setDimInfo(undampenTrend(AVOe2005R.unisex.group), data = "loaded, group, no trend damping");
AVOe2005R.male.nodamping.group.unloaded      = mT.setDimInfo(undampenTrend(AVOe2005R.male.group.unloaded), data = "unloaded, group, no trend damping");
AVOe2005R.female.nodamping.group.unloaded    = mT.setDimInfo(undampenTrend(AVOe2005R.female.group.unloaded), data = "unloaded, group, no trend damping");


AVOe2005R.nodamping = array(
  data = c(mortalityTable.NA),
  dim = c(3, 2, 2),
  dimnames = list(Geschlecht = c("m", "w", "u"), Collar = c("Einzel", "Gruppe"), Type = c("loaded", "unloaded"))
)

AVOe2005R.nodamping[["m", "Einzel", "loaded"]] =   AVOe2005R.male.nodamping
AVOe2005R.nodamping[["w", "Einzel", "loaded"]] =   AVOe2005R.female.nodamping
AVOe2005R.nodamping[["u", "Einzel", "loaded"]] =   AVOe2005R.unisex.nodamping
AVOe2005R.nodamping[["m", "Gruppe", "loaded"]] =   AVOe2005R.male.nodamping.group
AVOe2005R.nodamping[["w", "Gruppe", "loaded"]] =   AVOe2005R.female.nodamping.group
AVOe2005R.nodamping[["u", "Gruppe", "loaded"]] =   AVOe2005R.unisex.nodamping.group

AVOe2005R.nodamping[, , "unloaded"] = NA
AVOe2005R.nodamping[["m", "Einzel", "unloaded"]] = AVOe2005R.male.nodamping.unloaded
AVOe2005R.nodamping[["w", "Einzel", "unloaded"]] = AVOe2005R.female.nodamping.unloaded
AVOe2005R.nodamping[["m", "Gruppe", "unloaded"]] = AVOe2005R.male.nodamping.group.unloaded
AVOe2005R.nodamping[["w", "Gruppe", "unloaded"]] = AVOe2005R.female.nodamping.group.unloaded


save(
  AVOe2005R.nodamping,
  AVOe2005R.male.nodamping,
  AVOe2005R.female.nodamping,
  AVOe2005R.unisex.nodamping,
  AVOe2005R.male.nodamping.unloaded,
  AVOe2005R.female.nodamping.unloaded,
  AVOe2005R.male.nodamping.group,
  AVOe2005R.female.nodamping.group,
  AVOe2005R.unisex.nodamping.group,
  AVOe2005R.male.nodamping.group.unloaded,
  AVOe2005R.female.nodamping.group.unloaded,

  file = avoe2005R.nodamping.file.out
)


##############################################################################h#
# AVÖ 2005R with age-shifting (Male, Female, unisex), 1st-order only        ####
##############################################################################h#

avoe2005R.av.file.out = here::here("data", "AVOe2005R.av.RData")

avoe2005rAV.base = full_join(
  readxl::read_excel(
    avoe2005Rfile, sheet = "AVÖ 2005R mit AV", skip = 13, n_max = 121,
    col_types = c(rep("guess", 5), rep("skip", 10)),
    col_names = c("x", "qx", "qx Gruppe", "qy", "qy Gruppe")
  ),
  readxl::read_excel(
    avoe2005RUfile, sheet = "AVÖ 2005R unisex mit AV", skip = 4, n_max = 121,
    col_types = c(rep("guess", 3), rep("skip", 4)),
    col_names = c("x", "qu", "qu Gruppe")
  ),
  by = "x"
)
avoe2005rAV.shift = full_join(
  readxl::read_excel(
    avoe2005Rfile, sheet = "AVÖ 2005R mit AV", skip = 13, n_max = 116,
    col_types = c(rep("skip", 6), "guess", rep(c("skip", "guess"), 4)),
    col_names = c("YOB", "shiftM", "shiftMG", "shiftF", "shiftFG")
  ),
  readxl::read_excel(
    avoe2005RUfile, sheet = "AVÖ 2005R unisex mit AV", skip = 4, n_max = 116,
    col_types = c(rep("skip", 4), rep("guess", 3)),
    col_names = c("YOB", "shiftU", "shiftUG")
  ),
  by = "YOB"
)

AVOe2005R_gen.av = function(nm, probs, shft, sex = "m", data = "age-shift, loaded", type = "Rententafel", year = 1965) {
  mortalityTable.ageShift(
    name = nm,
    ages = avoe2005rAV.base$x,
    deathProbs = avoe2005rAV.base[[probs]],
    ageShifts = generateAgeShiftFromTable(avoe2005rAV.shift %>% mutate(shift = .[[shft]])),
    baseYear = year,
    data = list(
      dim = list(table = "AVÖ 2005-R AV", sex = sex, collar = "Rententafel", type = "Rententafel", country = "Österreich", data = data, year = year)
    )
  )
}

AVOe2005R.male.av = AVOe2005R_gen.av(
  "AVÖ 2005R male (age-shifted), loaded",
  "qx", "shiftM", sex = "m", data = "age-shift, loaded");
AVOe2005R.female.av = AVOe2005R_gen.av(
  "AVÖ 2005R female (age-shifted), loaded",
  "qy", "shiftF", sex = "w", data = "age-shift, loaded");
AVOe2005R.unisex.av = AVOe2005R_gen.av(
  "AVÖ 2005R unisex (age-shifted), loaded",
  "qu", "shiftU", sex = "u", data = "age-shift, loaded", year = 1972);
AVOe2005R.male.group.av = AVOe2005R_gen.av(
  "AVÖ 2005R male group (age-shifted), loaded",
  "qx Gruppe", "shiftMG", sex = "m", data = "age-shift, loaded, group", type = "Gruppenrententafel");
AVOe2005R.female.group.av = AVOe2005R_gen.av(
  "AVÖ 2005R female group (age-shifted), loaded",
  "qy Gruppe", "shiftFG", sex = "w", data = "age-shift, loaded, group", type = "Gruppenrententafel");
AVOe2005R.unisex.group.av = AVOe2005R_gen.av(
  "AVÖ 2005R unisex group (age-shifted), loaded",
  "qu Gruppe", "shiftUG", sex = "u", data = "age-shift, loaded, group",
  type = "Gruppenrententafel", year = 1972)


AVOe2005R.av = array(
  data = c(mortalityTable.NA),
  dim = c(3, 2),
  dimnames = list(Geschlecht = c("m", "w", "u"), Collar = c("Einzel", "Gruppe"))
)

AVOe2005R.av[["m", "Einzel"]] = AVOe2005R.male.av
AVOe2005R.av[["w", "Einzel"]] = AVOe2005R.female.av
AVOe2005R.av[["u", "Einzel"]] = AVOe2005R.unisex.av
AVOe2005R.av[["m", "Gruppe"]] = AVOe2005R.male.group.av
AVOe2005R.av[["w", "Gruppe"]] = AVOe2005R.female.group.av
AVOe2005R.av[["u", "Gruppe"]] = AVOe2005R.unisex.group.av



save(
  AVOe2005R.av,
  AVOe2005R.male.av,
  AVOe2005R.female.av,
  AVOe2005R.unisex.av,
  AVOe2005R.male.group.av,
  AVOe2005R.female.group.av,
  AVOe2005R.unisex.group.av,

  file = avoe2005R.av.file.out
)


ages = 0:120
data.frame(ages = ages,
           m = deathProbabilities(AVOe2005R[["m", "Einzel", "loaded"]], YOB = 1930),
           w = deathProbabilities(AVOe2005R[["w", "Einzel", "loaded"]], YOB = 1930),
           mG = deathProbabilities(AVOe2005R[["m", "Gruppe", "loaded"]], YOB = 1930),
           wG = deathProbabilities(AVOe2005R[["w", "Gruppe", "loaded"]], YOB = 1930),
           m.2Ord = deathProbabilities(AVOe2005R[["m", "Einzel", "unloaded"]], YOB = 1930),
           w.2Ord = deathProbabilities(AVOe2005R[["w", "Einzel", "unloaded"]], YOB = 1930),
           mG.2Ord = deathProbabilities(AVOe2005R[["m", "Gruppe", "unloaded"]], YOB = 1930),
           wG.2Ord = deathProbabilities(AVOe2005R[["w", "Gruppe", "unloaded"]], YOB = 1930)
)


