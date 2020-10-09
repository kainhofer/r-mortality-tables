#############################################################################m#
#  Skript to generate German death risk mortality table objects            ####
#############################################################################m#
library(MortalityTables)
library(here)
library(readxl)
library(dplyr)




#############################################################################h#
### DAV 1994 T                                                             ####
#############################################################################h#

dav1994T.file = here::here("data-raw", "DE", "DeathRisk", "DAV_1994T.xls")
dav1994T.file.out = here::here("data", "DAV1994T.RData")


dav1994T.data = readxl::read_excel(dav1994T.file, sheet = 1, skip = 2,
                           col_types = c("guess", rep("skip", 3), "guess", "guess", rep("skip", 8), "guess", "guess", rep("skip", 4)),
                           col_names = c("x", "qx.2Ord", "qx", "qy.2Ord", "qy")) %>%
  dplyr::mutate(qx = qx/1000, qy = qy/1000, qx.2Ord = qx.2Ord/1000, qy.2Ord = qy.2Ord/1000)

createDAV1994T = function(name, ages, deathProbs, sex = "m", data = "loaded") {
  mortalityTable.period(
    name = name,
    ages = ages,
    deathProbs = deathProbs,
    baseYear = 1994,
    data = list(
      dim = list(table = "DAV 1994T", sex = sex, collar = "Aggregat", type = "Risikosterbetafel", country = "Deutschland", data = data, year = 1994)
    )
  )
}

DAV1994T.male = createDAV1994T("DAV 1994T Männer", dav1994T.data$x, dav1994T.data$qx, "m", "loaded")
DAV1994T.female = createDAV1994T("DAV 1994T Frauen", dav1994T.data$x, dav1994T.data$qy, "w", "loaded")
DAV1994T.male.2Ord = createDAV1994T("DAV 1994T Männer 2.Ordnung", dav1994T.data$x, dav1994T.data$qx.2Ord, "m", "2.Ordnung")
DAV1994T.female.2Ord = createDAV1994T("DAV 1994T Frauen 2.Ordnung", dav1994T.data$x, dav1994T.data$qy.2Ord, "w", "2.Ordnung")

DAV1994T = array(
  data = c(DAV1994T.male, DAV1994T.female),
  dim = c(2),
  dimnames = list(Geschlecht = c("m", "w"))
)

DAV1994T.2Ord = array(
  data = c(DAV1994T.male.2Ord, DAV1994T.female.2Ord),
  dim = c(2),
  dimnames = list(Geschlecht = c("m", "w"))
)

save(
  DAV1994T,
  DAV1994T.2Ord,
  DAV1994T.male, DAV1994T.female,
  DAV1994T.male.2Ord, DAV1994T.female.2Ord,
  file = dav1994T.file.out
)




#############################################################################h#
# DAV 2008T exact (Male, Frauen, unisex)                                   ####
# gender-specific tables also have 2nd-order tables, unisex only 1st-order table
#############################################################################h#

DAV2008Tfile = here::here("data-raw", "DE", "DeathRisk", "DAV2008T Aggregat-Raucher-Nichtraucher.xlsx")
DAV2008Tfile.out = here::here("data", "DAV2008T.RData")


DAV2008T.data = dplyr::full_join(
  readxl::read_excel(
    DAV2008Tfile, sheet = "DAV 2008T R-NR Männer", skip = 5,
    col_types = c("guess", rep("skip", 3), rep("guess", 6)),
    col_names = c(
      "x",
      "qx.2Ord", "qx.NR.2Ord", "qx.R.2Ord",
      "qx", "qx.NR", "qx.R"
    )
  ),
  readxl::read_excel(
    DAV2008Tfile, sheet = "DAV 2008T R-NR Frauen", skip = 5,
    col_types = c("guess", rep("skip", 3), rep("guess", 6)),
    col_names = c(
      "x",
      "qy.2Ord", "qy.NR.2Ord", "qy.R.2Ord",
      "qy", "qy.NR", "qy.R"
    )
  ),
  by = "x"
)

genDAV2008T = function(tbl, name, col, collar, sex, data) {
  mortalityTable.period(
    name = name,
    ages = getElement(tbl, "x"),
    deathProbs = getElement(tbl, col),
    baseYear = 2008,
    data = list(
      dim = list(table = "DAV 1994T", sex = sex, collar = collar, type = "Risikosterbetafel", country = "Deutschland", data = data, year = 2008)
    )
  )
}

DAV2008T.male = genDAV2008T(DAV2008T.data, "DAV 2008T Männer", "qx", "m", "Aggregat", "loaded")
DAV2008T.male.2Ord = genDAV2008T(DAV2008T.data, "DAV 2008T Männer 2.Ordnung", "qx.2Ord", "m", "Aggregat", "unloaded")
DAV2008T.female = genDAV2008T(DAV2008T.data, "DAV 2008T Frauen", "qy", "w", "Aggregat", "loaded")
DAV2008T.female.2Ord = genDAV2008T(DAV2008T.data, "DAV 2008T Frauen 2.Ordnung", "qy.2Ord", "w", "Aggregat", "unloaded")

DAV2008T.male.smoker = genDAV2008T(DAV2008T.data, "DAV 2008T Männer Raucher", "qx.R", "m", "Raucher", "loaded")
DAV2008T.male.smoker.2Ord = genDAV2008T(DAV2008T.data, "DAV 2008T Männer Raucher, 2.Ordnung", "qx.R.2Ord", "m", "Raucher", "unloaded")
DAV2008T.female.smoker = genDAV2008T(DAV2008T.data, "DAV 2008T Frauen Raucher", "qy.R", "w", "Raucher", "loaded")
DAV2008T.female.smoker.2Ord = genDAV2008T(DAV2008T.data, "DAV 2008T Frauen Raucher, 2.Ordnung", "qy.R.2Ord", "w", "Raucher", "unloaded")

DAV2008T.male.nonsmoker = genDAV2008T(DAV2008T.data, "DAV 2008T Männer Nichtraucher", "qx.NR", "m", "Nichtraucher", "loaded")
DAV2008T.male.nonsmoker.2Ord = genDAV2008T(DAV2008T.data, "DAV 2008T Männer Nichtraucher, 2.Ordnung", "qx.NR.2Ord", "m", "Nichtraucher", "unloaded")
DAV2008T.female.nonsmoker = genDAV2008T(DAV2008T.data, "DAV 2008T Frauen Nichtraucher", "qy.NR", "w", "Nichtraucher", "loaded")
DAV2008T.female.nonsmoker.2Ord = genDAV2008T(DAV2008T.data, "DAV 2008T Frauen Nichtraucher, 2.Ordnung", "qy.NR.2Ord", "w", "Nichtraucher", "unloaded")

DAV2008T = array(
  data = c(DAV2008T.male, DAV2008T.female, DAV2008T.male.2Ord, DAV2008T.female.2Ord,
           DAV2008T.male.nonsmoker, DAV2008T.female.nonsmoker, DAV2008T.male.nonsmoker.2Ord, DAV2008T.female.nonsmoker.2Ord,
           DAV2008T.male.smoker, DAV2008T.female.smoker, DAV2008T.male.smoker.2Ord, DAV2008T.female.smoker.2Ord
  ),
  dim = c(2, 2, 3),
  dimnames = list(Geschlecht = c("m", "w"), Tafel = c("1. Ordnung", "2. Ordnung"), Typ = c("Aggregat", "Nichtraucher", "Raucher"))
)

save(
  DAV2008T,
  DAV2008T.male, DAV2008T.female, DAV2008T.male.2Ord, DAV2008T.female.2Ord,
  DAV2008T.male.smoker, DAV2008T.female.smoker, DAV2008T.male.smoker.2Ord, DAV2008T.female.smoker.2Ord,
  DAV2008T.male.nonsmoker, DAV2008T.female.nonsmoker, DAV2008T.male.nonsmoker.2Ord, DAV2008T.female.nonsmoker.2Ord,
  file = DAV2008Tfile.out
)

