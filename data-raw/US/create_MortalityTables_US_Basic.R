################################################################################m#
#  Skript to generate US Basic (select and ultimate) valuation table objects  ####
################################################################################m#
library(MortalityTables)
library(here)
library(readxl)
library(dplyr)
library(purrr)

fillMissingAges = function(vect) {
  # Walk forward one element after the other and replace all NA with previous+1
  # Handles all trailing NA (and possibly some intermediate NA, which should not occur)
  vect = Reduce(
    function(o,n) {ifelse(is.na(n) && !is.na(o), o+1, n)},
    vect, init = NA, accumulate = TRUE) %>%
    tail(-1)
  # Do the same backwards to replace leading NA
  vect = Reduce(
    function(n, o) {ifelse(is.na(n) && !is.na(o), o-1, n)},
    vect, init = NA, right = TRUE, accumulate = TRUE) %>%
    head(-1)
  vect
}
createUSSelectTable = function(
  file, sheet = 1, skip = 0,
  age.col = "Initial Age", rm.cols = c("Attained Age"),
  name, year = 2020, scale = 1,
  table, type = "CSO", country = "USA", ...
) {
  data = readxl::read_excel(file, sheet = sheet, skip = skip)
  # Remove all rows that contain ONLY NA (i.e. spacer lines in the table)
  # Taken from https://stackoverflow.com/a/55309831
# browser()
  data = data %>%
    filter_all(any_vars(!is.na(.)))
  # Fill NA ages if ultimate table is longer than select
  ages = fillMissingAges(getElement(data, age.col))
  qx = data %>%
    dplyr::select(-all_of(age.col), -all_of(rm.cols))

  mortalityTable.period(
    name = name, ages = ages, deathProbs = as.matrix(scale * qx), baseYear = year,
    selectInitialAge = TRUE,
    data = list(
      dim = list(table = table, type = type, country = country, data = "official", year = year, ...)
    )
  )
}





#############################################################################h#
# USA 1925-39 Basic Select and Ultimate Tables                             ####
#############################################################################h#

basic1925file = here::here("data-raw", "US", "Basic_Select-Ultimate", "USA_1925-39_BasicTable.xlsx")
basic1925file.out = here::here("data", "US.1925.39.Basic.RData")


US.1925.39.Basic = createUSSelectTable(
  basic1925file, sheet = 1, skip = 3, age.col = "Initial Age", rm.cols = NULL,
  name = "USA 1925-39 Basic Table", year = 1939,
  table = "1925-39 Basic Table", sex = "u", ageType = "ANB", type = "Basic Select", country = "USA")

save(
  US.1925.39.Basic,
  file = basic1925file.out
)



#############################################################################h#
# USA 1946-49 Basic Select and Ultimate Tables                             ####
#############################################################################h#

basic1946file = here::here("data-raw", "US", "Basic_Select-Ultimate", "USA_1946-49_Basic_Select_Ultimate.xlsx")
basic1946file.out = here::here("data", "US.1946.49.Basic.RData")


USA.1946.49.Basic = createUSSelectTable(
  basic1946file, sheet = 1, skip = 3, scale = 0.001,
  name = "USA 1946-49 Basic Table", year = 1949,
  table = "1946-49 Basic Table", sex = "u", ageType = "ANB", type = "Basic Select-Ultimate", country = "USA")

save(
  USA.1946.49.Basic,
  file = basic1946file.out
)




#############################################################################h#
# USA 1955-60 Basic Select and Ultimate Tables                             ####
#############################################################################h#

basic1955file = here::here("data-raw", "US", "Basic_Select-Ultimate", "USA_1955-60_Basic_Select_Ultimate.xlsx")
basic1955file.out = here::here("data", "US.1955.60.Basic.RData")


USA.1955.60.Basic.U = createUSSelectTable(
  basic1955file, sheet = 1, skip = 3, scale = 0.001,
  name = "USA 1955-60 Basic Table Unisex", year = 1960,
  table = "1955-60 Basic Table", sex = "u", ageType = "ANB", type = "Basic Select-Ultimate", country = "USA")

USA.1955.60.Basic.M = createUSSelectTable(
  basic1955file, sheet = 2, skip = 3, scale = 0.001,
  name = "USA 1955-60 Basic Table Males", year = 1960,
  table = "1955-60 Basic Table", sex = "m", ageType = "ANB", type = "Basic Select-Ultimate", country = "USA")

USA.1955.60.Basic.F = createUSSelectTable(
  basic1955file, sheet = 3, skip = 3, scale = 0.001,
  name = "USA 1955-60 Basic Table Females", year = 1960,
  table = "1955-60 Basic Table", sex = "f", ageType = "ANB", type = "Basic Select-Ultimate", country = "USA")

USA.1955.60.Basic = array(
  data = c(
    USA.1955.60.Basic.U, USA.1955.60.Basic.M, USA.1955.60.Basic.F
  ),
  dim = c(3),
  dimnames = list(Sex = c("u", "m", "f"))
)

save(
  USA.1955.60.Basic,
  USA.1955.60.Basic.U, USA.1955.60.Basic.M, USA.1955.60.Basic.F,
  file = basic1955file.out
)




#############################################################################h#
# USA 1965-70 Basic Select and Ultimate Tables                             ####
#############################################################################h#

basic1965file = here::here("data-raw", "US", "Basic_Select-Ultimate", "USA_1965-70_Basic_Select_Ultimate.xlsx")
basic1965file.out = here::here("data", "US.1965.70.Basic.RData")


USA.1965.70.Basic.M.ANB = createUSSelectTable(
  basic1965file, sheet = 1, skip = 3,
  name = "USA 1965-70 Basic Table Male ANB", year = 1970,
  table = "1965-70 Basic Table", sex = "m", ageType = "ANB", type = "Basic Select-Ultimate", country = "USA")

USA.1965.70.Basic.F.ANB = createUSSelectTable(
  basic1965file, sheet = 2, skip = 3,
  name = "USA 1965-70 Basic Table Female ANB", year = 1970,
  table = "1965-70 Basic Table", sex = "f", ageType = "ANB", type = "Basic Select-Ultimate", country = "USA")

USA.1965.70.Basic.M.ALB = createUSSelectTable(
  basic1965file, sheet = 3, skip = 3,
  name = "USA 1965-70 (Modified) Basic Table Male ALB", year = 1970,
  table = "1965-70 Basic Table", sex = "m", ageType = "ALB", type = "Basic Select-Ultimate", country = "USA")

USA.1965.70.Basic.F.ALB = createUSSelectTable(
  basic1965file, sheet = 4, skip = 3,
  name = "USA 1965-70 (Modified) Basic Table Female ALB", year = 1970,
  table = "1965-70 Basic Table", sex = "f", ageType = "ALB", type = "Basic Select-Ultimate", country = "USA")


USA.1965.70.Basic = array(
  data = c(
    USA.1965.70.Basic.M.ANB, USA.1965.70.Basic.F.ANB,
    USA.1965.70.Basic.M.ALB, USA.1965.70.Basic.F.ALB
  ),
  dim = c(2, 2),
  dimnames = list(Sex = c("m", "f"), age = c("ANB", "ALB"))
)



save(
  USA.1965.70.Basic,
  USA.1965.70.Basic.M.ANB, USA.1965.70.Basic.F.ANB,
  USA.1965.70.Basic.M.ALB, USA.1965.70.Basic.F.ALB,
  file = basic1965file.out
)





#############################################################################h#
# USA 1975-80 Basic Select and Ultimate Tables                             ####
#############################################################################h#

basic1975file = here::here("data-raw", "US", "Basic_Select-Ultimate", "USA_1975-80_Basic_Select_Ultimate.xlsx")
basic1975file.out = here::here("data", "US.1975.80.Basic.RData")


USA.1975.80.Basic.M.ANB = createUSSelectTable(
  basic1975file, sheet = 1, skip = 3, scale = 0.001,
  name = "USA 1975-80 Basic Table Male ANB", year = 1980,
  table = "1975-80 Basic Table", sex = "m", ageType = "ANB", type = "Basic Select-Ultimate", country = "USA")

USA.1975.80.Basic.F.ANB = createUSSelectTable(
  basic1975file, sheet = 2, skip = 3, scale = 0.001,
  name = "USA 1975-80 Basic Table Female ANB", year = 1980,
  table = "1975-80 Basic Table", sex = "f", ageType = "ANB", type = "Basic Select-Ultimate", country = "USA")

USA.1975.80.Basic.M.ALB = createUSSelectTable(
  basic1975file, sheet = 3, skip = 3, scale = 0.001,
  name = "USA 1975-80 Basic Table Male ALB", year = 1980,
  table = "1975-80 Basic Table", sex = "m", ageType = "ALB", type = "Basic Select-Ultimate", country = "USA")

USA.1975.80.Basic.F.ALB = createUSSelectTable(
  basic1975file, sheet = 4, skip = 3, scale = 0.001,
  name = "USA 1975-80 Basic Table Female ALB", year = 1980,
  table = "1975-80 Basic Table", sex = "f", ageType = "ALB", type = "Basic Select-Ultimate", country = "USA")


USA.1975.80.Basic = array(
  data = c(
    USA.1975.80.Basic.M.ANB, USA.1975.80.Basic.F.ANB,
    USA.1975.80.Basic.M.ALB, USA.1975.80.Basic.F.ALB
  ),
  dim = c(2, 2),
  dimnames = list(Sex = c("m", "f"), age = c("ANB", "ALB"))
)



save(
  USA.1975.80.Basic,
  USA.1975.80.Basic.M.ANB, USA.1975.80.Basic.F.ANB,
  USA.1975.80.Basic.M.ALB, USA.1975.80.Basic.F.ALB,
  file = basic1975file.out
)





#############################################################################h#
# USA 1985-90 Basic Select and Ultimate Tables                             ####
#############################################################################h#

basic1985file = here::here("data-raw", "US", "Basic_Select-Ultimate", "MFlb85-90.xls")
basic1985file.out = here::here("data", "US.1985.90.Basic.RData")


USA.1985.90.Basic.M.ANB = createUSSelectTable(
  here::here("data-raw", "US", "Basic_Select-Ultimate", "Mnb85-90.xls"), sheet = 1, skip = 1, scale = 0.001,
  age.col = "MNB", rm.cols = c("...27", "...29"),
  name = "USA 1985-90 Basic Table Male ANB", year = 1990,
  table = "1985-90 Basic Table", sex = "m", ageType = "ANB", type = "Basic Select-Ultimate", country = "USA")

USA.1985.90.Basic.F.ANB = createUSSelectTable(
  here::here("data-raw", "US", "Basic_Select-Ultimate", "Fnb85-90.xls"), sheet = 1, skip = 1, scale = 0.001,
  age.col = "FNB", rm.cols = c("...27", "Att. Age"),
  name = "USA 1985-90 Basic Table Female ANB", year = 1990,
  table = "1985-90 Basic Table", sex = "f", ageType = "ANB", type = "Basic Select-Ultimate", country = "USA")

USA.1985.90.Basic.M.ALB = createUSSelectTable(
  basic1985file, sheet = 1, skip = 1, scale = 0.001,
  age.col = "MLB", rm.cols = c("...27", "att. Age"),
  name = "USA 1985-90 Basic Table Male ALB", year = 1990,
  table = "1985-90 Basic Table", sex = "m", ageType = "ALB", type = "Basic Select-Ultimate", country = "USA")

USA.1985.90.Basic.F.ALB = createUSSelectTable(
  basic1985file, sheet = 2, skip = 1, scale = 0.001,
  age.col = "FLB", rm.cols = c("...27", "Att. Age"),
  name = "USA 1985-90 Basic Table Female ALB", year = 1990,
  table = "1985-90 Basic Table", sex = "f", ageType = "ALB", type = "Basic Select-Ultimate", country = "USA")


USA.1985.90.Basic = array(
  data = c(
    USA.1985.90.Basic.M.ANB, USA.1985.90.Basic.F.ANB,
    USA.1985.90.Basic.M.ALB, USA.1985.90.Basic.F.ALB
  ),
  dim = c(2, 2),
  dimnames = list(Sex = c("m", "f"), age = c("ANB", "ALB"))
)



save(
  USA.1985.90.Basic,
  USA.1985.90.Basic.M.ANB, USA.1985.90.Basic.F.ANB,
  USA.1985.90.Basic.M.ALB, USA.1985.90.Basic.F.ALB,
  file = basic1985file.out
)






#############################################################################h#
# USA 1990-95 Basic Select and Ultimate Tables                             ####
#############################################################################h#

basic1990file = here::here("data-raw", "US", "Basic_Select-Ultimate", "Final90-95.xls")
basic1990file.out = here::here("data", "US.1990.95.Basic.RData")

createUS1990SelectTable = function(
  file, sheet = 1, skip = 0,
  age.col = "Initial Age", rm.cols = c(),
  name, scale = 0.001, ...
) {
  data = map(sheet, function(s) readxl::read_excel(sheet = s, path = file, skip = skip) ) %>%
    bind_rows()
  # browser()
  # TODO: Fill ages if ultimate table is longer than select (use attained age of ultimate table to derive initial age)
  ages = getElement(data, age.col)
  qx = data %>%
    dplyr::select(-all_of(age.col), -all_of(rm.cols))

  mortalityTable.period(
    name = name, ages = ages, deathProbs = as.matrix(scale * qx), baseYear = 1995,
    selectInitialAge = TRUE,
    data = list(
      dim = list(table = "1990-95 Basic Table", type = "Basic Select-Ultimate", country = "USA", data = "official", year = 1995, ...)
    )
  )
}

USA.1990.95.Basic.M.ANB = createUS1990SelectTable(
  basic1990file, sheet = c("MNB Experience", "MNB Extrapolation"), scale = 0.001,
  age.col = "MNB", rm.cols = c("...27"),
  name = "USA 1990-95 Basic Table Male ANB",  sex = "m", ageType = "ANB")

USA.1990.95.Basic.F.ANB = createUS1990SelectTable(
  basic1990file, sheet = c("FNB Experience", "FNB Extrapolation"), scale = 0.001,
  age.col = "FNB", rm.cols = c("...27"),
  name = "USA 1990-95 Basic Table Female ANB",  sex = "f", ageType = "ANB")

USA.1990.95.Basic.M.ALB = createUS1990SelectTable(
  basic1990file, sheet = c("MLB Experience", "MLB Extrapolation"), scale = 0.001,
  age.col = "MLB", rm.cols = c("...27"),
  name = "USA 1990-95 Basic Table Male ALB",  sex = "m", ageType = "ALB")

USA.1990.95.Basic.F.ALB = createUS1990SelectTable(
  basic1990file, sheet = c("FLB Experience", "FLB Extrapolation"), scale = 0.001,
  age.col = "FLB", rm.cols = c("...27"),
  name = "USA 1990-95 Basic Table Female ALB",  sex = "f", ageType = "ALB")


USA.1990.95.Basic = array(
  data = c(
    USA.1990.95.Basic.M.ANB, USA.1990.95.Basic.F.ANB,
    USA.1990.95.Basic.M.ALB, USA.1990.95.Basic.F.ALB
  ),
  dim = c(2, 2),
  dimnames = list(Sex = c("m", "f"), age = c("ANB", "ALB"))
)



save(
  USA.1990.95.Basic,
  USA.1990.95.Basic.M.ANB, USA.1990.95.Basic.F.ANB,
  USA.1990.95.Basic.M.ALB, USA.1990.95.Basic.F.ALB,
  file = basic1990file.out
)


## Smoker / Non-Smoker / Band 1 / Band 2 / Band 3 variants

USA.1990.95.Basic.Breakdown = array(
  data = c(mortalityTable.NA),
  dim = c(2, 2, 5),
  dimnames = list(Sex = c("m", "f"), age = c("ANB", "ALB"), type = c("Non-Smoker", "Smoker", "Band1", "Band2", "Band3"))
)


load.factors.1990Basic = function(file, sheet, range) {
  read_excel(file, sheet = sheet, range = range) %>%
    as.data.frame() %>%
    as.matrix()
}

factors.1990Basic = array(
  data = 0,
  dim = c(53, 10,2,5),
  dimnames = list(Alter = 20:72, Duration = 1:10, Sex = c("m", "f"), type = c("Non-Smoker", "Smoker", "Band1", "Band2", "Band3"))
)

factors.1990Basic[,,"m", "Non-Smoker"] = load.factors.1990Basic(basic1990file, "NS", "B2:K55")
factors.1990Basic[,,"m", "Smoker"] = load.factors.1990Basic(basic1990file, "SM", "B2:K55")
factors.1990Basic[,,"m", "Band1"] = load.factors.1990Basic(basic1990file, "Band 1", "B3:K56")
factors.1990Basic[,,"m", "Band2"] = load.factors.1990Basic(basic1990file, "Band 2", "B3:K56")
factors.1990Basic[,,"m", "Band3"] = load.factors.1990Basic(basic1990file, "Band 3", "B3:K56")

factors.1990Basic[,,"f", "Non-Smoker"] = load.factors.1990Basic(basic1990file, "NS", "N2:W55")
factors.1990Basic[,,"f", "Smoker"] = load.factors.1990Basic(basic1990file, "SM", "N2:W55")
factors.1990Basic[,,"f", "Band1"] = load.factors.1990Basic(basic1990file, "Band 1", "N3:W56")
factors.1990Basic[,,"f", "Band2"] = load.factors.1990Basic(basic1990file, "Band 2", "N3:W56")
factors.1990Basic[,,"f", "Band3"] = load.factors.1990Basic(basic1990file, "Band 3", "N3:W56")


createUS1990SelectSubTable = function(data, variant = "Non-Smoker", name, scale = 0.001, sex, ...) {
  data.sm = data
  data.sm[,2:11] = data.sm[,2:11] * factors.1990Basic[,, sex, variant]

  qx = data.sm# convertUSSelectData(data.sm, ultimate_cols = c())
  ages = getElement(qx, 1)
  qx = qx %>%
    dplyr::select(-1)

  mortalityTable.period(
    name = paste(name, variant), ages = ages, deathProbs = as.matrix(scale * qx),
    baseYear = 1995,
    selectInitialAge = TRUE,
    data = list(
      dim = list(table = paste("1990-95 Basic Table", variant), sex = sex, type = "Basic Select-Ultimate", country = "USA", data = "official", year = 1995, collar = variant, ...)
    )
  )
}
createUS1990SelectTable.Breakdown = function(
  file, sheet = 1, name, scale = 0.001, sex = "m", ageType = "ANB", ...
) {
  data = readxl::read_excel(sheet = sheet, path = file, range = "A22:K74", col_names = c("Initial Age", as.character(1:10)))
  USA.1990.95.Basic.Breakdown[[sex, ageType, "Non-Smoker"]] <<- createUS1990SelectSubTable(data, "Non-Smoker", name = name, scale = scale, sex = sex, ageType = ageType, ...)
  USA.1990.95.Basic.Breakdown[[sex, ageType, "Smoker"]] <<- createUS1990SelectSubTable(data, "Smoker", name = name, scale = scale, sex = sex, ageType = ageType, ...)
  USA.1990.95.Basic.Breakdown[[sex, ageType, "Band1"]] <<- createUS1990SelectSubTable(data, "Band1", name = name, scale = scale, sex = sex, ageType = ageType, ...)
  USA.1990.95.Basic.Breakdown[[sex, ageType, "Band2"]] <<- createUS1990SelectSubTable(data, "Band2", name = name, scale = scale, sex = sex, ageType = ageType, ...)
  USA.1990.95.Basic.Breakdown[[sex, ageType, "Band3"]] <<- createUS1990SelectSubTable(data, "Band3", name = name, scale = scale, sex = sex, ageType = ageType, ...)
}

createUS1990SelectTable.Breakdown(
  file = basic1990file, sheet = "MNB Experience",
  name = "USA 1990-95 Basic Table Male ANB", sex = "m", ageType = "ANB")
createUS1990SelectTable.Breakdown(
  file = basic1990file, sheet = "FNB Experience",
  name = "USA 1990-95 Basic Table Female ANB", sex = "f", ageType = "ANB")
createUS1990SelectTable.Breakdown(
  file = basic1990file, sheet = "MLB Experience",
  name = "USA 1990-95 Basic Table Male ALB", sex = "m", ageType = "ALB")
createUS1990SelectTable.Breakdown(
  file = basic1990file, sheet = "FLB Experience",
  name = "USA 1990-95 Basic Table Female ALB", sex = "f", ageType = "ALB")

save(
  USA.1990.95.Basic.Breakdown,
  file = here::here("data", "US.1990.95.Basic.Breakdown.RData")
)





#############################################################################h#
# USA 2001 VBT Tables                                                      ####
#############################################################################h#

VBT2001file = here::here("data-raw", "US", "VBT", "VBT2001_ALB.xls")
VBT2001file.out = here::here("data", "VBT2001.RData")


VBT2001 = array(
  data = c(mortalityTable.NA),
  dim = c(2, 2, 3),
  dimnames = list(Sex = c("m", "f"), age = c("ANB", "ALB"), type = c("Composite", "Non-Smoker", "Smoker"))
)

createVBT2001 = function(sheet = 1, skip = 0, name, sex = "m", collar, ...) {
  createUSSelectTable(
    VBT2001file, sheet = sheet, skip = skip,
    age.col = "Age...1", rm.cols = c("Age...28"),
    name = name, year = 2001, scale = 0.001,
    table = "2001 VBT", sex = sex, collar = collar, type = "VBT", country = "USA")
}

# TODO: dimensional info for ALB/ANB and Composite/Smoker/Non-Smoker! Currently, only one is stored as collar!
VBT2001[["m", "ALB", "Composite"]] = createVBT2001(sheet = "VBT MComp ALB",skip = 6, name = "2001 VBT Male Composite ALB",   "m", "Composite", ageType = "ALB")
VBT2001[["m", "ALB", "Non-Smoker"]]= createVBT2001(sheet = "VBT MNS ALB",  skip = 2, name = "2001 VBT Male Non-Smoke ALB",   "m", "Non-Smoker", ageType = "ALB")
VBT2001[["m", "ALB", "Smoker"]]    = createVBT2001(sheet = "VBT MSM ALB",  skip = 2, name = "2001 VBT Male Smoker ALB",      "m", "Smoker", ageType = "ALB")
VBT2001[["f", "ALB", "Composite"]] = createVBT2001(sheet = "VBT FComp ALB",skip = 2, name = "2001 VBT Female Composite ALB", "f", "Composite", ageType = "ALB")
VBT2001[["f", "ALB", "Non-Smoker"]]= createVBT2001(sheet = "VBT FNS ALB",  skip = 2, name = "2001 VBT Female Non-Smoker ALB","f", "Non-Smoker", ageType = "ALB")
VBT2001[["f", "ALB", "Smoker"]]    = createVBT2001(sheet = "VBT FSM ALB",  skip = 2, name = "2001 VBT Female Smoker ALB",    "f", "Smoker", ageType = "ALB")
VBT2001[["m", "ANB", "Composite"]] = createVBT2001(sheet = "VBT MComp ANB",skip = 6, name = "2001 VBT Male Composite ANB",   "m", "Composite", ageType = "ANB")
VBT2001[["m", "ANB", "Non-Smoker"]]= createVBT2001(sheet = "VBT MNS ANB",  skip = 2, name = "2001 VBT Male Non-Smoker ANB",  "m", "Non-Smoker", ageType = "ANB")
VBT2001[["m", "ANB", "Smoker"]]    = createVBT2001(sheet = "VBT MSM ANB",  skip = 2, name = "2001 VBT Male Smoker ANB",      "m", "Smoker", ageType = "ANB")
VBT2001[["f", "ANB", "Composite"]] = createVBT2001(sheet = "VBT FComp ANB",skip = 2, name = "2001 VBT Female Composite ANB", "f", "Composite", ageType = "ANB")
VBT2001[["f", "ANB", "Non-Smoker"]]= createVBT2001(sheet = "VBT FNS ANB",  skip = 2, name = "2001 VBT Female Non-Smoker ANB","f", "Non-Smoker", ageType = "ANB")
VBT2001[["f", "ANB", "Smoker"]]    = createVBT2001(sheet = "VBT FSM ANB",  skip = 2, name = "2001 VBT Female Smoker ANB",    "f", "Smoker", ageType = "ANB")

save(
  VBT2001,
  file = VBT2001file.out
)




#############################################################################h#
# USA 2008 VBT Tables                                                      ####
#############################################################################h#

#------------------------------------#
### Primary Tables                ####
#------------------------------------#

VBT2008file = here::here("data-raw", "US", "VBT", "2008 VBT", "App C - 2008 VBT Primary Tables 2008 12_w Sm Unk.xls")
VBT2008file.out = here::here("data", "VBT2008.RData")

VBT2008 = array(
  data = c(mortalityTable.NA),
  dim = c(2, 2, 3),
  dimnames = list(Sex = c("m", "f"), age = c("ANB", "ALB"), type = c("Composite", "Non-Smoker", "Smoker"))
)

createVBT2008 = function(sheet = 1, skip = 0, name, sex = "m", collar, ...) {
  createUSSelectTable(
    VBT2008file, sheet = sheet, skip = skip,
    age.col = "Age...1", rm.cols = c("Age...28"),
    name = name, year = 2008, scale = 0.001,
    table = "2008 VBT", sex = sex, collar = collar, type = "VBT", country = "USA", ...)
}

VBT2008[["m", "ANB", "Non-Smoker"]]= createVBT2008(sheet = "Male NS ANB",   skip = 3, name = "2008 VBT Male Non-Smoker ANB",  "m", "Non-Smoker", ageType = "ANB")
VBT2008[["m", "ANB", "Composite"]] = createVBT2008(sheet = "Male SUN ANB",  skip = 3, name = "2008 VBT Male Composite ANB",   "m", "Composite",  ageType = "ANB")
VBT2008[["m", "ANB", "Smoker"]]    = createVBT2008(sheet = "Male SM ANB",   skip = 3, name = "2008 VBT Male Smoker ANB",      "m", "Smoker",     ageType = "ANB")
VBT2008[["f", "ANB", "Non-Smoker"]]= createVBT2008(sheet = "Female NS ANB", skip = 3, name = "2008 VBT Female Non-Smoker ANB","f", "Non-Smoker", ageType = "ANB")
VBT2008[["f", "ANB", "Composite"]] = createVBT2008(sheet = "Female SUN ANB",skip = 3, name = "2008 VBT Female Composite ANB", "f", "Composite",  ageType = "ANB")
VBT2008[["f", "ANB", "Smoker"]]    = createVBT2008(sheet = "Female SM ANB", skip = 3, name = "2008 VBT Female Smoker ANB",    "f", "Smoker",     ageType = "ANB")
VBT2008[["m", "ALB", "Non-Smoker"]]= createVBT2008(sheet = "Male NS ALB",   skip = 3, name = "2008 VBT Male Non-Smoke ALB",   "m", "Non-Smoker", ageType = "ALB")
VBT2008[["m", "ALB", "Composite"]] = createVBT2008(sheet = "Male SUN ALB",  skip = 3, name = "2008 VBT Male Composite ALB",   "m", "Composite",  ageType = "ALB")
VBT2008[["m", "ALB", "Smoker"]]    = createVBT2008(sheet = "Male SM ALB",   skip = 3, name = "2008 VBT Male Smoker ALB",      "m", "Smoker",     ageType = "ALB")
VBT2008[["f", "ALB", "Non-Smoker"]]= createVBT2008(sheet = "Female NS ALB", skip = 3, name = "2008 VBT Female Non-Smoker ALB","f", "Non-Smoker", ageType = "ALB")
VBT2008[["f", "ALB", "Composite"]] = createVBT2008(sheet = "Female SUN ALB",skip = 3, name = "2008 VBT Female Composite ALB", "f", "Composite",  ageType = "ALB")
VBT2008[["f", "ALB", "Smoker"]]    = createVBT2008(sheet = "Female SM ALB", skip = 3, name = "2008 VBT Female Smoker ALB",    "f", "Smoker",     ageType = "ALB")

save(
  VBT2008,
  file = VBT2008file.out
)




#------------------------------------#
### Limited Underwriting tables   ####
#------------------------------------#

VBT2008LUfile = here::here("data-raw", "US", "VBT", "2008 VBT", "App G - 2008 VBT Limited Underwriting Tables 2008 12_w Sm Unk.xls")
VBT2008LUfile.out = here::here("data", "VBT2008.LimitedUnderwriting.RData")

VBT2008.LimitedUnderwriting = array(
  data = c(mortalityTable.NA),
  dim = c(2, 2, 3),
  dimnames = list(Sex = c("m", "f"), age = c("ANB", "ALB"), type = c("Composite", "Non-Smoker", "Smoker"))
)

createVBT2008LU = function(sheet = 1, skip = 0, name, sex = "m", collar, ...) {
  createUSSelectTable(
    VBT2008LUfile, sheet = sheet, skip = skip,
    age.col = "Age...1", rm.cols = c("Age...28"),
    name = name, year = 2008, scale = 0.001,
    table = "2008 VBT LU", sex = sex, collar = collar, type = "VBT", country = "USA")
}

# TODO: dimensional info for ALB/ANB and Composite/Smoker/Non-Smoker! Currently, only one is stored as collar!
VBT2008.LimitedUnderwriting[["m", "ANB", "Non-Smoker"]]= createVBT2008LU(sheet = "Male NS ANB",   skip = 3, name = "2008 VBT Limited Underwriting Male Non-Smoker ANB",  "m", "Non-Smoker")
VBT2008.LimitedUnderwriting[["m", "ANB", "Composite"]] = createVBT2008LU(sheet = "Male SUN ANB",  skip = 3, name = "2008 VBT Limited Underwriting Male Composite ANB",   "m", "Composite")
VBT2008.LimitedUnderwriting[["m", "ANB", "Smoker"]]    = createVBT2008LU(sheet = "Male SM ANB",   skip = 3, name = "2008 VBT Limited Underwriting Male Smoker ANB",      "m", "Smoker")
VBT2008.LimitedUnderwriting[["f", "ANB", "Non-Smoker"]]= createVBT2008LU(sheet = "Female NS ANB", skip = 3, name = "2008 VBT Limited Underwriting Female Non-Smoker ANB","f", "Non-Smoker")
VBT2008.LimitedUnderwriting[["f", "ANB", "Composite"]] = createVBT2008LU(sheet = "Female SUN ANB",skip = 3, name = "2008 VBT Limited Underwriting Female Composite ANB", "f", "Composite")
VBT2008.LimitedUnderwriting[["f", "ANB", "Smoker"]]    = createVBT2008LU(sheet = "Female SM ANB", skip = 3, name = "2008 VBT Limited Underwriting Female Smoker ANB",    "f", "Smoker")
VBT2008.LimitedUnderwriting[["m", "ALB", "Non-Smoker"]]= createVBT2008LU(sheet = "Male NS ALB",   skip = 3, name = "2008 VBT Limited Underwriting Male Non-Smoke ALB",   "m", "Non-Smoker")
VBT2008.LimitedUnderwriting[["m", "ALB", "Composite"]] = createVBT2008LU(sheet = "Male SUN ALB",  skip = 3, name = "2008 VBT Limited Underwriting Male Composite ALB",   "m", "Composite")
VBT2008.LimitedUnderwriting[["m", "ALB", "Smoker"]]    = createVBT2008LU(sheet = "Male SM ALB",   skip = 3, name = "2008 VBT Limited Underwriting Male Smoker ALB",      "m", "Smoker")
VBT2008.LimitedUnderwriting[["f", "ALB", "Non-Smoker"]]= createVBT2008LU(sheet = "Female NS ALB", skip = 3, name = "2008 VBT Limited Underwriting Female Non-Smoker ALB","f", "Non-Smoker")
VBT2008.LimitedUnderwriting[["f", "ALB", "Composite"]] = createVBT2008LU(sheet = "Female SUN ALB",skip = 3, name = "2008 VBT Limited Underwriting Female Composite ALB", "f", "Composite")
VBT2008.LimitedUnderwriting[["f", "ALB", "Smoker"]]    = createVBT2008LU(sheet = "Female SM ALB", skip = 3, name = "2008 VBT Limited Underwriting Female Smoker ALB",    "f", "Smoker")

save(
  VBT2008.LimitedUnderwriting,
  file = VBT2008LUfile.out
)




#------------------------------------#
### Relative Risk tables          ####
#------------------------------------#

VBT2008RRfile.out = here::here("data", "VBT2008.RelativeRisk.RData")

VBT2008.RelativeRisk.SM = array(
  data = c(mortalityTable.NA),
  dim = c(2, 2, 4),
  dimnames = list(
    Sex = c("m", "f"),
    age = c("ANB", "ALB"),
    RR = c("RR75", "RR100", "RR125", "RR150")
  )
)

VBT2008.RelativeRisk.NS = array(
  data = c(mortalityTable.NA),
  dim = c(2, 2, 10),
  dimnames = list(
    Sex = c("m", "f"),
    age = c("ANB", "ALB"),
    RR = c("RR70", "RR80", "RR90", "RR100", "RR110", "RR120", "RR130", "RR140", "RR150", "RR160")
  )
)

createVBT2008RR = function(file, sheet = 1, skip = 0, name, collar = "Smoker", ...) {
  createUSSelectTable(
    file = file, sheet = sheet, skip = skip,
    age.col = "Age...1", rm.cols = c("Age...28"),
    name = name, year = 2008, scale = 0.001,
    table = sprintf("2008 VBT Relative Risk %s", collar), type = "VBT", country = "USA", collar = collar, ...)
}

for (sex in c("m", "f")) {
  Sx = recode(sex, "m" = "Male", "f" = "Female")
  for (ageType in c("ANB", "ALB")) {
    type = "Non-Smoker"
    Sm = "Nonsmoker"
    Sm.short = "NS"
    for (RR in c("RR70", "RR80", "RR90", "RR100", "RR110", "RR120", "RR130", "RR140", "RR150", "RR160")) {
      file = here::here("data-raw", "US", "VBT", "2008 VBT", sprintf("2008 VBT %s %s %s Relative Risk Tables 2008 12.xls", Sx, Sm, ageType))
      sheet = sprintf("%s %s %s %s", Sx, RR, Sm.short, ageType)
      VBT2008.RelativeRisk.NS[[sex, ageType, RR]] = createVBT2008RR(
        file = file, sheet = sheet, skip = 3,
        name = sprintf("2008 VBT %s %s Non-Smoker ANB", RR, Sx, Sm, ageType),
        sex = sex, collar = type,
        ageType = ageType, RelativeRisk = RR
      )
    }
  }
}
for (sex in c("m", "f")) {
  Sx = recode(sex, "m" = "Male", "f" = "Female")
  for (ageType in c("ANB", "ALB")) {
    type = "Smoker"
    Sm = "Smoker"
    Sm.short = "SM"
    for (RR in c("RR75", "RR100", "RR125", "RR150")) {
      file = here::here("data-raw", "US", "VBT", "2008 VBT", sprintf("2008 VBT %s %s %s Relative Risk Tables 2008 12.xls", Sx, Sm, ageType))
      sheet = sprintf("%s %s %s %s", Sx, RR, Sm.short, ageType)
      VBT2008.RelativeRisk.SM[[sex, ageType, RR]] = createVBT2008RR(
        file = file, sheet = sheet, skip = 3,
        name = sprintf("2008 VBT %s %s Smoker ANB", RR, Sx, Sm, ageType),
        sex = sex, collar = type,
        ageType = ageType, RelativeRisk = RR
      )
    }
  }
}

save(
  VBT2008.RelativeRisk.SM, VBT2008.RelativeRisk.NS,
  file = VBT2008RRfile.out
)







#############################################################################h#
# USA 2015 VBT Tables                                                      ####
#############################################################################h#

#------------------------------------#
### Primary Tables                ####
#------------------------------------#

VBT2015file = here::here("data-raw", "US", "VBT", "2015 VBT", "2015-vbt-unismoke-alb-anb.xlsx")
VBT2015SMfile = here::here("data-raw", "US", "VBT", "2015 VBT", "2015-vbt-smoker-distinct-alb-anb.xlsx")
VBT2015file.out = here::here("data", "VBT2015.RData")

VBT2015 = array(
  data = c(mortalityTable.NA),
  dim = c(2, 2, 3),
  dimnames = list(Sex = c("m", "f"), age = c("ANB", "ALB"), type = c("Unismoke", "Non-Smoker", "Smoker"))
)

createVBT2015 = function(file, sheet, skip = 2, sex = "m", collar, ageType, ...) {
  Sx = recode(sex, "m" = "Male", "f" = "Female")
  name = sprintf("2015 VBT %s %s %s", Sx, collar, ageType)
  if(missing(sheet)) {
    if (collar == "Unismoke") {
      sheet = sprintf("2015 %s %s %s", Sx, collar, ageType)
    } else {
      sheet = sprintf("2015 %s%s %s", toupper(sex), recode(collar, "Smoker" = "SM", "Non-Smoker" = "NS"), ageType)
    }
  }
  createUSSelectTable(
    file = file, sheet = sheet, skip = skip,
    age.col = "Iss. Age", rm.cols = c("Att. Age"),
    name = name, year = 2015, scale = 0.001,
    table = sprintf("2015 VBT %s", collar), sex = sex, collar = collar, type = "VBT", country = "USA", ageType = ageType, ...)
}

VBT2015[["m", "ANB", "Unismoke"]]  = createVBT2015(file = VBT2015file, sex = "m", collar = "Unismoke",   ageType = "ANB")
VBT2015[["f", "ANB", "Unismoke"]]  = createVBT2015(file = VBT2015file, sex = "f", collar = "Unismoke",   ageType = "ANB")
VBT2015[["m", "ALB", "Unismoke"]]  = createVBT2015(file = VBT2015file, sheet = "Male Unismoke ALB", sex = "m", collar = "Unismoke",   ageType = "ALB")
VBT2015[["f", "ALB", "Unismoke"]]  = createVBT2015(file = VBT2015file, sheet = "Female Unismoke ALB", sex = "f", collar = "Unismoke",   ageType = "ALB")

VBT2015[["m", "ANB", "Non-Smoker"]]= createVBT2015(file = VBT2015SMfile, sex = "m", collar = "Non-Smoker", ageType = "ANB")
VBT2015[["m", "ANB", "Smoker"]]    = createVBT2015(file = VBT2015SMfile, sex = "m", collar = "Smoker",     ageType = "ANB")
VBT2015[["f", "ANB", "Non-Smoker"]]= createVBT2015(file = VBT2015SMfile, sex = "f", collar = "Non-Smoker", ageType = "ANB")
VBT2015[["f", "ANB", "Smoker"]]    = createVBT2015(file = VBT2015SMfile, sex = "f", collar = "Smoker",     ageType = "ANB")
VBT2015[["m", "ALB", "Non-Smoker"]]= createVBT2015(file = VBT2015SMfile, sex = "m", collar = "Non-Smoker", ageType = "ALB")
VBT2015[["m", "ALB", "Smoker"]]    = createVBT2015(file = VBT2015SMfile, sex = "m", collar = "Smoker",     ageType = "ALB")
VBT2015[["f", "ALB", "Non-Smoker"]]= createVBT2015(file = VBT2015SMfile, sex = "f", collar = "Non-Smoker", ageType = "ALB")
VBT2015[["f", "ALB", "Smoker"]]    = createVBT2015(file = VBT2015SMfile, sex = "f", collar = "Smoker",     ageType = "ALB")

save(
  VBT2015,
  file = VBT2015file.out
)




#------------------------------------#
### Relative Risk tables          ####
#------------------------------------#

VBT2015RRfile.out = here::here("data", "VBT2015.RelativeRisk.RData")

VBT2015.RelativeRisk.SM = array(
  data = c(mortalityTable.NA),
  dim = c(2, 2, 4),
  dimnames = list(
    Sex = c("m", "f"),
    age = c("ANB", "ALB"),
    RR = c("RR75", "RR100", "RR125", "RR150")
  )
)

VBT2015.RelativeRisk.NS = array(
  data = c(mortalityTable.NA),
  dim = c(2, 2, 10),
  dimnames = list(
    Sex = c("m", "f"),
    age = c("ANB", "ALB"),
    RR = c("RR50", "RR60", "RR70", "RR80", "RR90", "RR100", "RR110", "RR125", "RR150", "RR175")
  )
)


createVBT2015RR = function(file, sheet, age.col = "Iss. Age", skip = 2, sex = "m", collar, ageType, RelativeRisk = "RR100",...) {
  Sx = recode(sex, "m" = "Male", "f" = "Female")
  name = sprintf("2015 VBT %s %s %s %s", RelativeRisk, Sx, collar, ageType)
  if(missing(sheet)) {
    sheet = sprintf("2015 %s %s%s %s", RelativeRisk, toupper(sex), recode(collar, "Smoker" = "SM", "Non-Smoker" = "NS"), ageType)
  }
  createUSSelectTable(
    file = file, sheet = sheet, skip = skip,
    age.col = age.col, rm.cols = c("Att. Age"),
    name = name, year = 2015, scale = 0.001,
    table = sprintf("2015 VBT %s %s", RelativeRisk, collar), sex = sex, collar = collar,
    type = "VBT", country = "USA", agetType = ageType, RelativeRisk = RelativeRisk, ...)
}


for (sex in c("m", "f")) {
  Sx = recode(sex, "m" = "Male", "f" = "Female")
  for (ageType in c("ANB", "ALB")) {
    type = "Non-Smoker"
    Sm = "non"
    file = here::here("data-raw", "US", "VBT", "2015 VBT", sprintf("2015-vbt-%s-%s-rr-%s.xlsx", tolower(Sx), Sm, tolower(ageType)))
    for (RR in c("RR50", "RR60", "RR70", "RR80", "RR90", "RR100", "RR110", "RR125", "RR150", "RR175")) {
      VBT2015.RelativeRisk.NS[[sex, ageType, RR]] = createVBT2015RR(
        age.col = ifelse(ageType == "ALB", "Age", "Iss. Age"),
        file = file,
        sex = sex, collar = type, ageType = ageType, RelativeRisk = RR
      )
    }
  }
}
for (sex in c("m", "f")) {
  Sx = recode(sex, "m" = "Male", "f" = "Female")
  for (ageType in c("ANB", "ALB")) {
    type = "Smoker"
    Sm = "smoker"
    file = here::here("data-raw", "US", "VBT", "2015 VBT", sprintf("2015-vbt-%s-%s-rr-%s.xlsx", tolower(Sx), Sm, tolower(ageType)))
    for (RR in c("RR75", "RR100", "RR125", "RR150")) {
      VBT2015.RelativeRisk.SM[[sex, ageType, RR]] = createVBT2015RR(
        age.col = ifelse(ageType == "ALB", "Age", "Iss. Age"),
        file = file,
        sex = sex, collar = type, ageType = ageType, RelativeRisk = RR
      )
    }
  }
}

save(
  VBT2015.RelativeRisk.SM, VBT2015.RelativeRisk.NS,
  file = VBT2015RRfile.out
)




