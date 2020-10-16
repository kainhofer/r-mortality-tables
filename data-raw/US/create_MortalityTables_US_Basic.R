#############################################################################m#
#  Skript to generate US Accidental death benefits table objects               ####
#############################################################################m#
library(MortalityTables)
library(here)
library(readxl)
library(dplyr)
library(purrr)

convertUSSelectData = function(data, ultimate_cols = c("qx" = "Ultimate", "age" = "Attained Age"), initial_cols = c("Initial Age" = "Initial Age")) {
  # Prepare Ultimate table (handled separately, as we might not have an initial age)
  if (!is_empty(ultimate_cols)) {
    ultimate = data %>%
      dplyr::select(all_of(ultimate_cols)) %>%
      dplyr::mutate(Duration = "Ultimate")# %>%
      # dplyr::select(age, Duration,  qx)
  } else {
    ultimate = tibble()
  }
  # All remaining durations are collected and the actual age at that corresponding year is calculated
  selct = data %>%
    dplyr::select(-all_of(ultimate_cols)) %>%
    rename(initial_cols) %>%
    tidyr::gather(Duration, qx, -`Initial Age`) %>%
    filter(!is.null(qx), !is.na(qx)) %>%
    dplyr::mutate(age = `Initial Age` + as.numeric(Duration) - 1) %>%
    dplyr::select(age, Duration, qx)

  qx = dplyr::bind_rows(selct, ultimate) %>%
    filter(!is.null(qx), !is.na(qx)) %>%
    tidyr::pivot_wider(names_from = Duration, values_from = qx) %>%
    dplyr::arrange(age)
  qx
}

createUSSelectTable = function(
  file, sheet = 1, skip = 0, ultimate_cols = c("qx" = "Ultimate", "age" = "Attained Age"), initial_cols = c("Initial Age" = "Initial Age"),
  name, year = 2020, scale = 1,
  table, sex = "m", collar, type = "CSO", country = "USA", ...
) {
  data = readxl::read_excel(file, sheet = sheet, skip = skip)
# browser()
  qx = convertUSSelectData(data, ultimate_cols = ultimate_cols, initial_cols = initial_cols)
  # browser()
  ages = qx$age
  qx = qx %>%
    dplyr::select(-age)

  mortalityTable.period(
    name = name, ages = ages, deathProbs = as.matrix(scale * qx), baseYear = year,
    data = list(
      dim = list(table = table, sex = sex, collar = collar, type = type, country = country, data = "official", year = year)
    )
  )
}





#############################################################################h#
# USA 1925-39 Basic Select and Ultimate Tables                             ####
#############################################################################h#

basic1925file = here::here("data-raw", "US", "Basic_Select-Ultimate", "USA_1925-39_BasicTable.xlsx")
basic1925file.out = here::here("data", "US.1925.39.Basic.RData")


US.1925.39.Basic = createUSSelectTable(
  basic1925file, sheet = 1, skip = 3, ultimate_cols = c(),
  name = "USA 1925-39 Basic Table", year = 1939,
  table = "1925-39 Basic Table", sex = "u", collar = "", type = "Basic Select", country = "USA")

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
  table = "1946-49 Basic Table", sex = "u", collar = "", type = "Basic Select-Ultimate", country = "USA")

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
  table = "1955-60 Basic Table", sex = "u", collar = "", type = "Basic Select-Ultimate", country = "USA")

USA.1955.60.Basic.M = createUSSelectTable(
  basic1955file, sheet = 2, skip = 3, scale = 0.001,
  name = "USA 1955-60 Basic Table Males", year = 1960,
  table = "1955-60 Basic Table", sex = "m", collar = "", type = "Basic Select-Ultimate", country = "USA")

USA.1955.60.Basic.F = createUSSelectTable(
  basic1955file, sheet = 3, skip = 3, scale = 0.001,
  name = "USA 1955-60 Basic Table Females", year = 1960,
  table = "1955-60 Basic Table", sex = "f", collar = "", type = "Basic Select-Ultimate", country = "USA")

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
  name = "USA 1975-80 Basic Table Male ANB", year = 1970,
  table = "1975-80 Basic Table", sex = "m", collar = "ANB", type = "Basic Select-Ultimate", country = "USA")

USA.1965.70.Basic.F.ANB = createUSSelectTable(
  basic1965file, sheet = 2, skip = 3,
  name = "USA 1975-80 Basic Table Female ANB", year = 1970,
  table = "1975-80 Basic Table", sex = "f", collar = "ANB", type = "Basic Select-Ultimate", country = "USA")

USA.1965.70.Basic.M.ALB = createUSSelectTable(
  basic1965file, sheet = 3, skip = 3,
  name = "USA 1975-80 (Modified) Basic Table Male ALB", year = 1970,
  table = "1975-80 Basic Table", sex = "m", collar = "ALB", type = "Basic Select-Ultimate", country = "USA")

USA.1965.70.Basic.F.ALB = createUSSelectTable(
  basic1965file, sheet = 4, skip = 3,
  name = "USA 1975-80 (Modified) Basic Table Female ALB", year = 1970,
  table = "1975-80 Basic Table", sex = "f", collar = "ALB", type = "Basic Select-Ultimate", country = "USA")


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
  table = "1975-80 Basic Table", sex = "m", collar = "ANB", type = "Basic Select-Ultimate", country = "USA")

USA.1975.80.Basic.F.ANB = createUSSelectTable(
  basic1975file, sheet = 2, skip = 3, scale = 0.001,
  name = "USA 1975-80 Basic Table Female ANB", year = 1980,
  table = "1975-80 Basic Table", sex = "f", collar = "ANB", type = "Basic Select-Ultimate", country = "USA")

USA.1975.80.Basic.M.ALB = createUSSelectTable(
  basic1975file, sheet = 3, skip = 3, scale = 0.001,
  name = "USA 1975-80 Basic Table Male ALB", year = 1980,
  table = "1975-80 Basic Table", sex = "m", collar = "ALB", type = "Basic Select-Ultimate", country = "USA")

USA.1975.80.Basic.F.ALB = createUSSelectTable(
  basic1975file, sheet = 4, skip = 3, scale = 0.001,
  name = "USA 1975-80 Basic Table Female ALB", year = 1980,
  table = "1975-80 Basic Table", sex = "f", collar = "ALB", type = "Basic Select-Ultimate", country = "USA")


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
  ultimate_cols = c("qx" = "Ultimate", "age" = "...29"), initial_cols = c("Initial Age" = "MNB"),
  name = "USA 1985-90 Basic Table Male ANB", year = 1990,
  table = "1985-90 Basic Table", sex = "m", collar = "ANB", type = "Basic Select-Ultimate", country = "USA")

USA.1985.90.Basic.F.ANB = createUSSelectTable(
  here::here("data-raw", "US", "Basic_Select-Ultimate", "Fnb85-90.xls"), sheet = 1, skip = 1, scale = 0.001,
  ultimate_cols = c("qx" = "Ultimate", "age" = "Att. Age"), initial_cols = c("Initial Age" = "FNB"),
  name = "USA 1985-90 Basic Table Female ANB", year = 1990,
  table = "1985-90 Basic Table", sex = "f", collar = "ANB", type = "Basic Select-Ultimate", country = "USA")

USA.1985.90.Basic.M.ALB = createUSSelectTable(
  basic1985file, sheet = 1, skip = 1, scale = 0.001,
  ultimate_cols = c("qx" = "ultimates", "age" = "att. Age"), initial_cols = c("Initial Age" = "MLB"),
  name = "USA 1985-90 Basic Table Male ALB", year = 1990,
  table = "1985-90 Basic Table", sex = "m", collar = "ALB", type = "Basic Select-Ultimate", country = "USA")

USA.1985.90.Basic.F.ALB = createUSSelectTable(
  basic1985file, sheet = 2, skip = 1, scale = 0.001,
  ultimate_cols = c("qx" = "Ultimates", "age" = "Att. Age"), initial_cols = c("Initial Age" = "FLB"),
  name = "USA 1985-90 Basic Table Female ALB", year = 1990,
  table = "1985-90 Basic Table", sex = "f", collar = "ALB", type = "Basic Select-Ultimate", country = "USA")


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
  file, sheet = 1, skip = 0, ultimate_cols = c("qx" = "Ultimate", "age" = "Attained Age"), initial_cols = c("Initial Age" = "Initial Age"),
  name, scale = 0.001,
  sex = "m", collar, ...
) {
  # browser()
  data = map(sheet, function(s) readxl::read_excel(sheet = s, path = file, skip = skip) ) %>%
    bind_rows() %>%
    mutate(`Attained Age` = .[[1]] + 25)
  # browser()
  qx = convertUSSelectData(data, ultimate_cols = ultimate_cols, initial_cols = initial_cols)
  # browser()
  ages = qx$age
  qx = qx %>%
    dplyr::select(-age)

  mortalityTable.period(
    name = name, ages = ages, deathProbs = as.matrix(scale * qx), baseYear = 1995,
    data = list(
      dim = list(table = "1990-95 Basic Table", sex = sex, collar = collar, type = "Basic Select-Ultimate", country = "USA", data = "official", year = 1995)
    )
  )
}

USA.1990.95.Basic.M.ANB = createUS1990SelectTable(
  basic1990file, sheet = c("MNB Experience", "MNB Extrapolation"), scale = 0.001,
  initial_cols = c("Initial Age" = "MNB"),
  name = "USA 1990-95 Basic Table Male ANB",  sex = "m", collar = "ANB")

USA.1990.95.Basic.F.ANB = createUS1990SelectTable(
  basic1990file, sheet = c("FNB Experience", "FNB Extrapolation"), scale = 0.001,
  initial_cols = c("Initial Age" = "FNB"),
  name = "USA 1990-95 Basic Table Female ANB",  sex = "f", collar = "ANB")

USA.1990.95.Basic.M.ALB = createUS1990SelectTable(
  basic1990file, sheet = c("MLB Experience", "MLB Extrapolation"), scale = 0.001,
  initial_cols = c("Initial Age" = "MLB"),
  name = "USA 1990-95 Basic Table Male ALB",  sex = "m", collar = "ALB")

USA.1990.95.Basic.F.ALB = createUS1990SelectTable(
  basic1990file, sheet = c("FLB Experience", "FLB Extrapolation"), scale = 0.001,
  initial_cols = c("Initial Age" = "FLB"),
  name = "USA 1990-95 Basic Table Female ALB",  sex = "f", collar = "ALB")


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


## TODO: Smoker / Non-Smoker / Band 1 / Band 2 / Band 3 variants

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


createUS1990SelectSubTable = function(data, variant = "Non-Smoker", name, scale = 0.001, sex, collar) {
  data.sm = data
  data.sm[,2:11] = data.sm[,2:11] * factors.1990Basic[,, sex, variant]

  qx = convertUSSelectData(data.sm, ultimate_cols = c())
  ages = qx$age
  qx = qx %>%
    dplyr::select(-age)

  mortalityTable.period(
    name = paste(name, variant), ages = ages, deathProbs = as.matrix(scale * qx), baseYear = 1995,
    data = list(
      dim = list(table = paste("1990-95 Basic Table", variant), sex = sex, collar = collar, type = "Basic Select-Ultimate", country = "USA", data = "official", year = 1995, variant = variant)
    )
  )
}
createUS1990SelectTable.Breakdown = function(
  file, sheet = 1, name, scale = 0.001, sex = "m", collar, ...
) {
  data = readxl::read_excel(sheet = sheet, path = file, range = "A22:K74", col_names = c("Initial Age", as.character(1:10)))
  USA.1990.95.Basic.Breakdown[[sex, collar, "Non-Smoker"]] <<- createUS1990SelectSubTable(
    data, "Non-Smoker", name = name, scale = scale, sex = sex, collar = collar)
  USA.1990.95.Basic.Breakdown[[sex, collar, "Smoker"]] <<- createUS1990SelectSubTable(
    data, "Smoker", name = name, scale = scale, sex = sex, collar = collar)
  USA.1990.95.Basic.Breakdown[[sex, collar, "Band1"]] <<- createUS1990SelectSubTable(
    data, "Band1", name = name, scale = scale, sex = sex, collar = collar)
  USA.1990.95.Basic.Breakdown[[sex, collar, "Band2"]] <<- createUS1990SelectSubTable(
    data, "Band2", name = name, scale = scale, sex = sex, collar = collar)
  USA.1990.95.Basic.Breakdown[[sex, collar, "Band3"]] <<- createUS1990SelectSubTable(
    data, "Band3", name = name, scale = scale, sex = sex, collar = collar)
}

createUS1990SelectTable.Breakdown(
  file = basic1990file, sheet = "MNB Experience",
  name = "USA 1990-95 Basic Table Male ANB", sex = "m", collar = "ANB")
createUS1990SelectTable.Breakdown(
  file = basic1990file, sheet = "FNB Experience",
  name = "USA 1990-95 Basic Table Female ANB", sex = "f", collar = "ANB")
createUS1990SelectTable.Breakdown(
  file = basic1990file, sheet = "MLB Experience",
  name = "USA 1990-95 Basic Table Male ALB", sex = "m", collar = "ALB")
createUS1990SelectTable.Breakdown(
  file = basic1990file, sheet = "FLB Experience",
  name = "USA 1990-95 Basic Table Female ALB", sex = "f", collar = "ALB")

save(
  USA.1990.95.Basic.Breakdown,
  file = here::here("data", "US.1990.95.Basic.Breakdown.RData")
)





#############################################################################h#
# USA 2001 VBT Tables                                                      ####
#############################################################################h#

VBT2001file = here::here("data-raw", "US", "VBT", "USA_2001_VBT.xlsx")
VBT2001file.out = here::here("data", "US.2001.VBT.RData")

# TODO!




#############################################################################h#
# USA 2008 VBT Tables                                                      ####
#############################################################################h#

VBT2008file = here::here("data-raw", "US", "VBT", "USA_2008_VBT.xlsx")
VBT2008file.out = here::here("data", "US.2008.VBT.RData")

# TODO!




#############################################################################h#
# USA 2015 VBT Tables                                                      ####
#############################################################################h#

VBT2015file = here::here("data-raw", "US", "VBT", "USA_2015_VBT.xlsx")
VBT2015file.out = here::here("data", "US.2015.VBT.RData")

# TODO!




