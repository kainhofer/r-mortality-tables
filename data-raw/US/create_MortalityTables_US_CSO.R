#############################################################################m#
#  Skript to generate US CSO table objects                                 ####
#############################################################################m#
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
# USA 2017 CSO Tables                                                      ####
#############################################################################h#

CSO2017Pref.file.out = here::here("data", "CSO2017.Preferred.RData")

CSO2017 = array(
  data = c(mortalityTable.NA),
  dim = c(6, 2, 3, 2),
  dimnames = list(
    sex = c("m", "80% Male", "60% Male", "40% Male", "20% Male", "f"),
    age = c("ANB", "ALB"),
    type = c("Composite", "Non-Smoker", "Smoker"),
    loaded = c("loaded", "unloaded")
  )
)

createCSO2017 = function(
  file, sheet, skip = 2,
  sex = "m", collar = "Composite", ageType, loaded = "loaded",
  age.col = "Iss. Age", rm.cols = c("Att. Age"),
  ...
) {
  Sx = recode(sex, "m" = "Male", "f" = "Female")
  name = sprintf("2017 %s CSO %s %s %s", loaded, Sx, collar, ageType)
  if(missing(sheet)) {
    if (collar == "Composite") {
      sheet = sprintf("2017 %s %s %s", Sx, collar, ageType)
    } else {
      sheet = sprintf("2017 %s%s %s", toupper(sex), recode(collar, "Smoker" = "SM", "Non-Smoker" = "NS"), ageType)
    }
  }
  createUSSelectTable(
    file = file, sheet = sheet, skip = skip,
    age.col = age.col, rm.cols = rm.cols,
    name = name, year = 2017, scale = 0.001,
    table = sprintf("2017 CSO %s", collar), sex = sex, collar = collar, type = "CSO", country = "USA", ageType = ageType, loaded = loaded,
    ...)
}


#-----------------------------------------------------------------#
### Primary Tables: Composite loaded / unloaded                ####
#-----------------------------------------------------------------#

loaded = "loaded"
type = "Composite"
file = here::here("data-raw", "US", "CSO", "2017 CSO", "research-2016-08-composite-loaded-cso.xlsx")
for (sex in c("m", "f")) {
  Sx = recode(sex, "m" = "Male", "f" = "Female")
  for (ageType in c("ANB", "ALB")) {
    CSO2017[[sex, ageType, type, loaded]] = createCSO2017(
      file = file,
      sex = sex, collar = type, ageType = ageType, loaded = loaded
    )
  }
}

loaded = "unloaded"
type = "Composite"
file = here::here("data-raw", "US", "CSO", "2017 CSO", "research-2017-cso-unloaded.xlsx")
for (sex in c("m", "f")) {
  Sx = recode(sex, "m" = "Male", "f" = "Female")
  for (ageType in c("ANB", "ALB")) {
    CSO2017[[sex, ageType, type, loaded]] = createCSO2017(
      file = file,
      sex = sex, collar = type, ageType = ageType, loaded = loaded
    )
  }
}


#-----------------------------------------------------------------#
### Smoker / Nonsmoker  loaded / unloaded                ####
#-----------------------------------------------------------------#

loaded = "loaded"
file = here::here("data-raw", "US", "CSO", "2017 CSO", "research-2015-smoker-distinct-loaded-cso.xlsx")
for (sex in c("m", "f")) {
  Sx = recode(sex, "m" = "Male", "f" = "Female")
  for (ageType in c("ANB", "ALB")) {
    for (type in c("Non-Smoker", "Smoker")) {
      CSO2017[[sex, ageType, type, loaded]] = createCSO2017(
        file = file,
        sex = sex, collar = type, ageType = ageType, loaded = loaded
      )
    }
  }
}

loaded = "unloaded"
file = here::here("data-raw", "US", "CSO", "2017 CSO", "research-2017-smoker-distinct-unloaded-cso.xlsx")
for (sex in c("m", "f")) {
  Sx = recode(sex, "m" = "Male", "f" = "Female")
  for (ageType in c("ANB", "ALB")) {
    for (type in c("Non-Smoker", "Smoker")) {
      CSO2017[[sex, ageType, type, loaded]] = createCSO2017(
        file = file,
        sex = sex, collar = type, ageType = ageType, loaded = loaded
      )
    }
  }
}


#-----------------------------------------------------------------#
### Gender-blended tables (only loaded)                        ####
#-----------------------------------------------------------------#

loaded = "loaded"
type = "Composite"
file = here::here("data-raw", "US", "CSO", "2017 CSO", "research-2016-08-gender-cso-blended.xlsx")
for (sex in c("80% Male", "60% Male", "40% Male", "20% Male")) {
  for (ageType in c("ANB", "ALB")) {
    sheet = sprintf("%s %s %s", ageType, type, sex)
    CSO2017[[sex, ageType, type, loaded]] = createCSO2017(
      file = file, sheet = sheet,
      sex = sex, collar = type, ageType = ageType, loaded = loaded
    )
  }
}
loaded = "loaded"
type = "Non-Smoker"
file = here::here("data-raw", "US", "CSO", "2017 CSO", "research-2017-non-smoker-cso-blended.xlsx")
for (sex in c("80% Male", "60% Male", "40% Male", "20% Male")) {
  for (ageType in c("ANB", "ALB")) {
    sheet = sprintf("%s %s %s", ageType, "Nonsmoker", sex)
    CSO2017[[sex, ageType, type, loaded]] = createCSO2017(
      file = file, sheet = sheet,
      sex = sex, collar = type, ageType = ageType, loaded = loaded
    )
  }
}
loaded = "loaded"
type = "Smoker"
file = here::here("data-raw", "US", "CSO", "2017 CSO", "research-2017-smoker-cso-blended.xlsx")
for (sex in c("80% Male", "60% Male", "40% Male", "20% Male")) {
  for (ageType in c("ANB", "ALB")) {
    sheet = sprintf("%s %s %s", ageType, type, sex)
    CSO2017[[sex, ageType, type, loaded]] = createCSO2017(
      file = file, sheet = sheet,
      sex = sex, collar = type, ageType = ageType, loaded = loaded
    )
  }
}


save(CSO2017, file = CSO2017file.out)


# plotMortalityTables(CSO2017) + facet_grid(ageType ~ collar) + aes(linetype = loaded, color = sex)

# plotMortalityTables(CSO2017) + facet_grid(sex ~ ageType) + aes(linetype = loaded, color = collar)


#-----------------------------------------------------------------#
### Preferred 2017 CSO Tables (only loaded)                    ####
#-----------------------------------------------------------------#

CSO2017.Preferred = array(
  data = c(mortalityTable.NA),
  dim = c(3, 2, 2, 2, 2),
  dimnames = list(
    preferred = c("Super Preferred", "Preferred", "Residual"),
    sex = c("m", "f"),
    age = c("ANB", "ALB"),
    type = c("Non-Smoker", "Smoker"),
    loaded = c("loaded", "unloaded")
  )
)

createCSO2017.Preferred = function(
  file, sheet, skip = 2,
  sex = "m", collar = "Composite", ageType, loaded = "loaded", Preferred = "Preferred",
  age.col = "Iss. Age", rm.cols = c("Att. Age"),
  ...
) {
# browser()
  Sx = recode(sex, "m" = "Male", "f" = "Female")
  Sm.short = recode(collar, "Smoker" = "SM", "Non-Smoker" = "NS")
  name = sprintf("%s %s %s 2017 %sCSO %s", Sx, collar, Preferred, recode(loaded, "loaded" = "", "unloaded" = "unloaded "), ageType)
  if(missing(sheet)) {
    if (collar == "Non-Smoker") {
      prf = recode(Preferred, "Super Preferred" = 1, "Preferred" = 2, "Residual" = 3)
    } else {
      prf = recode(Preferred, "Preferred" = 1, "Residual" = 2)
    }
    sheet = sprintf("%s%s%s %s", toupper(sex), Sm.short, prf, ageType)
  }
  createUSSelectTable(
    file = file, sheet = sheet, skip = skip,
    age.col = age.col, rm.cols = rm.cols,
    name = name, year = 2017, scale = 0.001,
    table = sprintf("2017 CSO %s", collar), sex = sex, collar = collar, type = "CSO", country = "USA", ageType = ageType, loaded = loaded, Preferred = Preferred,
    ...)
}

loaded = "loaded"
file = here::here("data-raw", "US", "CSO", "2017 CSO", "research-2015-cso-preferred-structure.xlsx")
for (preferred in c("Super Preferred", "Preferred", "Residual")) {
  for (sex in c("m", "f")) {
    for (ageType in c("ANB", "ALB")) {
      for (type in c("Smoker", "Non-Smoker")) {
        # There are no super-preferred smokers => skip that case
        if (preferred != "Super Preferred" || type != "Smoker") {
          CSO2017.Preferred[[preferred, sex, ageType, type, loaded]] = createCSO2017.Preferred(
            file = file,
            sex = sex, collar = type, ageType = ageType, loaded = loaded, Preferred = preferred
          )
        }
      }
    }
  }
}
loaded = "unloaded"
file = here::here("data-raw", "US", "CSO", "2017 CSO", "research-2016-cso-preferred-structure.xlsx")
for (preferred in c("Super Preferred", "Preferred", "Residual")) {
  for (sex in c("m", "f")) {
    for (ageType in c("ANB", "ALB")) {
      for (type in c("Smoker", "Non-Smoker")) {
        # There are no super-preferred smokers => skip that case
        if (preferred != "Super Preferred" || type != "Smoker") {
          CSO2017.Preferred[[preferred, sex, ageType, type, loaded]] = createCSO2017.Preferred(
            file = file,
            sex = sex, collar = type, ageType = ageType, loaded = loaded, Preferred = preferred
          )
        }
      }
    }
  }
}


# plotMortalityTables(CSO2017.Preferred[,,"ANB",,]) + facet_grid(sex ~ collar) + aes(linetype = loaded, color = Preferred)

save(CSO2017.Preferred, file = CSO2017Pref.file.out)







