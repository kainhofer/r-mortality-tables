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
# USA 1941 CSO Tables                                                      ####
#############################################################################h#

CSO1941.file = here::here("data-raw", "US", "CSO", "1941 CSO", "USA_1941_CSO.xlsx")
CSO1941.file.out = here::here("data", "CSO1941.RData")
CSO1941.data = read_excel(CSO1941.file, sheet = "1941 CSO", skip = 4)

CSO1941 = array(
  data = c(mortalityTable.NA),
  dim = c(2),
  dimnames = list(
    ageType = c("ANB", "ALB")
  )
)

CSO1941[["ANB"]] = mortalityTable.period(
  name = "1941 CSO ANB", ages = CSO1941.data$Age, deathProbs = CSO1941.data$`CSO ANB`, baseYear = 1941,
  data = list(
    dim = list(table = "1941 CSO", sex = "u", collar = "Composite", country = "USA", ageType = "ANB", data = "official", year = 1941)
  )
)
CSO1941[["ALB"]] = mortalityTable.period(
  name = "1941 CSO ALB", ages = CSO1941.data$Age, deathProbs = CSO1941.data$`CSO ALB`, baseYear = 1941,
  data = list(
    dim = list(table = "1941 CSO", sex = "u", collar = "Composite", country = "USA", ageType = "ALB", data = "official", year = 1941)
  )
)
CSO1941.basic = mortalityTable.period(
  name = "1941 CSO Basic ANB", ages = CSO1941.data$Age, deathProbs = CSO1941.data$`CSO Basic ANB`, baseYear = 1941,
  data = list(
    dim = list(table = "1941 CSO Basic", sex = "u", collar = "Composite", country = "USA", ageType = "ANB", data = "official", year = 1941)
  )
)

save(CSO1941, CSO1941.basic, file = CSO1941.file.out)




# plotMortalityTables(CSO1941, CSO1941.basic, title = "1941 CSO tables")









#############################################################################h#
# USA 1958 CSO Tables                                                      ####
#############################################################################h#

CSO1958.file = here::here("data-raw", "US", "CSO", "1958 CSO", "USA_1958_CSO-CET.xlsx")
CSO1958.file.out = here::here("data", "CSO1958.RData")

CSO1958 = array(
  data = c(mortalityTable.NA),
  dim = c(2, 2),
  dimnames = list(
    sex = c("m", "f"),
    age = c("ANB", "ALB")
  )
)
CET1958 = CSO1958

CSO1958.basic = array(
  data = c(mortalityTable.NA),
  dim = c(2),
  dimnames = list(
    sex = c("m", "f")
  )
)

createCSO1958 = function(
  data, col, age.col = "Age", sex = "m", collar = "Composite",
  ageType = "ANB",table = "1958 CSO", baseYear = 1958, ...
) {
  qx = data %>%
    select(age = !!age.col, qx = !!col) %>%
    filter(!is.na(qx))

  Sx = recode(sex, "m" = "Male", "f" = "Female")
  name = sprintf("%s %s %s", table, Sx, ageType)

  mortalityTable.period(
    name = name, ages = qx$age, deathProbs = qx$qx, baseYear = baseYear,
    data = list(
      dim = list(table = table, sex = sex, collar = collar, country = "USA", ageType = ageType, data = "official", year = baseYear, ...)
    )
  )
}


CSO.data = read_excel(CSO1958.file, sheet = "1958 CSO CET", skip = 4)

for (sex in c("m", "f")) {
  Sx = recode(sex, "m" = "Male", "f" = "Female")
  CSO1958.basic[[sex]] = createCSO1958(
    data = CSO.data, col = paste("CSO Basic", Sx, "ANB"), age.col = "Age",
    table = "1958 CSO Basic", sex = sex, ageType = "ANB"
  )

  for (ageType in c("ANB", "ALB")) {
    CSO1958[[sex, ageType]] = createCSO1958(
      data = CSO.data, col = paste("CSO", Sx, ageType), age.col = "Age",
      table = "1958 CSO", sex = sex, ageType = ageType
    )
    CET1958[[sex, ageType]] = createCSO1958(
      data = CSO.data, col = paste("CET", Sx, ageType), age.col = "Age",
      table = "1958 CET", sex = sex, ageType = ageType
    )
  }
}

save(CSO1958, CSO1958.basic, CET1958, file = CSO1958.file.out)


# plotMortalityTables(CSO1958, CET1958, CSO1958.basic, aes = aes(color = sex), title = "1958 CSO and CET tables") + facet_grid(table ~ageType)









#############################################################################h#
# USA 1980 CSO Tables                                                      ####
#############################################################################h#

CSO1980.file = here::here("data-raw", "US", "CSO", "1980 CSO", "USA_1980_CSO-CET.xlsx")
CSO1980.file.out = here::here("data", "CSO1980.RData")

CSO1980 = array(
  data = c(mortalityTable.NA),
  dim = c(7, 2, 3),
  dimnames = list(
    sex = c("m", "80% Male", "60% Male", "50% Male", "40% Male", "20% Male", "f"),
    age = c("ANB", "ALB"),
    type = c("Composite", "Non-Smoker", "Smoker")
  )
)
CET1980 = CSO1980

CSO1980.basic = array(
  data = c(mortalityTable.NA),
  dim = c(2, 1, 3),
  dimnames = list(
    sex = c("m", "f"),
    age = c("ANB"),
    type = c("Composite", "Non-Smoker", "Smoker")
  )
)

createCSO1980 = function(
  data, col, age.col = "Age",
  sex = "Male", collar = "Composite", ageType = "ANB",
  table = "1980 CSO", baseYear = 1980, ...
) {
  qx = data %>%
    select(age = !!age.col, qx = !!col) %>%
    filter(!is.na(qx))

  name = ifelse(collar == "Composite",
    sprintf("%s %s %s", table, sex, ageType),
    sprintf("%s %s %s %s", table, sex, collar, ageType))

  mortalityTable.period(
    name = name, ages = qx$age, deathProbs = qx$qx, baseYear = baseYear,
    selectInitialAge = TRUE,
    data = list(
      dim = list(table = table, sex = sex, collar = collar, country = "USA", ageType = ageType, data = "official", year = baseYear, ...)
    )
  )
}


# plotMortalityTables(CSO1980) + facet_grid(.~ageType)
#-----------------------------------------------------------------#
### Primary Tables: M/F Comp/NS/SM ANB                         ####
#-----------------------------------------------------------------#

CSO.data = full_join(
  read_excel(CSO1980.file, sheet = "1980 CSO", skip = 4),
  read_excel(CSO1980.file, sheet = "1980 CSO gender-blended", skip = 5),
  by = "Age"
)
CET.data = full_join(
  read_excel(CSO1980.file, sheet = "1980 CET", skip = 4),
  read_excel(CSO1980.file, sheet = "1980 CET gender-blended", skip = 5),
  by = "Age"
)
for (sex in c("m", "80% Male", "60% Male", "50% Male", "40% Male", "20% Male", "f")) {
  Sx = recode(sex, "m" = "Male", "f" = "Female")
  for (ageType in c("ANB", "ALB")) {
    for (type in c("Composite", "Non-Smoker", "Smoker")) {
      col = ifelse(type == "Composite", paste(Sx, ageType), paste(Sx, type, ageType))
      CSO1980[[sex, ageType, type]] = createCSO1980(
        data = CSO.data, col = col, age.col = "Age",
        table = "1980 CSO", sex = Sx, collar = type, ageType = ageType
      )
      CET1980[[sex, ageType, type]] = createCSO1980(
        data = CSO.data, col = col, age.col = "Age",
        table = "1980 CET", sex = Sx, collar = type, ageType = ageType
      )
    }
  }
}
for (sex in c("m", "f")) {
  Sx = recode(sex, "m" = "Male", "f" = "Female")
  for (ageType in c("ANB")) {
    for (type in c("Composite", "Non-Smoker", "Smoker")) {
      col = ifelse(type == "Composite", paste("Basic Table", Sx, ageType), paste("Basic Table", Sx, type, ageType))
      CSO1980.basic[[sex, ageType, type]] = createCSO1980(
        data = CSO.data, col = col, age.col = "Age",
        table = "1980 CSO Basic", sex = Sx, collar = type, ageType = ageType
      )
    }
  }
}

# plotMortalityTables(CSO1980, aes = aes(color = sex)) + facet_grid(ageType ~ collar)
# plotMortalityTables(CSO1980.basic, aes = aes(color = sex)) + facet_grid(ageType ~ collar)
# plotMortalityTables(CSO1980.basic, aes = aes(color = collar, linetype = sex))



#-----------------------------------------------------------------#
### Select tables                                              ####
#-----------------------------------------------------------------#

CSO1980.select = CSO1980
CET1980.select = CET1980

# Add selection factors
CSO1980.selectionFactors = list()

for (sex in c("m", "80% Male", "60% Male", "50% Male", "40% Male", "20% Male", "f")) {
  CSO1980.selectionFactors[[sex]] = read_excel(CSO1980.file, sheet = sprintf("Selection Factors %s", recode(sex, "m" = "Male", "f" = "Female")), skip = 4)
  CSO1980.select[sex,,] = CSO1980.select[sex,,] %>%
    mT.setSlot("selectionFactors", CSO1980.selectionFactors[[sex]]) %>%
    mT.setDimInfo(data = "select")
  CET1980.select[sex,,] = CET1980.select[sex,,] %>%
    mT.setSlot("selectionFactors", CSO1980.selectionFactors[[sex]]) %>%
    mT.setDimInfo(data = "select")
}



# plotMortalityTables(
#   CSO1980[,"ANB","Composite"] %>% mT.setDimInfo(SelectionAge = "Ultimate"),
# CSO1980.select[,"ANB","Composite"] %>% getPeriodTable(Period = 1980, selectionAge = 20) %>% mT.setDimInfo(SelectionAge = 20),
# CSO1980.select[,"ANB","Composite"] %>% getPeriodTable(Period = 1980, selectionAge = 40) %>% mT.setDimInfo(SelectionAge = 40),
# CSO1980.select[,"ANB","Composite"] %>% getPeriodTable(Period = 1980, selectionAge = 60) %>% mT.setDimInfo(SelectionAge = 60),
# CSO1980.select[,"ANB","Composite"] %>% getPeriodTable(Period = 1980, selectionAge = 80) %>% mT.setDimInfo(SelectionAge = 80),
# aes = aes(color = as.factor(SelectionAge))
# ) + facet_grid(sex~.)


save(CSO1980, CET1980, CSO1980.basic, CSO1980.select, CET1980.select, file = CSO1980.file.out)







#############################################################################h#
# USA 2001 CSO Tables                                                      ####
#############################################################################h#

CSO2001.file.out = here::here("data", "CSO2001.RData")

CSO2001 = array(
  data = c(mortalityTable.NA),
  dim = c(7, 2, 3),
  dimnames = list(
    sex = c("m", "80% Male", "60% Male", "50% Male", "40% Male", "20% Male", "f"),
    age = c("ANB", "ALB"),
    type = c("Composite", "Non-Smoker", "Smoker")
  )
)

createCSO2001 = function(
  file, sheet, skip = 2,
  sex = "m", collar = "Composite", ageType,
  age.col = "Iss. Age", rm.cols = c("Att. Age"),
  ...
) {
  Sx = recode(sex, "m" = "Male", "f" = "Female")
  name = sprintf("2001 CSO %s %s %s", Sx, collar, ageType)
  if(missing(sheet)) {
    if (collar == "Composite") {
      sheet = sprintf("2001 %s %s %s", Sx, collar, ageType)
    } else {
      sheet = sprintf("2001 %s%s %s", toupper(sex), recode(collar, "Smoker" = "SM", "Non-Smoker" = "NS"), ageType)
    }
  }
  createUSSelectTable(
    file = file, sheet = sheet, skip = skip,
    age.col = age.col, rm.cols = rm.cols,
    name = name, year = 2001, scale = 0.001,
    table = sprintf("2001 CSO %s", collar), sex = sex, collar = collar, type = "CSO", country = "USA", ageType = ageType,
    ...)
}

# plotMortalityTables(CSO2001) + facet_grid(.~ageType)
#-----------------------------------------------------------------#
### Primary Tables: M/F Comp/NS/SM ANB                         ####
#-----------------------------------------------------------------#

file = here::here("data-raw", "US", "CSO", "2001 CSO", "Table 2001 VBT CSO.xls")
for (sex in c("m", "f")) {
  Sx = recode(sex, "m" = "Male", "f" = "Female")
  for (ageType in c("ANB")) {
    for (type in c("Composite", "Non-Smoker", "Smoker")) {
      sheet = sprintf("CSO %s%s", toupper(sex), recode(type, "Composite" = "Comp", "Smoker" = "SM", "Non-Smoker" = "NS"))
      CSO2001[[sex, ageType, type]] = createCSO2001(
        file = file, sheet = sheet,
        age.col = "2001 Valuation Basic Table and  2001 CSO Table", rm.cols = "Age",
        sex = sex, collar = type, ageType = ageType
      )
    }
  }
}

file = here::here("data-raw", "US", "CSO", "2001 CSO", "CSO_taskforce_appendix_j3_june2002.xls")
for (sex in c("m", "f")) {
  Sx = recode(sex, "m" = "Male", "f" = "Female")
  for (ageType in c("ALB")) {
    for (type in c("Composite", "Non-Smoker", "Smoker")) {
      sheet = sprintf("(%s) %s S&U %s", toupper(sex), type, ageType)
      CSO2001[[sex, ageType, type]] = createCSO2001(
        file = file, sheet = sheet, skip = 1,
        age.col = "Issue Age", rm.cols = "Attained Age",
        sex = sex, collar = type, ageType = ageType
      )
    }
  }
}

# Gender-blended tables
file = here::here("data-raw", "US", "CSO", "2001 CSO", "CSO_taskforce_appendix_j3_june2002.xls")
for (sex in c("80% Male", "60% Male", "50% Male", "40% Male", "20% Male")) {
  Sx = substr(sex, 1, 2)
  for (ageType in c("ANB", "ALB")) {
    for (type in c("Composite", "Non-Smoker", "Smoker")) {
      sheet = sprintf("(%s) %s S&U %s", Sx, type, ageType)
      CSO2001[[sex, ageType, type]] = createCSO2001(
        file = file, sheet = sheet, skip = 1,
        age.col = "Issue Age", rm.cols = "Attained Age",
        sex = sex, collar = type, ageType = ageType
      )
    }
  }
}



# plotMortalityTables(CSO2001) + facet_grid(ageType ~ collar) + aes(color = sex)
# plotMortalityTables(CSO2001) + facet_grid(sex ~ ageType) + aes(color = collar)

save(CSO2001, file = CSO2001.file.out)


#-----------------------------------------------------------------#
### Preferred 2001 CSO Tables (only loaded)                    ####
#-----------------------------------------------------------------#

CSO2001.Preferred = array(
  data = c(mortalityTable.NA),
  dim = c(3, 2, 2, 2),
  dimnames = list(
    preferred = c("Super Preferred", "Preferred", "Residual"),
    sex = c("m", "f"),
    age = c("ANB", "ALB"),
    type = c("Non-Smoker", "Smoker")
  )
)

createCSO2001.Preferred = function(
  file, sheet, skip = 6,
  sex = "m", collar = "Composite", ageType, Preferred = "Preferred",
  age.col = "...1", rm.cols = c("Age"),
  ...
) {
  # browser()
  Sx = recode(sex, "m" = "Male", "f" = "Female")
  name = sprintf("2001 CSO %s %s - %s %s", Preferred, ageType, Sx, collar)
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
    name = name, year = 2001, scale = 0.001,
    table = sprintf("2001 CSO %s", collar), sex = sex, collar = collar, type = "CSO", country = "USA", ageType = ageType, Preferred = Preferred,
    ...)
}


file = here::here("data-raw", "US", "CSO", "2001 CSO Preferred", "2001 CSO Preferred Class Structure Mortality Tables.xls")
for (preferred in c("Super Preferred", "Preferred", "Residual")) {
  for (sex in c("m", "f")) {
    for (ageType in c("ANB", "ALB")) {
      for (type in c("Smoker", "Non-Smoker")) {
        # There are no super-preferred smokers => skip that case
        if (preferred != "Super Preferred" || type != "Smoker") {
          # Sheet naming is defined in the tab "Table of Contents": A.[1234].[ab].{i,ii,iii,iv,v}
          sheet = sprintf("A.%d.%s.%s", recode(ageType, "ANB" = 1, "ALB" = 3), recode(sex, "m" = "a", "f" = "b"), recode(
            paste(preferred, type),
            "Super Preferred Non-Smoker" = "i",
            "Preferred Non-Smoker" = "ii",
            "Residual Non-Smoker" = "iii",
            "Preferred Smoker" = "iv",
            "Residual Smoker" = "v"))
          CSO2001.Preferred[[preferred, sex, ageType, type]] = createCSO2001.Preferred(
            file = file, sheet = sheet,
            sex = sex, collar = type, ageType = ageType, Preferred = preferred
          )
        }
      }
    }
  }
}


# plotMortalityTables(CSO2001.Preferred[,,"ANB",], legend.position = "bottom") + facet_grid(sex ~ collar) + aes(color = Preferred)

save(CSO2001.Preferred, file = here::here("data", "CSO2001.Preferred.RData"))





#############################################################################h#
# USA 2017 CSO Tables                                                      ####
#############################################################################h#

CSO2017file.out = here::here("data", "CSO2017.RData")
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
# plotMortalityTables(CSO2017.Preferred[,,"ANB",,]) + facet_grid(sex ~ collar) + aes(linetype = loaded, color = Preferred)


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







