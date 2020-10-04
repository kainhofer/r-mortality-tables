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
### Observed three-year National Life Tables 1980-1982 to 2017-2019
###############################################################################

ukfile.out = here("data", "mort.UK.national.RData")

ukfile = here("data-raw", "UK", "Population", "nationallifetables3yearuk.xlsx")
gbfile = here("data-raw", "UK", "Population", "nationallifetables3yeargb.xlsx")
engwalesfile = here("data-raw", "UK", "Population", "nationallifetables3yearew.xlsx")
englandfile = here("data-raw", "UK", "Population", "nationallifetables3yeareng.xlsx")
walesfile = here("data-raw", "UK", "Population", "nationallifetables3yearwal.xlsx")
scotlandfile = here("data-raw", "UK", "Population", "nationallifetables3yearsco.xlsx")
northirelandfile = here("data-raw", "UK", "Population", "nationallifetables3yearni.xlsx")

mort.UK.national = array(
  data = c(mortalityTable.NA),
  dim = c(7,2),
  dimnames = list(Area = c("UK", "Great Britain", "England & Wales", "England", "Wales", "Scotland", "Northern Ireland"), Sex = c("m", "f"))
)


loadUKdata = function(filename, area, years = 1981:2018) {
  ages = 0:100
  deathProbs = data.frame()
  for (y in years) {
    deathProbs = bind_rows(
      deathProbs,
      read_excel(filename, sheet = paste0(y-1, "-", y+1), range = "A8:I108",
                 col_types = c("numeric", "skip", "numeric", rep("skip", 5), "numeric"),
                 col_names = c("age", "male", "female")) %>%
        gather(key = "sex", value = "qx", male, female) %>%
        mutate(year = y, area = area)
    )
  }
  deathProbs
}

data.UK = bind_rows(
  loadUKdata(ukfile, area = "UK"),
  loadUKdata(gbfile, area = "Great Britain"),
  loadUKdata(engwalesfile, area = "England & Wales"),
  loadUKdata(englandfile, area = "England"),
  loadUKdata(walesfile, area = "Wales"),
  loadUKdata(scotlandfile, area = "Scotland"),
  loadUKdata(northirelandfile, area = "Northern Ireland")
)


censtable = function(data, area = "UK", sex = "male") {
  sx = recode(sex, "male" = "m", "female" = "f")
  deathProbs = filter(data, area == !!area, sex == !!sex) %>%
    select(age, qx, year) %>%
    spread(year, qx)
  ages = deathProbs$age
  deathProbs = select(deathProbs, -age)
  years = as.numeric(colnames(deathProbs))
  name = paste0("National Life Tables ", area, " ", sex)
  tbl = mortalityTable.observed(
    name = name,
    ages = ages, deathProbs = deathProbs,
    years = years,
    data = list(
      dim = list(
        table = paste("National Life Table", area),
        sex = sx,
        collar = "Population",
        type = "Observed Mortality",
        country = area,
        data = "official",
        year = "1980-1982 to 2017-2019"
      )
    )
  )
  mort.UK.national[[area, sx]] = tbl
  tbl
}



mort.UK.national.UK.male = censtable(data.UK, area = "UK", sex = "male")
mort.UK.national.UK.female = censtable(data.UK, area = "UK", sex = "female")
mort.UK.national.GB.male = censtable(data.UK, area = "Great Britain", sex = "male")
mort.UK.national.GB.female = censtable(data.UK, area = "Great Britain", sex = "female")
mort.UK.national.EW.male = censtable(data.UK, area = "England & Wales", sex = "male")
mort.UK.national.EW.female = censtable(data.UK, area = "England & Wales", sex = "female")
mort.UK.national.Eng.male = censtable(data.UK, area = "England", sex = "male")
mort.UK.national.Eng.female = censtable(data.UK, area = "England", sex = "female")
mort.UK.national.Wales.male = censtable(data.UK, area = "Wales", sex = "male")
mort.UK.national.Wales.female = censtable(data.UK, area = "Wales", sex = "female")
mort.UK.national.Sco.male = censtable(data.UK, area = "Scotland", sex = "male")
mort.UK.national.Sco.female = censtable(data.UK, area = "Scotland", sex = "female")
mort.UK.national.NI.male = censtable(data.UK, area = "Northern Ireland", sex = "male")
mort.UK.national.NI.female = censtable(data.UK, area = "Northern Ireland", sex = "female")


save(
  mort.UK.national,

  mort.UK.national.UK.male,
  mort.UK.national.UK.female,
  mort.UK.national.GB.male,
  mort.UK.national.GB.female,
  mort.UK.national.EW.male,
  mort.UK.national.EW.female,
  mort.UK.national.Eng.male,
  mort.UK.national.Eng.female,
  mort.UK.national.Wales.male,
  mort.UK.national.Wales.female,
  mort.UK.national.Sco.male,
  mort.UK.national.Sco.female,
  mort.UK.national.NI.male,
  mort.UK.national.NI.female,

  file = ukfile.out
)




