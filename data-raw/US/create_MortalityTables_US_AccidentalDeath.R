#############################################################################m#
#  Skript to generate US Accidental death benefits table objects               ####
#############################################################################m#
library(MortalityTables)
library(here)
library(readxl)




#############################################################################h#
# 1926-33 Intercompany double indemnity table                              ####
#############################################################################h#

accDeathfile = here::here("data-raw", "US", "AccidentalDeath", "US_AccidentalDeath_1926_1951Exp_1959.xlsx")
accDeathfile.out = here::here("data", "US.AccDeath.1959.RData")

accDeath.data = read_excel(accDeathfile, skip = 3)

US.AccDeath.1926 = mortalityTable.period(
  name = "US 1926-33 Intercompany Double Indemnity Table for Accidental Death Benefits",
  ages = accDeath.data$age,
  deathProbs = accDeath.data$`1926-33 Table`,
  baseYear = 1930,
  data = list(
    dim = list(table = "1926-33 Accidental Death Benefits", sex = "m", collar = "AccidentalDeath", type = "AccidentalDeathBenefits", country = "USA", data = "official", year = 1930)
  )
)

US.AccDeath.1951.56.Experience = mortalityTable.period(
  name = "US 1951-56 Accidental Death Benefits Experience",
  ages = accDeath.data$age,
  deathProbs = accDeath.data$`1951-56 Experience`,
  baseYear = 1953,
  data = list(
    dim = list(table = "1951-56 Accidental Death Benefits Experience", sex = "m", collar = "AccidentalDeath", type = "AccidentalDeathBenefits", country = "USA", data = "official", year = 1953)
  )
)

US.AccDeath.1959 = mortalityTable.period(
  name = "US 1959 Accidental Death Benefits Table",
  ages = accDeath.data$age,
  deathProbs = accDeath.data$`1959 Table`,
  baseYear = 1959,
  data = list(
    dim = list(table = "1959 Accidental Death Benefits Table", sex = "m", collar = "AccidentalDeath", type = "AccidentalDeathBenefits", country = "USA", data = "official", year = 1959)
  )
)

save(
  US.AccDeath.1926, US.AccDeath.1951.56.Experience, US.AccDeath.1959,
  file = accDeathfile.out
)


