#############################################################################m#
#  Generate German (compulsory) private health insurance mortality tables  ####
#############################################################################m#
library(MortalityTables)
library(here)
library(readxl)

PKVSterbetafel.file.out = here::here("data", "PKVSterbetafel.RData")

years = c(2004, 2007:2021)

PKVSterbetafel = array(
  data = c(mortalityTable.NA),
  dim = c(2, length(years)),
  dimnames = list(Geschlecht = c("m", "w"), Jahr = as.character(years))
)


generatePKVtable = function(year) {
  data = read_excel(here::here("data-raw", "DE", "PKV", sprintf("dl_st_%04g_pkv_sterbetafel_va.xls", year)), skip = 1)

  PKVSterbetafel[["m", as.character(year)]] <<- mortalityTable.period(
    name = paste("PKV-Sterbetafel", year, "Männer"),
    ages = data$Alter,
    deathProbs = data$qx,
    baseYear = year,
    data = list(
      dim = list(table = paste("PKV-Sterbetafel", year), sex = "m", collar = "PKV", type = "PKV-Sterbetafel", country = "Deutschland", data = "official", year = year)
    )
  )

  PKVSterbetafel[["w", as.character(year)]] <<- mortalityTable.period(
    name = paste("PKV-Sterbetafel", year, "Frauen"),
    ages = data$Alter,
    deathProbs = data$qy,
    baseYear = year,
    data = list(
      dim = list(table = paste("PKV-Sterbetafel", year), sex = "w", collar = "PKV", type = "PKV-Sterbetafel", country = "Deutschland", data = "official", year = year)
    )
  )

  return()
}

sapply(years, generatePKVtable)


save(
  PKVSterbetafel,
  file = PKVSterbetafel.file.out
)

