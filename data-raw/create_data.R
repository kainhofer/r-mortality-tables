
library(here)

# AUSTRIA:
source(here("data-raw", "AT", "Austria_Population2017_HMD_StatistikAustria_prepare.R"))
source(here("data-raw", "AT", "create_MortalityTables_Austria_Population.R"))
source(here("data-raw", "AT", "create_MortalityTables_Austria_Annuities.R"))

source(here("data-raw", "DE", "create_MortalityTables_Germany_Annuities.R"))
source(here("data-raw", "DE", "create_MortalityTables_Germany_MTPL.R"))

source(here("data-raw", "UK", "create_MortalityTables_UK_Population.R"))
