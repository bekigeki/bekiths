library(readxl)
library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)
library(tidyr)
library(bekiths)

path <- system.file("data-raw", "schulstatistik-thueringen.de", package = "bekiths")


# Position Spalten in xlsx
cols <- c(
    Schuljahr        = 1,
    Schulnummer      = 7,
    Schulname        = 8,
    N_Schule =  9,
    N_Primarbereich = 10,
    N_K01 = 11,
    N_K02 = 12,
    N_K03 = 13,
    N_K04 = 14, 
    N_Sekundarbereich_1 = 15,
    N_K05 = 16,
    N_K06 = 17,
    N_K07 = 18,
    N_K08 = 19,
    N_K09 = 20,
    N_K10 = 21,
    N_Sekundarbereich_2 = 22,
    N_K11 = 23,
    N_K12 = 24,
    N_K13 = 25,
    N_Einfuehrungsphase = 26,
    N_Q01 = 27,
    N_Q02 = 28,
    N_Geistige_Entwicklung = 29
)




# Extract Stichtags-Datum und Download-Datum
meta_specs <- list(
  Stichtag = list(                   
    col = 1,
    pattern = "Stichtag:\\s*([^,]+)",
    group=1
  ),
  erstellt_am = list(                     
    col = 1,
    pattern = "erstellt am\\s*([^/]+) ",
    group=1
  )
)

# read and bind raw data
df <- read_schools(
  path = path, 
  skip=10,
  cols = cols,
  meta_specs = meta_specs)

# Spaltentypisierung
bekiths_data <- df |>
  mutate(
    across(starts_with("N_"), ~ as.integer(.x)), 
    Stichtag = as.POSIXct(
      Stichtag,
      format = "%d.%m.%Y",
      tz = "Europe/Berlin"),
    erstellt_am = as.POSIXct(
      erstellt_am,
      format = "%d.%m.%Y %H:%M",
      tz = "Europe/Berlin"))

# save rds for package

usethis::use_data(bekiths_data, overwrite = TRUE)

