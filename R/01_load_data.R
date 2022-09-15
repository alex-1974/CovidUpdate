library(tidyverse)
library(lubridate)
library(purrr)
library(zoo)

bundesländer.fct <- c("Wien", "Niederösterreich", "Oberösterreich", "Burgenland", "Steiermark", "Tirol", "Kärnten", "Vorarlberg", "Salzburg", "Österreich")
altersgruppe.fct <- c("<5", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", ">84")
sex.fct <- c("m", "w")

.path <- "data/"

ages.timeline <- read_csv(paste0(.path, "ages.timeline.csv")) |>
  mutate(
    Bundesland = factor(Bundesland, levels = bundesländer.fct, ordered = TRUE)
  )
ages.altersgruppe <- read_csv(paste0(.path, "ages.altersgruppe.csv")) |> 
  mutate(
    Bundesland = factor(Bundesland, levels = bundesländer.fct, ordered = TRUE),
    Altersgruppe = factor(Altersgruppe, levels = altersgruppe.fct, ordered = TRUE),
    Geschlecht = factor(Geschlecht, levels = sex.fct)
  )
ems.timeline <- read_csv(paste0(.path, "ems.timeline.csv")) |>
  mutate(
    Bundesland = factor(Bundesland, levels = bundesländer.fct, ordered = TRUE)
  )
bmsgpk.timeline <- read_csv(paste0(.path, "BMSGPK.timeline.csv")) |>
  mutate(
    Bundesland = factor(Bundesland, levels = bundesländer.fct, ordered = TRUE)
  )
ages.hospitalisierung <- read_csv(paste0(.path, "ages.hospitalisierung.csv")) |>
  mutate(
    Bundesland = factor(Bundesland, levels = bundesländer.fct, ordered = TRUE)
  )
ages.reff.österreich <- read_csv(paste0(.path, "ages.reff.österreich.csv")) 
ages.reff.bundesländer <- read_csv(paste0(.path, "ages.reff.bundesländer.csv")) |>
  mutate(
    Bundesland = factor(Bundesland, levels = bundesländer.fct, ordered = TRUE)
  )

.ages.timeline.at <- ages.timeline |>
  filter(
    Bundesland == "Österreich"
  ) |>
  select(Datum = Testdatum, Einwohner, ages.neu = Faelle.neu)

.ems.timeline.at <- ems.timeline |>
  filter(
    Bundesland == "Österreich"
  ) |>
  select(Datum = Meldedatum, ems.neu = Faelle.neu)

.bmsgpk.timeline.at <- bmsgpk.timeline |>
  filter(
    Bundesland == "Österreich"
  ) |>
  select(Datum = Meldedatum, bmsgpk.neu = Faelle.neu)

