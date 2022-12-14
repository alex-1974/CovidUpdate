library(tidyverse)
library(lubridate)
library(ggpubr)

source("R/libmath.R")

period.from <- rollback(add_with_rollback(now(),months(-1)), roll_to_first = TRUE)
now2 <- add_with_rollback(now(),days(+1))
img.path <- "img"
.rds.path <- "rds/"

source("R/01_load_data.R")
ems.ages.bmsgpk.timeline <- readRDS(paste0(.rds.path, "ems_ages_bmsgpk_timeline.rds"))
ages.estimate_r.österreich.gesamt <- readRDS(paste0(.rds.path, "ages_reff_österreich_gesamt.rds"))
#ems.estimate_r.österreich.gesamt <-readRDS(paste0(.rds.path, "ems_reff_österreich_gesamt.rds"))
ages.estimate_r.österreich.altersgruppe.gesamt <- readRDS(paste0(.rds.path, "ages_reff_österreich_altersgruppe_gesamt.rds"))
timeline.covid <- readRDS(paste0(.rds.path, "timeline_covid.rds"))

####################
# Inzidenzen
####################

ems.ages.bmsgpk.timeline.österreich <- ems.ages.bmsgpk.timeline |> filter(Bundesland == "Österreich")

ages.inz7d.österreich.gesamt <- ems.ages.bmsgpk.timeline |>
  mutate(
    Faelle = incidence.7days(cases = ages.neu, pop = Einwohner)
  ) |>
  select(Datum, Faelle)
tail(ages.inz7d.österreich.gesamt)

ages.inz7d.bundesländer.gesamt<- ages.timeline |>
  filter(
    Bundesland != "Österreich"
  ) |>
  group_by(Bundesland) |> 
  mutate(
    Faelle = incidence.7days(cases = Faelle.neu, pop = Einwohner)
  ) |>
  select(Testdatum,Bundesland,Faelle) 

######################
# Nach Geschlecht
######################

ages.sex.bundesländer <- ages.altersgruppe |>
  filter(
    Testdatum == max(Testdatum)
  ) |>
  select(Bundesland,Einwohner, Geschlecht, Faelle.cumsum) |>
  group_by(Bundesland,Geschlecht) |>
  mutate(
    Einwohner = sum(Einwohner),
    Faelle.cumsum = sum(Faelle.cumsum)
  ) |>
  unique() |> 
  group_by(Bundesland) |>
  mutate(
    sum.infected = sum(Faelle.cumsum),
    ratio = 100 / sum.infected * Faelle.cumsum
  )

###################
# Tests
###################

bmsgpk.test <- purrr::reduce(
  # kombiniere bmsgpk.timeline mit ages.timeline um die Einwohnerzahl 
  # für die Inzidenzenberechnung zu erhalten.
  list(
    bmsgpk.timeline |> select(Datum = Meldedatum, Bundesland, Tests.gesamt.neu, Tests.AG.neu, Tests.PCR.neu),
    ages.timeline |> select(Datum = Testdatum, Bundesland, Einwohner, Faelle.neu)
  ),
  dplyr::right_join,
  by = c("Datum", "Bundesland")
) |>
  select(Datum, Bundesland, Einwohner, Tests.gesamt.neu, Tests.AG.neu, Tests.PCR.neu, Faelle.neu) |>
  filter(
    if_all(
    .cols = starts_with("Tests"),
    .fns = ~ !is.na(.x)
  )) |>
  group_by(Bundesland) |>
  mutate(
    Positivrate.PCR = (Faelle.neu / Tests.PCR.neu) * 100,
    Positivrate.PCR.sma7 = zoo::rollmean(Positivrate.PCR, 7, align = "right", fill = 0),
    Testrate.gesamt = (Tests.gesamt.neu / Einwohner) * 100,
    Testrate.gesamt.sma7 = zoo::rollmean(Testrate.gesamt, 7, align = "right", fill = 0),
    Testrate.gesamt.inz7d = notification_rate_7d(cases = Faelle.neu, pop = Einwohner),
    Testrate.PCR.gesamt = (Tests.PCR.neu / Einwohner) * 100,
    Testrate.PCR.sma7 = zoo::rollmean(Testrate.PCR.gesamt, 7, align = "right", fill = 0),
    Tests.gesamt.sma7 = zoo::rollmean(Tests.gesamt.neu, 7, align = "right", fill = 0)
  )
bmsgpk.test.österreich <- bmsgpk.test |> filter(Bundesland == "Österreich") 

###################
# Native R by RKI
###################

ages.timeline.reff <- ages.timeline |>
  select(Testdatum, Bundesland, Faelle.neu) |>
  group_by(Bundesland) |>
  mutate(
    r4 = reff.rki(Faelle.neu, 4),
    r4.now = reff.rki(Faelle.neu, 4, drop.last = FALSE),
    r7 = reff.rki(Faelle.neu,4),
    r7.now = reff.rki(Faelle.neu, 4, drop.last = FALSE),
  )

source("R/05_01_process_data_tables.R")
source("R/05_02_process_data_graphs.R")


