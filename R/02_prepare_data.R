library(tidyverse)
library(lubridate)

source("R/libmath.R")
source("R/01_load_data.R")
options(dplyr.summarise.inform = FALSE)

.rds.path <- "rds/"

# Combine ems, ages and bmsgpk data
ems.ages.bmsgpk.timeline <- purrr::reduce(
  list(
    ages.timeline |> select(Datum = Testdatum, Bundesland, Einwohner, ages.neu = Faelle.neu),
    ems.timeline |> select(Datum = Meldedatum, Bundesland, ems.neu = Faelle.neu)
    #bmsgpk.timeline |> select(Datum = Meldedatum, Bundesland, bmsgpk.neu = Faelle.neu)
  ),
  dplyr::full_join,
  by = c("Datum", "Bundesland")
) |>
  group_by(Bundesland) |>
  mutate(
    Einwohner = case_when(
      is.na(Einwohner) ~ lag(Einwohner, 2),
      TRUE ~ Einwohner
    ),
    ages.inc = notification_rate_7d(ages.neu, Einwohner),
    ems.inc = notification_rate_7d(ems.neu, Einwohner)
    #bmsgpk.inc = notification_rate_7d(bmsgpk.neu, Einwohner)
  )
saveRDS(ems.ages.bmsgpk.timeline, file = paste0(.rds.path, "ems_ages_bmsgpk_timeline.rds"))


###################
#Estimate R
###################

message("Estimate R may take a while")

message("Estimate ages.estimate_r.österreich.gesamt")
ages.estimate_r.österreich.gesamt <- ages.timeline |>
  estimate_r(cases = Faelle.neu, date = Testdatum, groupby = Bundesland, intervall = 7)
saveRDS(ages.estimate_r.österreich.gesamt, file = paste0(.rds.path, "ages_reff_österreich_gesamt.rds"))

message("Estimate ems.estimate_r.österreich.gesamt ausgesetzt")
#ems.estimate_r.österreich.gesamt <- ems.timeline |>
#  estimate_r(cases = Faelle.neu, date = Meldedatum, groupby = Bundesland, intervall = 7)
#saveRDS(ems.estimate_r.österreich.gesamt, file = paste0(.rds.path, "ems_reff_österreich_gesamt.rds"))

message("Estimate ages.estimate_r.österreich.altersgruppe.gesamt")
ages.estimate_r.österreich.altersgruppe.gesamt <- ages.altersgruppe |> 
  # summarise Geschlecht
  group_by(Testdatum, Bundesland, Altersgruppe) |> 
  summarise(Faelle.neu = sum(Faelle.neu))|>
  # filter Österreich
  filter(Bundesland == "Österreich") |>
  # estimate R
  estimate_r(cases = Faelle.neu, date = Testdatum, groupby = Altersgruppe, intervall = 7)
saveRDS(ages.estimate_r.österreich.altersgruppe.gesamt, file = paste0(.rds.path, "ages_reff_österreich_altersgruppe_gesamt.rds"))

#####################
# Timeline
#####################

timeline.covid <- tribble(
  ~event, ~subgroup, ~group, ~start, ~end,
  "1. Lockdown full",     "full",  "Lockdowns", "2020-03-16", "2020-04-14",
  "1. Lockdown half",     "light", "Lockdowns", "2020-04-14", "2020-05-01",
  "2. Lockdown light",    "light", "Lockdowns", "2020-11-03", "2020-11-17",
  "2.Lockdown full",      "full",  "Lockdowns", "2020-11-17", "2020-12-07",
  "2.lockdown light",     "light", "Lockdowns", "2020-12-07", "2020-12-25",
  "3. Lockdown",          "full",  "Lockdowns", "2020-12-26", "2021-02-21",
  "Lockdown, W, Bgld, NÖ","light", "Lockdowns", "2021-04-01", "2021-05-03",
  "4. Lockdown",          "full",  "Lockdowns", "2021-11-21", "2021-12-11"
) 
saveRDS(timeline.covid, file = paste0(.rds.path, "timeline_covid.rds"))

