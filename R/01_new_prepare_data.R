library(tidyverse)
library(lubridate)
library(stringr)

source("R/config.R")
source("R/libmath.R")

# AGES timeline
ages.timeline <- ages.timeline |>
  mutate(
    Testdatum = as_date(dmy_hms(Testdatum)),
    Bundesland = factor(Bundesland, levels = bundesländer.fct, ordered = TRUE)
  ) |>
  group_by(Bundesland) |>
  mutate(
    Faelle.cumsum = Faelle.neu + lag(Faelle.neu, default = 0, order_by = Testdatum),
    Genesen.cumsum = Genesen.neu + lag(Genesen.neu, default = 0, order_by = Testdatum),
    Gestorben.cumsum = Gestorben.neu + lag(Gestorben.neu, default = 0, order_by = Testdatum),
    Faelle.inz7d = notification_rate_7d(Faelle.neu, pop = Einwohner),
    Genesen.inz7d = notification_rate_7d(Genesen.neu, pop = Einwohner),
    Gestorben.inz7d = notification_rate_7d(Gestorben.neu, pop = Einwohner),
    Faelle.inz7d.rate = Faelle.inz7d / lag(Faelle.inz7d, 7),
    Erkrankt.Genesen.rate = Faelle.inz7d / (Genesen.inz7d+Gestorben.inz7d)
  )
saveRDS(ages.timeline, str_glue(path.data, "ages.timeline.RDS"), compress = "xz")

# AGES altersgruppe
ages.altersgruppe <- ages.altersgruppe |> 
  mutate(
    Testdatum = as_date(dmy_hms(Testdatum)), 
    Bundesland = factor(Bundesland, levels = bundesländer.fct, ordered = TRUE),
    Altersgruppe = factor(Altersgruppe, levels = altersgruppe.fct, ordered = TRUE),
    Geschlecht = factor(str_to_lower(Geschlecht), levels = sex.fct)
  ) |>
  group_by(Altersgruppe, Bundesland, Geschlecht) |>
  mutate(
    Faelle.neu = Faelle.cumsum - lag(Faelle.cumsum, order_by = Testdatum),
    Genesen.neu = Genesen.cumsum - lag(Genesen.cumsum, order_by = Testdatum),
    Gestorben.neu = Gestorben.cumsum - lag(Gestorben.cumsum, order_by = Testdatum),
    Faelle.inz7d = notification_rate_7d(Faelle.neu, pop = Einwohner),
    Genesen.inz7d = notification_rate_7d(Genesen.neu, pop = Einwohner),
    Gestorben.inz7d = notification_rate_7d(Gestorben.neu, pop = Einwohner),
    Faelle.inz7d.rate = Faelle.inz7d / lag(Faelle.inz7d, 7),
    Erkrankt.Genesen.rate = Faelle.inz7d / (Genesen.inz7d+Gestorben.inz7d)
  ) 
saveRDS(ages.altersgruppe, str_glue(path.data, "ages.altersgruppe.RDS"), compress = "xz")

#####################################
# Hospitalisierung
#
# Datenquellen:
# * ages.fallzahlen
# * ages.hospitalisierung
# Die 'erweiterbare Kapazität' sind zusätzlich verfügbare Betten gewidmet für COVID-19 PatientInnen, 
# die aus Sicht des Berichtstages binnen 7 Tagen zu Verfügung stehen könnten. 
# Für Wien liegt aufgrund einer unterschiedlichen Dateneinmeldung diese Information nicht vor 
#####################################


# AGES fallzahlen
ages.fallzahlen <- ages.fallzahlen |>
  mutate(
    Meldedatum = as_date(dmy(Meldedatum)),
    Bundesland = if_else(str_to_lower(Bundesland) == "alle", "Österreich", Bundesland),
    Bundesland = factor(Bundesland, levels = bundesländer.fct, ordered = TRUE)
  ) |>
  group_by(Bundesland) |>
  mutate(
    ICU.gesamt.cov19 = ICU.belegt.cov19 + ICU.verfügbar.cov19
  )
saveRDS(ages.fallzahlen, str_glue(path.data, "ages.fallzahlen.RDS"), compress = "xz")

# AGES hospitalisierung
ages.hospitalisierung <- ages.hospitalisierung |>
  mutate(
    Meldedatum = as_date(dmy_hms(Meldedatum)),
    Bundesland = factor(Bundesland, levels = bundesländer.fct, ordered = TRUE)
  ) 
saveRDS(ages.hospitalisierung, str_glue(path.data, "ages.hospitalisierung.RDS"), compress = "xz")

# Combine ages.fallzahlen and ages.hospitalisierung
hospitalization.combined <- purrr::reduce(
  list(
    ages.timeline |> select(Datum = Testdatum, Bundesland, Einwohner),
    ages.fallzahlen |> select(Datum = Meldedatum, Bundesland, Normalbetten.belegt.cov19, Normalbetten.verfügbar.cov19, ICU.verfügbar.cov19),
    ages.hospitalisierung |> select(Datum = Meldedatum, Bundesland, ICU.belegt.cov19, ICU.belegt.non_cov19, ICU.frei, ICU.kapazität)
  ),
  dplyr::full_join,
  by = c("Datum", "Bundesland")
  ) |>
  filter(
    if_all(
      .cols = starts_with(c("ICU.","Normalbetten.")),
      .fns = ~ !is.na(.x)
    )) |>
  group_by(Bundesland) |>
  mutate(
    Einwohner = if_else(is.na(Einwohner), lag(Einwohner, 1, default = NA, order_by = Datum), Einwohner),
    ICU.Auslastung.cov19 = (100 / ICU.kapazität) * ICU.belegt.cov19
  )
tail(hospitalization.combined)
saveRDS(hospitalization.combined, str_glue(path.data, "hospitalization.combined.RDS"), compress = "xz")

# BMSGPK timeline (Testing data)
bmsgpk.timeline <- bmsgpk.timeline |>
  mutate(
    Meldedatum = as_date(ymd_hms(Meldedatum)),
    Bundesland = factor(Bundesland, levels = bundesländer.fct, ordered = TRUE)
  ) |>
  group_by(Bundesland) |>
  mutate(
    #Faelle.neu = Faelle.cumsum - lag(Faelle.cumsum, order_by = Meldedatum),
    Tests.gesamt.neu = Tests.gesamt.cumsum - lag(Tests.gesamt.cumsum, order_by = Meldedatum),
    Tests.AG.neu = Tests.AG.cumsum - lag(Tests.AG.cumsum, order_by = Meldedatum),
    Tests.PCR.neu = Tests.PCR.cumsum - lag(Tests.PCR.cumsum, order_by = Meldedatum)
  )
saveRDS(bmsgpk.timeline, str_glue(path.data, "bmsgpk.timeline.RDS"), compress = "xz")

testing.combined <- purrr::reduce(
  list(
    bmsgpk.timeline |> rename(Datum = Meldedatum),
    ages.timeline |> select(Datum = Testdatum, Bundesland, Einwohner, Faelle.neu)
  ),
  dplyr::left_join,
  by = c("Datum", "Bundesland")
  ) |>
  filter(
    if_all(
      .cols = starts_with("Tests"),
      .fns = ~ !is.na(.x)
    )) |>
  group_by(Bundesland) |>
  mutate(
    Einwohner = if_else(is.na(Einwohner), lag(Einwohner, 1, default = NA, order_by = Datum), Einwohner),
    Positivrate.PCR = (Faelle.neu / Tests.PCR.neu) * 100,
    Positivrate.PCR.sma7 = zoo::rollmean(Positivrate.PCR, 7, align = "right", fill = 0),
    Testrate.gesamt = (Tests.gesamt.neu / Einwohner) * 100,
    Testrate.gesamt.sma7 = zoo::rollmean(Testrate.gesamt, 7, align = "right", fill = 0),
    Testrate.gesamt.inz7d = notification_rate_7d(cases = Faelle.neu, pop = Einwohner),
    Testrate.PCR.gesamt = (Tests.PCR.neu / Einwohner) * 100,
    Testrate.PCR.sma7 = zoo::rollmean(Testrate.PCR.gesamt, 7, align = "right", fill = 0),
    Tests.gesamt.sma7 = zoo::rollmean(Tests.gesamt.neu, 7, align = "right", fill = 0)
  )
tail(testing.combined)
saveRDS(testing.combined, str_glue(path.data, "testing.combined.RDS"), compress = "xz")

  
bmsgpk.vaccination <- bmsgpk.vaccination |>
  mutate(
    Meldedatum = as_date(ymd_hms(Meldedatum)),
    Bundesland = factor(Bundesland, levels = bundesländer.fct, ordered = TRUE),
    Impfstoff = factor(Impfstoff, levels = vaccination.fct, ordered = TRUE),
    Dosis = factor(Dosis, ordered = TRUE)
  ) |>
  group_by(Bundesland,Impfstoff,Dosis) |>
  mutate(
    Anzahl.neu = Anzahl.cumsum - lag(Anzahl.cumsum, default = 0, order_by = Meldedatum),
  ) 
saveRDS(bmsgpk.vaccination, str_glue(path.data, "bmsgpk.vaccination.RDS"), compress = "xz")

vaccination.combined <- purrr::reduce(
  list(
    bmsgpk.vaccination |> rename(Datum = Meldedatum),
    ages.timeline |> select(Datum = Testdatum, Bundesland, Einwohner)
  ),
  dplyr::full_join,
  by = c("Datum", "Bundesland")
) |>
  group_by(Bundesland) |>
  mutate(
    Einwohner = if_else(is.na(Einwohner), lag(Einwohner, 1, default = NA, order_by = Datum), Einwohner)
  ) 
tail(vaccination.combined)
saveRDS(vaccination.combined, str_glue(path.data, "vaccination.combined.RDS"), compress = "xz")

ages.reff.österreich <- ages.reff.österreich |>
  mutate(
    Datum = as_date(ymd(Datum)),
    R_eff = as.numeric(gsub(",", ".", R_eff)),
    R_eff_lwr = as.numeric(gsub(",", ".", R_eff_lwr)),
    R_eff_upr = as.numeric(gsub(",", ".", R_eff_upr))
  ) 
saveRDS(ages.reff.österreich, str_glue(path.data, "ages.reff.österreich.RDS"), compress = "xz")

ages.reff.bundesländer <- ages.reff.bundesländer |>
  mutate(
    Datum = as_date(ymd(Datum)),
    Bundesland = factor(Bundesland, levels = bundesländer.fct, ordered = TRUE),
    R_eff = as.numeric(gsub(",", ".", R_eff)),
    R_eff_lwr = as.numeric(gsub(",", ".", R_eff_lwr)),
    R_eff_upr = as.numeric(gsub(",", ".", R_eff_upr))
  ) 
saveRDS(ages.reff.bundesländer, str_glue(path.data, "ages.reff.bundesländer.RDS"), compress = "xz")

reff.österreich <- ages.timeline.österreich |>
  estimate_r(cases = Faelle.neu, date = Datum, groupby = Bundesland, intervall = 7)
saveRDS(reff.österreich, paste0(path.data, "reff.österreich.RDS"))

reff.altersgruppe.österreich <- ages.altersgruppe |> 
  # summarise Geschlecht
  group_by(Testdatum, Bundesland, Altersgruppe) |> 
  summarise(Faelle.neu = sum(Faelle.neu))|>
  # filter Österreich
  filter(Bundesland == "Österreich") |>
  # estimate R
  estimate_r(cases = Faelle.neu, date = Testdatum, groupby = Altersgruppe, intervall = 7)
saveRDS(reff.altersgruppe.österreich, file = paste0(path.data, "reff.altersgruppe.österreich.RDS"))

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
)  |>
  saveRDS(str_glue(path.data, "timeline_covid.RDS"), compress = "xz")



# last call
rm(list = ls())
gc()
