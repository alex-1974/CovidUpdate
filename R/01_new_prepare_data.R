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
  ) |>
  saveRDS(str_glue(path.data, "ages.timeline.RDS"), compress = "xz")
rm("ages.timeline")

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
  ) |>
  saveRDS(str_glue(path.data, "ages.altersgruppe.RDS"), compress = "xz")
rm("ages.altersgruppe")

# AGES fallzahlen
ages.fallzahlen <- ages.fallzahlen |>
  mutate(
    Meldedatum = as_date(dmy(Meldedatum)),
    Bundesland = factor(Bundesland, levels = bundesländer.fct, ordered = TRUE)
  ) |>
  group_by(Bundesland) |>
  mutate(
    Normalstation.Gesamt = Normalstation.Faelle + Normalstation.Frei,
    ICU.Gesamt = ICU.Faelle + ICU.Frei
  ) |>
  saveRDS(str_glue(path.data, "ages.fallzahlen.RDS"), compress = "xz")
rm("ages.fallzahlen")

# AGES hospitalisierung
ages.hospitalisierung <- ages.hospitalisierung |>
  mutate(
    Meldedatum = as_date(dmy_hms(Meldedatum)),
    Bundesland = factor(Bundesland, levels = bundesländer.fct, ordered = TRUE)
  ) |>
  saveRDS(str_glue(path.data, "ages.hospitalisierung.RDS"), compress = "xz")
rm("ages.hospitalisierung")

# BMSGPK timeline
bmsgpk.timeline <- bmsgpk.timeline |>
  mutate(
    Bundesland = factor(Bundesland, levels = bundesländer.fct, ordered = TRUE)
  ) |>
  saveRDS(str_glue(path.data, "bmsgpk.timeline.RDS"), compress = "xz")
rm("bmsgpk.timeline")

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
  ) |>
  saveRDS(str_glue(path.data, "bmsgpk.vaccination.RDS"), compress = "xz")
rm("bmsgpk.vaccination")

ages.reff.österreich <- ages.reff.österreich |>
  mutate(
    Datum = as_date(ymd(Datum)),
    R_eff = as.numeric(gsub(",", ".", R_eff)),
    R_eff_lwr = as.numeric(gsub(",", ".", R_eff_lwr)),
    R_eff_upr = as.numeric(gsub(",", ".", R_eff_upr))
  ) |>
  saveRDS(str_glue(path.data, "ages.reff.österreich.RDS"), compress = "xz")
rm("ages.reff.österreich")

ages.reff.bundesländer <- ages.reff.bundesländer |>
  mutate(
    Datum = as_date(ymd(Datum)),
    Bundesland = factor(Bundesland, levels = bundesländer.fct, ordered = TRUE),
    R_eff = as.numeric(gsub(",", ".", R_eff)),
    R_eff_lwr = as.numeric(gsub(",", ".", R_eff_lwr)),
    R_eff_upr = as.numeric(gsub(",", ".", R_eff_upr))
  ) |>
  saveRDS(str_glue(path.data, "ages.reff.bundesländer.RDS"), compress = "xz")
rm("ages.reff.bundesländer")

# last call
gc()
