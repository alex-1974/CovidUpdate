library(tidyverse)
library(lubridate)
library(ggpubr)

source("R/libmath.R")
source("R/config.R")

period.from <- rollback(add_with_rollback(now(),months(-1)), roll_to_first = TRUE)
now2 <- add_with_rollback(now(),days(+1))
#img.path <- "img"
#.rds.path <- "rds/"

#source("R/01_load_data.R")
ages.timeline <- readRDS(paste0(path.data, "ages.timeline.RDS"))
ages.altersgruppe <- readRDS(paste0(path.data, "ages.altersgruppe.RDS"))
ages.reff.österreich <- readRDS(paste0(path.data, "ages.reff.österreich.RDS"))
ages.reff.bundesländer <- readRDS(paste0(path.data, "ages.reff.bundesländer.RDS"))
#ems.estimate_r.österreich.gesamt <-readRDS(paste0(.rds.path, "ems_reff_österreich_gesamt.rds"))
#ages.estimate_r.österreich.altersgruppe.gesamt <- readRDS(paste0(.rds.path, "ages_reff_österreich_altersgruppe_gesamt.rds"))
reff.österreich <- readRDS(str_glue(path.data, "reff.österreich.RDS"))
reff.altersgruppe.österreich <- readRDS(str_glue(path.data, "reff.altersgruppe.österreich.RDS"))

hospitalization <- readRDS(str_glue(path.data, "hospitalization.combined.RDS"))
testing <- readRDS(str_glue(path.data, "testing.combined.RDS"))
vaccination <- readRDS(str_glue(path.data, "vaccination.combined.RDS"))
timeline.covid <- readRDS(paste0(path.data, "timeline_covid.RDS"))

####################
# Inzidenzen
####################

#ems.ages.bmsgpk.timeline.österreich <- ems.ages.bmsgpk.timeline |> filter(Bundesland == "Österreich")

ages.inz7d.österreich.gesamt <- ages.timeline |>
  mutate(
    Faelle = incidence.7days(cases = Faelle.neu, pop = Einwohner)
  ) |>
  select(Testdatum, Faelle)
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


