library(tidyverse)
library(lubridate)
library(purrr)
library(zoo)

source("~/CovidR/Covid Data/download_data.R")

.path <- "~/CovidR/Covid Data/raw data/"

.notification_rate <- function(day, cases, pop, per, fill) {
  incidence = (zoo::rollsum(cases, day, align = "right", fill = fill) / pop) * per
}
notification_rate_7d <- function(cases, pop, fill = 0) {
  return(.notification_rate(day = 7, cases = cases, pop = pop, fill = fill, per = 100000))
}

ages.timeline <- read_csv(paste0(.path, "ages.timeline.csv"))
ages.altersgruppe <- read_csv(paste0(.path, "ages.altersgruppe.csv"))
ems.timeline <- read_csv(paste0(.path, "ems.timeline.csv"))
bmsgpk.timeline <- read_csv(paste0(.path, "BMSGPK.timeline.csv"))
ages.hospitalisierung <- read_csv(paste0(.path, "ages.hospitalisierung.csv"))

.ages.timeline.at <- ages.timeline |>
  filter(
    Bundesland == "Österreich"
  ) |>
  select(Datum, Einwohner, ages.neu = Faelle.neu)

.ems.timeline.at <- ems.timeline |>
  filter(
    Bundesland == "Österreich"
  ) |>
  select(Datum, ems.neu = Faelle.neu)

.bmsgpk.timeline.at <- bmsgpk.timeline |>
  filter(
    Bundesland == "Österreich"
  ) |>
  select(Datum, bmsgpk.neu = Faelle.neu)

ems.ages.bmsgpk.timeline <- purrr::reduce(
  list(.ages.timeline.at, .ems.timeline.at, .bmsgpk.timeline.at),
  dplyr::left_join,
  by = "Datum"
  ) |>
  mutate(
    ages.inc = notification_rate_7d(ages.neu, Einwohner),
    ems.inc = notification_rate_7d(ems.neu, Einwohner),
    bmsgpk.inc = notification_rate_7d(bmsgpk.neu, Einwohner)
  )


