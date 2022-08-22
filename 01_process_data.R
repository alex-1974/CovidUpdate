library(tidyverse)
library(lubridate)
library(kableExtra)

source("00_load_data.R")

table.ems.ages.bmsgpk.timeline <- purrr::reduce(
  list(
    ages.timeline |> select(Datum, Bundesland, Einwohner, ages.neu = Faelle.neu),
    ems.timeline |> select(Datum, Bundesland, ems.neu = Faelle.neu),
    bmsgpk.timeline |> select(Datum, Bundesland, bmsgpk.neu = Faelle.neu)
  ),
  dplyr::left_join,
  by = c("Datum", "Bundesland")
) |>
  group_by(Bundesland) |>
  mutate(
    ages.inc = notification_rate_7d(ages.neu, Einwohner),
    ems.inc = notification_rate_7d(ems.neu, Einwohner),
    bmsgpk.inc = notification_rate_7d(bmsgpk.neu, Einwohner)
  ) |>
  filter(Datum == max(Datum)) |>
  select(Bundesland, ages = ages.inc, ems = ems.inc, bmsgpk = bmsgpk.inc) |>
  knitr::kable(
    caption = "7-Tages-Inzidenzen der Bundesländer",
    booktabs = TRUE,
    digits=0) |>
  kable_styling(full_width = F) |>
  add_header_above(c(" ", "Datenquelle" = 3))

graph.epidemiologic_curve <- ages.timeline |>
  filter(
    Bundesland == "Österreich",
    Datum > max(Datum) -120
    ) |>
  select(Datum, Faelle.neu) |>
  ggplot(aes(y = Faelle.neu, x = Datum)) +
  geom_bar(stat="identity") +
  labs(
    title="Epidemische Kurve Österreich",
    subtitle ="Absolute Anzahl Neuerkrankter im Zeitverlauf",
    caption = "Quelle: Daten des AGES Dashboards",
    x="", y = "Neuerkrankte") +
  theme_minimal()

graph.inz7d.österreich <- ages.timeline |>
  filter(
    Bundesland == "Österreich"
  ) |>
  mutate(
    Faelle = notification_rate_7d(cases = Faelle.neu, pop = Einwohner)
  ) |>
  select(Datum, Faelle) |>
  ggplot(aes(y = Faelle, x = Datum)) +
  geom_line() +
  labs(
    title= "Verlauf der Pandemie in Österreich",
    subtitle = "7-Tages-Inzidenz pro 100.000 Einwohner",
    caption = "Quelle: Daten des AGES Dashboards",
    x="", y = "Inzidenz") +
  theme_minimal()

graph.inz7d.bundesländer <- ages.timeline |>
  filter(Datum > max(Datum) -180) |>
  group_by(Bundesland) |> 
  mutate(
    Faelle = notification_rate_7d(cases = Faelle.neu, pop = Einwohner, fill = NA)
  ) |>
  select(Datum,Bundesland,Faelle) |>
  ggplot(aes(y = Faelle, x = Datum, group = Bundesland, colour = Bundesland)) +
  geom_line() +
  labs(
    title= "Inzidenz der Bundesländer",
    subtitle = "7-Tages-Inzidenz pro 100.000 Einwohner",
    caption = "Quelle: Daten des AGES Dashboards",
    x="", y = "Inzidenz") +
  theme_minimal()

graph.hospitalisierung <- ages.hospitalisierung |>
  filter(
    Bundesland == "Österreich",
    Meldedatum > max(Meldedatum) - 180
    ) |>
  select(Meldedatum,Normalbetten.belegt, ICU.belegt.Cov19) |>
  gather(key="Station", value="Belegt", 2:3) |>
  ggplot(aes(x = Meldedatum, y = Belegt, group = Station, colour = Station)) +
  geom_line() +
  theme_minimal()

ages.sex.bundesländer <- ages.altersgruppe |>
  filter(
    Datum == max(Datum)
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

# alle gemeldeten Faelle
sum(ages.sex.bundesländer[ages.sex.bundesländer$Bundesland == "Österreich", ]$Faelle.cumsum)
sum(ages.timeline[ages.timeline$Bundesland != "Österreich", ]$Faelle.neu)

ages.sex.bundesländer[ages.sex.bundesländer$Bundesland == "Österreich" & ages.sex.bundesländer$Geschlecht == "m", ]$ratio
