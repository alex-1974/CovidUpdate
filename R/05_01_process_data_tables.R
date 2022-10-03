library(tidyverse)
library(lubridate)
library(kableExtra)

source("R/config.R")

table.ems.ages.bmsgpk.timeline <- ages.timeline |>
  filter(Testdatum == max(Testdatum)) |>
  select(Bundesland, ages = Faelle.inz7d) |>
  knitr::kable(
    caption = "7-Tages-Inzidenzen der Bundesländer",
    booktabs = TRUE,
    digits=0) |>
  kable_styling(full_width = F) |>
  add_header_above(c(" ", "Datenquelle" = 1))

table.ages.estimate_r.bundesländer <- reff.österreich |>
  filter(
    Testdatum == max(Testdatum) | Testdatum == max(Testdatum) - 1 | Testdatum == max(Testdatum) - 7
  ) |>
  mutate(
    Testdatum = case_when(
      Testdatum == max(Testdatum)    ~ " ",
      Testdatum == max(Testdatum) -1 ~ "Vortag",
      Testdatum == max(Testdatum) -7 ~ "Vorwoche"
    ),
    r_string = str_glue("{r_t_most_likely} ({r_t_lo};{r_t_hi})")
  ) |>
  select(Bundesland, Testdatum, r_string) |>
  spread(
    key = Testdatum,
    value = r_string
  )  |>
  knitr::kable(
    caption = str_glue("Reff der Bundesländer vom {max(reff.österreich$Testdatum)}"),
    booktabs = TRUE,
    digits=0,
    format = "latex") |>
  kable_styling(full_width = F) |>
  add_header_above(c(" ", " ",  "Änderung seit" = 2)) |>
  save_kable(file = "~/CovidR/Covid Update/img/table_ages_reff.pdf", size = 2)

table.ages.estimate_r.altersgruppe <- reff.altersgruppe.österreich |>
  filter(
    Testdatum == max(Testdatum) | Testdatum == max(Testdatum) - 1 | Testdatum == max(Testdatum) - 7
  ) |>
  mutate(
    Testdatum = case_when(
      Testdatum == max(Testdatum)    ~ " ",
      Testdatum == max(Testdatum) -1 ~ "Vortag",
      Testdatum == max(Testdatum) -7 ~ "Vorwoche"
    ),
    r_string = str_glue("{r_t_most_likely} ({r_t_lo};{r_t_hi})")
  ) |>
  select(Altersgruppe, Testdatum, r_string) |>
  spread(
    key = Testdatum,
    value = r_string
  ) |>
  knitr::kable(
    caption = str_glue("Reff der Altersgruppen vom {max(reff.altersgruppe.österreich$Testdatum)}"),
    booktabs = TRUE,
    digits=0) |>
  kable_styling(full_width = F) |>
  add_header_above(c(" ", " ",  "Änderung seit" = 2))
 
