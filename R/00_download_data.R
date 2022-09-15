#######################
# Download the data
#
# Daten von: https://github.com/martin-illarion/covid-age-bl/blob/master/Readme.md
#######################

library(tidyverse)
library(lubridate)
library(assertr)

temp <- tempfile()
.path <-"data/"
bundesländer.list <- c("Wien", "Niederösterreich", "Oberösterreich", "Burgenland", "Steiermark", "Tirol", "Kärnten", "Vorarlberg", "Salzburg")
date_range <- seq(as_date("2020-01-01"), as_date(now()), by = 1)

##################
# AGES
##################

# Download AGES Dashboard Daten als zip
tryCatch({
download.file("https://covid19-dashboard.ages.at/data/data.zip", destfile=temp, method="wget", quiet = TRUE)
message(paste0("Zip Archiv: ", unzip(temp, list=TRUE)[1]))
unzip(temp, list=FALSE, exdir=paste0(tempdir()))
message("Successfully downloaded AGES data")
},
error = function(e) { 
  message("Error downloading AGES data!")
  print(e)
},
warning = function(w) {
  message("Warning downloading AGES data!")
  print(w)
})

## Extract CovidFaelle_Altersgruppe.csv
read_delim(paste0(tempdir(), "/CovidFaelle_Altersgruppe.csv"), delim=";") |>
  select(
    Testdatum = Time, Altersgruppe, Bundesland, Einwohner = AnzEinwohner, Geschlecht, Faelle.cumsum = Anzahl, Genesen.cumsum = AnzahlGeheilt, Gestorben.cumsum = AnzahlTot
  ) |>
  group_by(Altersgruppe, Bundesland, Geschlecht) |>
  mutate(
    Testdatum = as_date(dmy_hms(Testdatum)),
    Geschlecht = str_to_lower(Geschlecht),
    Faelle.neu = Faelle.cumsum - lag(Faelle.cumsum, order_by = Testdatum),
    Genesen.neu = Genesen.cumsum - lag(Genesen.cumsum, order_by = Testdatum),
    Gestoben.neu = Gestorben.cumsum - lag(Gestorben.cumsum, order_by = Testdatum)
  ) |>
  write_csv(paste0(.path, "ages.altersgruppe.csv"))

ages.timeline.rules <- . %>%
  chain_start %>% 
  verify(nrow(.) > 0) %>%
  verify(Testdatum > as_date("2020-01-01") & Testdatum < now()) %>%
  assert(in_set(c("Österreich", bundesländer.list)), Bundesland) %>%
  verify(Einwohner >= 0) %>%
  verify(Faelle.neu >= 0) %>%
  verify(Genesen.neu >= 0) %>%
  verify(Gestorben.neu >= 0) %>%
  verify(all(ages.timeline |> count(Bundesland, Testdatum) |> select(n) == 1)) %>%
  chain_end

## Extract CovidFaelle_Timeline.csv
read_delim(paste0(tempdir(), "/CovidFaelle_Timeline.csv"), delim=";") |>
  select(
    Testdatum = Time, Bundesland, Einwohner = AnzEinwohner, Faelle.neu = AnzahlFaelle, Genesen.neu = AnzahlGeheiltTaeglich, Gestorben.neu = AnzahlTotTaeglich
  ) |>
  mutate(
    Testdatum = as_date(dmy_hms(Testdatum))
  ) %>%
  #ages.timeline.rules |>
  write_csv(paste0(.path, "ages.timeline.csv"))

## Extract CovidFallzahlen.csv
read_delim(paste0(tempdir(), "/CovidFallzahlen.csv"), delim=";") |>
  select(
    Meldedatum = Meldedat, Bundesland, Tests.gesamt = TestGesamt, 
    Normalstation.Faelle = FZHosp,  Normalstation.Frei = FZHospFree, ICU.Faelle = FZICU, ICU.Frei = FZICUFree
  ) |>
  mutate(
    Meldedatum = dmy(Meldedatum),
    Normalstation.Gesamt = Normalstation.Faelle + Normalstation.Frei,
    ICU.Gesamt = ICU.Faelle + ICU.Frei
  ) |>
  write_csv(paste0(.path, "ages.fallzahlen.csv"))

## Extract Hospitalisierung.csv
read_delim(paste0(tempdir(), "/Hospitalisierung.csv"), delim=";") |>
  select(
    Meldedatum, Bundesland, Tests.gesamt = TestGesamt, 
    Normalbetten.belegt = NormalBettenBelCovid19, 
    ICU.belegt.Cov19 = IntensivBettenBelCovid19, ICU.belegt.nichtCov19 = IntensivBettenBelNichtCovid19, ICU.frei = IntensivBettenFrei, ICU.kapazität = IntensivBettenKapGes
  ) |>
  mutate(
    Meldedatum = as_date(dmy_hms(Meldedatum))
  ) |>
  write_csv(paste0(.path, "ages.hospitalisierung.csv"))


####################
# EMS
####################


ems.rules <- . %>%
  chain_start %>% 
  verify(nrow(.) > 0) %>%
  verify(Meldedatum > as_date("2020-01-01") & Meldedatum < now()) %>%
  assert(in_set(c("Österreich", bundesländer.list)), Bundesland) %>%
  verify(Faelle.cumsum > 0) %>%
  verify(all(ems.timeline |> count(Bundesland, Meldedatum) |> select(n) == 1)) %>%
  chain_end

tryCatch({
download.file("https://info.gesundheitsministerium.gv.at/data/timeline-faelle-ems.csv", destfile=temp, method="wget", quiet = TRUE)
.ems.timeline <- read_delim(temp, delim=";", progress = FALSE) |>
  select(
    Meldedatum = Datum, Bundesland = Name, Faelle.cumsum = BestaetigteFaelleEMS
  ) |>
  group_by(Bundesland) |>
  mutate(
    Meldedatum = as_date(ymd_hms(Meldedatum)),
    Faelle.neu = Faelle.cumsum - lag(Faelle.cumsum, order_by = Meldedatum)
  ) %>%
  #ems.rules |>
  write_csv(paste0(.path, "ems.timeline.csv"))

print(tail(.ems.timeline))
message("Successfully downloaded EMS data")
},
error = function(e) {
  message("Error downloading EMS data!")
  print(e)
},
warning = function(w) {
  message("Warning downloading EMS data!")
  print(w)
})

#######################
# Gesundheitsministerium
#######################

bmsgpk.rules <- . %>%
  chain_start %>% 
  verify(nrow(.) > 0) %>%
  verify(Meldedatum >= as_date("2020-01-01") & Meldedatum <= now()) %>%
  assert(in_set(c("Österreich", bundesländer.list)), Bundesland) %>%
  verify(Faelle.cumsum > 0) %>%
  verify(all(bmsgpk.timeline |> count(Bundesland, Meldedatum) |> select(n) == 1)) %>%
  chain_end

tryCatch({
download.file("https://info.gesundheitsministerium.gv.at/data/timeline-faelle-bundeslaender.csv", destfile=temp, method="wget", quiet = TRUE)
.bmsgpk.timeline <-read_delim(temp, delim=";") |>
  select(
    Meldedatum = Datum, Bundesland = Name, Tests.gesamt.cumsum = Testungen, Tests.AG.cumsum = TestungenAntigen, Tests.PCR.cumsum = TestungenPCR, Faelle.cumsum = BestaetigteFaelleBundeslaender, Genesen.cumsum = Genesen, Gestorben.cumsum = Todesfaelle, Hospitalisierung, Intensivstation
  ) |>
  group_by(Bundesland) |>
  mutate(
    Meldedatum = as_date(ymd_hms(Meldedatum)),
    Faelle.neu = Faelle.cumsum - lag(Faelle.cumsum, order_by = Meldedatum),
    Tests.gesamt.neu = Tests.gesamt.cumsum - lag(Tests.gesamt.cumsum, order_by = Meldedatum),
    Tests.AG.neu = Tests.AG.cumsum - lag(Tests.AG.cumsum, order_by = Meldedatum),
    Tests.PCR.neu = Tests.PCR.cumsum - lag(Tests.PCR.cumsum, order_by = Meldedatum)
  ) %>%
  #bmsgpk.rules |>
  write_csv(paste0(.path, "BMSGPK.timeline.csv"))

print(tail(.bmsgpk.timeline))
message("Successfully downloaded BMSGPK timeline data")
},
error = function(e) {
  message("Error downloading BMSGPK timeline data!")
  print(e)
},
warning = function(w) {
  message("Warning downloading BMSGPK timeline data!")
  print(w)
})   

tryCatch({
download.file("https://info.gesundheitsministerium.gv.at/data/COVID19_vaccination_doses_timeline.csv", destfile=temp, method="wget", quiet = TRUE)
.bmsgpk.vaccination <- read_delim(temp, delim=";") |>
  select(
    Meldedatum = date, Bundesland = state_name, Impfstoff = vaccine, Dosis = dose_number, Anzahl.kumulativ = doses_administered_cumulative
  ) |>
  mutate(
    Meldedatum = as_date(ymd_hms(Meldedatum))
  ) |>
  write_csv(paste0(.path, "BMSGPK.vaccination.csv"))
print(tail(.bmsgpk.vaccination))
message("Successfully downloaded BMSGPK vaccination data")
},
error = function(e) {
  message("Error downloading BMSGPK vaccination data!")
  print(e)
},
warning = function(w) {
  message("Warning downloading BMSGPK vaccination data!")
  print(w)
}) 

#############
# AGES Reff Daten
#############

tryCatch({
download.file("https://wissenaktuell.ages.at/fileadmin/AGES2015/Wissen-Aktuell/COVID19/R_eff.csv", destfile=temp, method="wget", quiet = TRUE)
.ages.reff.österreich <- read_delim(temp, delim=";") |>
  mutate(
    Datum = as_date(ymd(Datum)),
    R_eff = as.numeric(gsub(",", ".", R_eff)),
    R_eff_lwr = as.numeric(gsub(",", ".", R_eff_lwr)),
    R_eff_upr = as.numeric(gsub(",", ".", R_eff_upr))
  ) |>
  write_csv(paste0(.path, "ages.reff.österreich.csv"))
print(tail(.ages.reff.österreich))
message("Successfully downloaded Ages reff Österreich data")
},
error = function(e) {
  message("Error downloading Ages reff Österreich data!")
  print(e)
},
warning = function(w) {
  message("Warning downloading Ages reff Österreich data!")
  print(w)
}) 

tryCatch({
download.file("https://wissenaktuell.ages.at/fileadmin/AGES2015/Wissen-Aktuell/COVID19/R_eff_bundesland.csv", destfile=temp, method="wget", quiet = TRUE)
.ages.reff.bundesländer <- read_delim(temp, delim=";", locale = locale(encoding = "latin1")) |>
  mutate(
    Datum = as_date(ymd(Datum)),
    R_eff = as.numeric(gsub(",", ".", R_eff)),
    R_eff_lwr = as.numeric(gsub(",", ".", R_eff_lwr)),
    R_eff_upr = as.numeric(gsub(",", ".", R_eff_upr))
  ) |>
  write_csv(paste0(.path, "ages.reff.bundesländer.csv"))
print(tail(.ages.reff.bundesländer))
message("Successfully downloaded Ages reff Bundesländer data")
},
error = function(e) {
  message("Error downloading Ages reff Bundesländer data!")
  print(e)
},
warning = function(w) {
  message("Warning downloading Ages reff Bundesländer data!")
  print(w)
}) 

