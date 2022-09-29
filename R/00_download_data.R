#######################
# Download the data
#
# Daten von: https://github.com/martin-illarion/covid-age-bl/blob/master/Readme.md
#######################

library(tidyverse)
#library(lubridate)
library(assertr)

source("R/config.R")

temp <- tempfile()

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
tryCatch({
ages.altersgruppe <- read_delim(paste0(tempdir(), "/CovidFaelle_Altersgruppe.csv"), delim=";") |>
  select(
    Testdatum = Time, Altersgruppe, Bundesland, Einwohner = AnzEinwohner, Geschlecht, Faelle.cumsum = Anzahl, Genesen.cumsum = AnzahlGeheilt, Gestorben.cumsum = AnzahlTot
  ) %>%
  chain_start %>%
  verify(nrow(.) > 0) %>%
  verify(as_date(dmy_hms(Testdatum)) > as_date("2020-01-01") & as_date(dmy_hms(Testdatum)) < now()) %>%
  assert(in_set(c("Österreich", bundesländer.list)), Bundesland) %>% 
  assert(in_set(altersgruppe.list), Altersgruppe) %>%
  assert(in_set(c("M","W")), Geschlecht) %>%
  verify(Einwohner >= 0) %>%
  verify(Faelle.cumsum >= 0) %>%
  verify(Genesen.cumsum >= 0) %>%
  verify(Gestorben.cumsum >= 0) %>%
  #verify(all(ages.altersgruppe |> count(Altersgruppe,Bundesland, Geschlecht,Testdatum) |> select(n) == 1)) %>%
  chain_end

print(tail(ages.altersgruppe))
message("Successfully extracted AGES Altersgruppe data")
},
error = function(e) { 
  message("Error extracting AGES Altersgruppe data!")
  print(e)
},
warning = function(w) {
  message("Warning extracting AGES Altersgruppe data!")
  print(w)
})

## Extract CovidFaelle_Timeline.csv
tryCatch({
ages.timeline <- read_delim(paste0(tempdir(), "/CovidFaelle_Timeline.csv"), delim=";") |>
  select(
    Testdatum = Time, Bundesland, Einwohner = AnzEinwohner, Faelle.neu = AnzahlFaelle, Genesen.neu = AnzahlGeheiltTaeglich, Gestorben.neu = AnzahlTotTaeglich
  )  %>%
  chain_start %>%
  verify(nrow(.) > 0) %>%
  #verify(Testdatum > as_date("2020-01-01") & Testdatum < now()) %>%
  assert(in_set(c("Österreich", bundesländer.list)), Bundesland) %>%
  verify(Einwohner >= 0) %>%
  verify(Faelle.neu >= 0) %>%
  verify(Genesen.neu >= 0) %>%
  verify(Gestorben.neu >= 0) %>%
  #verify(all(ages.timeline |> count(Bundesland, Testdatum) |> select(n) == 1)) %>%
  chain_end

print(tail(ages.timeline))
message("Successfully extracted AGES timeline data")
},
error = function(e) { 
  message("Error extracting AGES timeline data!")
  print(e)
},
warning = function(w) {
  message("Warning extracting AGES timeline data!")
  print(w)
})

## Extract CovidFallzahlen.csv
tryCatch({
ages.fallzahlen <- read_delim(paste0(tempdir(), "/CovidFallzahlen.csv"), delim=";") |>
  select(
    Meldedatum = Meldedat, Bundesland, Tests.gesamt = TestGesamt, 
    Normalstation.Faelle = FZHosp,  Normalstation.Frei = FZHospFree, ICU.Faelle = FZICU, ICU.Frei = FZICUFree
  )
print(tail(ages.fallzahlen))
message("Successfully extracted AGES Fallzahlen data")
},
error = function(e) { 
  message("Error extracting AGES Fallzahlen data!")
  print(e)
},
warning = function(w) {
  message("Warning extracting AGES Fallzahlen data!")
  print(w)
})

## Extract Hospitalisierung.csv
tryCatch({
ages.hospitalisierung <- read_delim(paste0(tempdir(), "/Hospitalisierung.csv"), delim=";") |>
  select(
    Meldedatum, Bundesland, Tests.gesamt = TestGesamt, 
    Normalbetten.belegt = NormalBettenBelCovid19, 
    ICU.belegt.Cov19 = IntensivBettenBelCovid19, ICU.belegt.nichtCov19 = IntensivBettenBelNichtCovid19, ICU.frei = IntensivBettenFrei, ICU.kapazität = IntensivBettenKapGes
  ) 
print(tail(ages.hospitalisierung))
message("Successfully extracted AGES Hospitalisierung data")
},
error = function(e) { 
  message("Error extracting AGES Hospitalisierung data!")
  print(e)
},
warning = function(w) {
  message("Warning extracting AGES Hospitalisierung data!")
  print(w)
})


#######################
# Gesundheitsministerium
#######################


tryCatch({
download.file("https://info.gesundheitsministerium.gv.at/data/timeline-faelle-bundeslaender.csv", destfile=temp, method="wget", quiet = TRUE)
bmsgpk.timeline <-read_delim(temp, delim=";") |>
  select(
    Meldedatum = Datum, Bundesland = Name, Tests.gesamt.cumsum = Testungen, Tests.AG.cumsum = TestungenAntigen, Tests.PCR.cumsum = TestungenPCR
  ) |>
  group_by(Bundesland) |>
  mutate(
    Meldedatum = as_date(ymd_hms(Meldedatum)),
    #Faelle.neu = Faelle.cumsum - lag(Faelle.cumsum, order_by = Meldedatum),
    Tests.gesamt.neu = Tests.gesamt.cumsum - lag(Tests.gesamt.cumsum, order_by = Meldedatum),
    Tests.AG.neu = Tests.AG.cumsum - lag(Tests.AG.cumsum, order_by = Meldedatum),
    Tests.PCR.neu = Tests.PCR.cumsum - lag(Tests.PCR.cumsum, order_by = Meldedatum)
  ) %>%
  chain_start %>% 
  verify(nrow(.) > 0) %>%
  verify(Meldedatum >= as_date("2020-01-01") & Meldedatum <= now()) %>%
  assert(in_set(c("Österreich", bundesländer.list)), Bundesland) %>%
  #verify(all(bmsgpk.timeline |> count(Bundesland, Meldedatum) |> select(n) == 1)) %>%
  chain_end

print(tail(bmsgpk.timeline))
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
bmsgpk.vaccination <- read_delim(temp, delim=";") |>
  select(
    Meldedatum = date, Bundesland = state_name, Impfstoff = vaccine, Dosis = dose_number, Anzahl.cumsum = doses_administered_cumulative
  ) 
print(tail(bmsgpk.vaccination))
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
ages.reff.österreich <- read_delim(temp, delim=";") 
 
print(tail(ages.reff.österreich))
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
ages.reff.bundesländer <- read_delim(temp, delim=";", locale = locale(encoding = "latin1")) 
  
print(tail(ages.reff.bundesländer))
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

