# bekiths

R-Paket zur Aggregation und Aufbereitung von Schulstatistiken aus https://www.schulstatistik-thueringen.de.


## Installation

```r
# install.packages("remotes")
remotes::install_github("bekigeki/bekiths")
```

## Beispiel

```r
library(bekiths)

# Datensatz Beschreibung ansehen
help(bekiths_data)

# Vignette zur Erstellung des Datensatzes
vignette("getting-started", package = "bekiths")

```
## Datenquelle

Die Daten stammen aus:

"Schulen und Schüler nach Schulart sowie Schulstufen (Primar-, Sekundarstufe I und II) (1A5)",
https://www.schulstatistik-thueringen.de.

Downloaddatum und Stichtagsdaum sind im Dataframe als Spalte enthalten.
