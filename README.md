# Detect stationarity in animal movement data

**This is a test repository.**
**Our final contribution to the EMAC23 Coding Challenge can be found in [this](https://github.com/alexvmt/animal-movement-summary-and-stationarity-analysis) repository.**

EMAC23 Coding Challenge: https://www.moveapps.org/#news

Use Cases: https://www.moveapps.org/assets/EMAC23_CodingChallenge_Details2.eeb3420c.pdf -> Challenge 1: Stationarity

Assume that stationarity is definite, e. g.:
- animal is deceased
- animal has dropped tag

Data used:
- Kruger African Buffalo, GPS tracking, South Africa (Movebank ID 1764627)
- Straw-colored fruit bats (Eidolon helvum) in Africa 2009-2014 (Movebank ID 404939825)
- Migration timing in white-fronted geese (data from Klzsch et al. 2016) (Movebank ID 133992043)

The used datasets are transformed to RDS-format using the MoveApps platform.

Move the downloaded transformed data into a directory called `data` outside of this repository.

The script `stationarity_analysis.R`
- loads a dataset,
- retrieves some general characteristics of the loaded dataset,
- processes the given data for each individual for a given time period,
- computes aggregated distances for each individual,
- plots the aggregated distances over time for a selected individual
- plots the last coordinates on a map for a selected individual
- and finally creates a movement summary table for all individuals in the given dataset.

Below are a time series and a map example for an individual from the buffalo dataset:

![time_series example](time_series_example.png 'time series example')

![map example](map_example.png 'map example')
