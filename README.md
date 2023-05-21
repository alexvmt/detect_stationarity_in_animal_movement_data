# Detect stationarity in animal movement data

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

Below are a timeseries and a map example for an individual from the buffalo dataset:

![timeseries example](timeseries_example.png 'timeseries example')

![map example](map_example.png 'map example')
