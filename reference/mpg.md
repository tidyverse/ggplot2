# Fuel economy data from 1999 to 2008 for 38 popular models of cars

This dataset contains a subset of the fuel economy data that the EPA
makes available on <https://fueleconomy.gov/>. It contains only models
which had a new release every year between 1999 and 2008 - this was used
as a proxy for the popularity of the car.

## Usage

``` r
mpg
```

## Format

A data frame with 234 rows and 11 variables:

- manufacturer:

  manufacturer name

- model:

  model name

- displ:

  engine displacement, in litres

- year:

  year of manufacture

- cyl:

  number of cylinders

- trans:

  type of transmission

- drv:

  the type of drive train, where f = front-wheel drive, r = rear wheel
  drive, 4 = 4wd

- cty:

  city miles per gallon

- hwy:

  highway miles per gallon

- fl:

  fuel type

- class:

  "type" of car
