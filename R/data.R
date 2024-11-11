#' Prices of over 50,000 round cut diamonds
#'
#' A dataset containing the prices and other attributes of almost 54,000
#'  diamonds. The variables are as follows:
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{price}{price in US dollars ($326--$18,823)}
#'   \item{carat}{weight of the diamond (0.2--5.01)}
#'   \item{cut}{quality of the cut (Fair, Good, Very Good, Premium, Ideal)}
#'   \item{color}{diamond colour, from D (best) to J (worst)}
#'   \item{clarity}{a measurement of how clear the diamond is (I1 (worst), SI2,
#'     SI1, VS2, VS1, VVS2, VVS1, IF (best))}
#'   \item{x}{length in mm (0--10.74)}
#'   \item{y}{width in mm (0--58.9)}
#'   \item{z}{depth in mm (0--31.8)}
#'   \item{depth}{total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43--79)}
#'   \item{table}{width of top of diamond relative to widest point (43--95)}
#' }
"diamonds"


#' US economic time series
#'
#' This dataset was produced from US economic time series data available from
#' \url{https://fred.stlouisfed.org/}. `economics` is in "wide"
#' format, `economics_long` is in "long" format.
#'
#' @format A data frame with 574 rows and 6 variables:
#' \describe{
#'   \item{date}{Month of data collection}
#'   \item{pce}{personal consumption expenditures, in billions of dollars,
#'     \url{https://fred.stlouisfed.org/series/PCE}}
#'   \item{pop}{total population, in thousands,
#'     \url{https://fred.stlouisfed.org/series/POP}}
#'   \item{psavert}{personal savings rate,
#'     \url{https://fred.stlouisfed.org/series/PSAVERT/}}
#'   \item{uempmed}{median duration of unemployment, in weeks,
#'     \url{https://fred.stlouisfed.org/series/UEMPMED}}
#'   \item{unemploy}{number of unemployed in thousands,
#'     \url{https://fred.stlouisfed.org/series/UNEMPLOY}}
#' }
#'
"economics"

#' @rdname economics
"economics_long"

#' Midwest demographics
#'
#' Demographic information of midwest counties from 2000 US census
#'
#' Note: this dataset is included for illustrative purposes. The original
#' descriptions were not documented and the current descriptions here are based
#' on speculation. For more accurate and up-to-date US census data, see the
#' [`acs` package](https://cran.r-project.org/package=acs).
#'
#' @format A data frame with 437 rows and 28 variables:
#' \describe{
#'  \item{PID}{Unique county identifier.}
#'  \item{county}{County name.}
#'  \item{state}{State to which county belongs to.}
#'  \item{area}{Area of county (units unknown).}
#'  \item{poptotal}{Total population.}
#'  \item{popdensity}{Population density (person/unit area).}
#'  \item{popwhite}{Number of whites.}
#'  \item{popblack}{Number of blacks.}
#'  \item{popamerindian}{Number of American Indians.}
#'  \item{popasian}{Number of Asians.}
#'  \item{popother}{Number of other races.}
#'  \item{percwhite}{Percent white.}
#'  \item{percblack}{Percent black.}
#'  \item{percamerindan}{Percent American Indian.}
#'  \item{percasian}{Percent Asian.}
#'  \item{percother}{Percent other races.}
#'  \item{popadults}{Number of adults.}
#'  \item{perchsd}{Percent with high school diploma.}
#'  \item{percollege}{Percent college educated.}
#'  \item{percprof}{Percent with professional degree.}
#'  \item{poppovertyknown}{Population with known poverty status.}
#'  \item{percpovertyknown}{Percent of population with known poverty status.}
#'  \item{percbelowpoverty}{Percent of people below poverty line.}
#'  \item{percchildbelowpovert}{Percent of children below poverty line.}
#'  \item{percadultpoverty}{Percent of adults below poverty line.}
#'  \item{percelderlypoverty}{Percent of elderly below poverty line.}
#'  \item{inmetro}{County considered in a metro area.}
#'  \item{category}{Miscellaneous.}
#' }
#'
"midwest"


#' Fuel economy data from 1999 to 2008 for 38 popular models of cars
#'
#' This dataset contains a subset of the fuel economy data that the EPA makes
#' available on \url{https://fueleconomy.gov/}. It contains only models which
#' had a new release every year between 1999 and 2008 - this was used as a
#' proxy for the popularity of the car.
#'
#' @format A data frame with 234 rows and 11 variables:
#' \describe{
#'   \item{manufacturer}{manufacturer name}
#'   \item{model}{model name}
#'   \item{displ}{engine displacement, in litres}
#'   \item{year}{year of manufacture}
#'   \item{cyl}{number of cylinders}
#'   \item{trans}{type of transmission}
#'   \item{drv}{the type of drive train, where f = front-wheel drive, r = rear wheel drive, 4 = 4wd}
#'   \item{cty}{city miles per gallon}
#'   \item{hwy}{highway miles per gallon}
#'   \item{fl}{fuel type}
#'   \item{class}{"type" of car}
#' }
"mpg"

#' An updated and expanded version of the mammals sleep dataset
#'
#' This is an updated and expanded version of the mammals sleep dataset.
#' Updated sleep times and weights were taken from V. M. Savage and G. B.
#' West. A quantitative, theoretical framework for understanding mammalian
#' sleep. Proceedings of the National Academy of Sciences, 104 (3):1051-1056,
#' 2007.
#'
#' Additional variables order, conservation status and vore were added from
#' wikipedia.
#'
#' @format A data frame with 83 rows and 11 variables:
#' \describe{
#'   \item{name}{common name}
#'   \item{genus}{}
#'   \item{vore}{carnivore, omnivore or herbivore?}
#'   \item{order}{}
#'   \item{conservation}{the conservation status of the animal}
#'   \item{sleep_total}{total amount of sleep, in hours}
#'   \item{sleep_rem}{rem sleep, in hours}
#'   \item{sleep_cycle}{length of sleep cycle, in hours}
#'   \item{awake}{amount of time spent awake, in hours}
#'   \item{brainwt}{brain weight in kilograms}
#'   \item{bodywt}{body weight in kilograms}
#' }
"msleep"

#' Terms of 12 presidents from Eisenhower to Trump
#'
#' The names of each president, the start and end date of their term, and
#' their party of 12 US presidents from Eisenhower to Trump.  This data is
#' in the public domain.
#'
#' @format A data frame with 12 rows and 4 variables:
#' \describe{
#'   \item{name}{Last name of president}
#'   \item{start}{Presidency start date}
#'   \item{end}{Presidency end date}
#'   \item{party}{Party of president}
#' }
"presidential"

#' Vector field of seal movements
#'
#' This vector field was produced from the data described in Brillinger, D.R.,
#' Preisler, H.K., Ager, A.A. and Kie, J.G. "An exploratory data analysis
#' (EDA) of the paths of moving animals". J. Statistical Planning and
#' Inference 122 (2004), 43-63, using the methods of Brillinger, D.R.,
#' "Learning a potential function from a trajectory", Signal Processing
#' Letters. December (2007).
#'
#' @format A data frame with 1155 rows and 4 variables
#' @references \url{https://www.stat.berkeley.edu/~brill/Papers/jspifinal.pdf}
"seals"

#' 2d density estimate of Old Faithful data
#'
#' A 2d density estimate of the waiting and eruptions variables data
#' \link{faithful}.
#'
#' @format A data frame with 5,625 observations and 3 variables:
#' \describe{
#'   \item{eruptions}{Eruption time in mins}
#'   \item{waiting}{Waiting time to next eruption in mins}
#'   \item{density}{2d density estimate}
#' }
"faithfuld"

#' `colors()` in Luv space
#'
#' All built-in [colors()] translated into Luv colour space.
#'
#' @format A data frame with 657 observations and 4 variables:
#' \describe{
#' \item{L,u,v}{Position in Luv colour space}
#' \item{col}{Colour name}
#' }
"luv_colours"

#' Housing sales in TX
#'
#' Information about the housing market in Texas provided by the TAMU
#' real estate center, \url{https://trerc.tamu.edu/}.
#'
#' @format A data frame with 8602 observations and 9 variables:
#' \describe{
#' \item{city}{Name of multiple listing service (MLS) area}
#' \item{year,month,date}{Date}
#' \item{sales}{Number of sales}
#' \item{volume}{Total value of sales}
#' \item{median}{Median sale price}
#' \item{listings}{Total active listings}
#' \item{inventory}{"Months inventory": amount of time it would take to sell
#'   all current listings at current pace of sales.}
#' }
"txhousing"
