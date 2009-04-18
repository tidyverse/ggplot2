\name{IMDB movies data}
\docType{data}
\alias{movies}
\title{Movie information and user ratings from IMDB.com}
\description{
The internet movie database, \url{http://imdb.com/}, is a website devoted to collecting movie data supplied by studios and fans.  It claims to be the biggest movie database on the web and is run by amazon.  More about information imdb.com can be found online,  \url{http://imdb.com/help/show_leaf?about}, including information about the data collection process, \url{http://imdb.com/help/show_leaf?infosource}.

IMDB makes their raw data available at \url{http://uk.imdb.com/interfaces/}. Unfortunately, the data is divided into many text files and the format of each file differs slightly.  To create one data file containing all the desired information I wrote a script in the ruby to extract the relevent information and store in a database.  This data was then exported into csv for easy import into many programs.

The following text files were downloaded and used:

\itemize{
	\item business.list. Total budget
	\item genres.list.  Genres that a movie belongs to (eg. comedy and action)
	\item movies.list.  Master list of all movie titles with year of production.
	\item mpaa-ratings-reasons.list.  MPAA ratings.
	\item ratings.list.  IMDB fan ratings.
	\item running-times.list.  Movie length in minutes.
}

Movies were selected for inclusion if they had a known length and had been rated by at least one imdb user.  The csv file contains the following fields:

\itemize{
	\item title.  Title of the movie.
	\item year.  Year of release.
	\item budget.  Total budget (if known) in US dollars
	\item length.  Length in minutes.
	\item rating.  Average IMDB user rating.
	\item votes.  Number of IMDB users who rated this movie.
	\item r1-10.  Multiplying by ten gives percentile (to nearest 10\%) of users who rated this movie a 1.
	\item mpaa.  MPAA rating.
	\item action, animation, comedy, drama, documentary, romance, short.  Binary variables representing if movie was classified as belonging to that genre.
}

}
\usage{data(movies)}
\format{A data frame with 28819 rows and 24 variables}
\references{\url{http://had.co.nz/data/movies/}}
\keyword{datasets}
