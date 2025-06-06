% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Pairing.R
\name{commit_pairs_to_history}
\alias{commit_pairs_to_history}
\title{Commit Session Pairs To Pair History}
\usage{
commit_pairs_to_history(pair_history, session_df)
}
\arguments{
\item{pair_history}{Existing history data frame.}

\item{session_df}{New session pairings to add.}
}
\value{
An updated pairing history data frame.
}
\description{
Appends the generated session pairings to the cumulative pairing history.
}
\details{
Clean separation of pairs generation and history update (for when we don't like a pairing ect.)
}
\examples{
pair_history <- commit_pairs_to_history(pair_history, session_df)
}
