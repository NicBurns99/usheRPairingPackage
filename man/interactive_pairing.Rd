% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Pairing.R
\name{interactive_pairing}
\alias{interactive_pairing}
\title{Interactive Session Pairing Tool}
\usage{
interactive_pairing(
  present_roster,
  pair_history,
  Class,
  Semester,
  Week,
  odd_student_strategy
)
}
\arguments{
\item{present_roster}{Data frame of present students.}

\item{pair_history}{Pairing history data frame.}

\item{Class}{Class identifier.}

\item{Semester}{Semester number.}

\item{Week}{Week number.}

\item{odd_student_strategy}{Strategy to handle odd student out: group of 3 or manual.}
}
\value{
list with two elements: student pairs (the confirmed session pairs), and history (the updated pairing history).
}
\description{
Generates session pairs, shows them to user, and asks for confirmation. Repeats until accepted, then commits to pairing history once accepted.
}
\examples{
interactive_pairing(present_students, pair_history, "DATASCIENCE101", 1, 1, "group_of_3")
}
