\name{ly.mao.diff}
\alias{ly.mao.diff}
\title{diff: lyx to mao code}
\usage{
  ly.mao.diff(str, ma.var = lyma$ma.var, lyma = new.lyma())
}
\arguments{
  \item{str}{a Latex math expression}

  \item{ma.var}{a Maxima variable name}
}
\description{
  diff: lyx to mao code
}

\examples{\dontrun{
  ly.mao.diff("x_{1}^2+5","x_1")
}}
