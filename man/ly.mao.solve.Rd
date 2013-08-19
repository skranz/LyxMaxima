\name{ly.mao.solve}
\alias{ly.mao.solve}
\title{Lyx code to maxima solve code}
\usage{
  ly.mao.solve(str, ma.var = lyma$ma.var,
    lyma = new.lyma())
}
\arguments{
  \item{str}{a Latex math expression}

  \item{var}{a Maxima variable name}
}
\description{
  Lyx code to maxima solve code
}

\examples{\dontrun{
  ly.mao.solve("x_{1}+y=5","x_1")
}}
