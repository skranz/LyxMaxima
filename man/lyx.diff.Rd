\name{lyx.diff}
\alias{lyx.diff}
\title{Differentiates a lyx expression}
\usage{
  lyx.diff(txt = lyma$txt, ma.var = lyma$ma.var,
    lyma = new.lyma(txt))
}
\description{
  Differentiates a lyx expression
}

\examples{\dontrun{
  txt = "x^2"
  lyx.diff("x^2","x")
}}
