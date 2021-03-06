% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pkRamp.r
\name{ramp}
\alias{ramp}
\alias{pkRamp}
\title{Load preset color ramps and palettes}
\usage{
ramp(
  name = "parula",
  number = NULL,
  reversed = F,
  random = F,
  center = 0.5,
  show = F,
  collapse = F
)

pkRamp(
  name = "parula",
  number = NULL,
  reversed = F,
  random = F,
  center = 0.5,
  show = F,
  collapse = F
)
}
\arguments{
\item{name}{Name of the color ramp (string).}

\item{number}{Number of output colors desired (integer). Defaults to number of colors in the pallete for qualitative type and to 100 for sequential and diverging types.}

\item{reversed}{Reverse the output color order (logical).}

\item{random}{Randomize output color order (logical).}

\item{center}{Relative position of the center of the color ramp (numeric, 0-1). Useful to change centerpoint of diverging scales.}

\item{show}{Show a plot of all available color ramps (logical).}

\item{collapse}{Print a single comma-separated and quoted string of colors to stdout}
}
\value{
Vector with hex colors strings.
}
\description{
Load color ramps and palettes from presets. Identical to \code{pkrf::pal() besides some argument defaults.}
}
