% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Fid_data_wrangler.R
\name{dailynlogReturn}
\alias{dailynlogReturn}
\title{Calculate Daily Return, Log close price and log return}
\usage{
dailynlogReturn(Date1, DataFrame)
}
\arguments{
\item{DataFrame}{a dataframe input that has the daily close price named: @param Closed.
and a date column named: @param Date with format as: floating point and "xxxx(year)-xx(month)-xx(day)"}
}
\value{
the dailyreturn, log closed price and log return
}
\description{
Calculate Daily Return, Log close price and log return
}
