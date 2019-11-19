pacman::p_load(tidyverse)

## this function requires a dataframe input that has the daily close price named:Closed and a date column named: Date with format as: floating point and "xxxx(year)-xx(month)-xx(day)"
## We trimmed the data from 1990-10-07 because 1990-10-08 is a Monday and Stock market closed during weekend
dailynlogReturn <- function(Date1,DataFrame){
  DataFrame = dplyr::mutate(DataFrame,
                     dailyReturn = (Close-dplyr::lag(Close))/Close,
                     log.Close = log(Close),
                     log.Return = log.Close-dplyr::lag(log.Close))%>%
    dplyr::mutate(perc_dailyRe = round(dailyReturn*100.0,3))%>%
    dplyr::filter(Date >= Date1)%>%
    dplyr::filter(Date <= as.Date("2018-12-31"))
}
## This function returns a projection value of the fund from the start date and assuming 10k investment from the start and reinvest all earnings
getProjectionValue <- function(DF){
  P0 = dplyr::pull(dplyr::filter(DF, Date == dplyr::pull(dplyr::top_n(DF["Date"],-1)))%>%dplyr::select(Close))
  DF = dplyr::mutate(DF,ProjValper10k = (Close*10000)/P0)
}
## Calculate Euclidean distances between two sets of data
sqerr <- function(x,y){
  z = x - y
  z = sqrt(dot(z,z)/length(y))
  return(z)
}

## function for standarize NAV
standardizedNAV = function(DF){
  return(dplyr::mutate(DF,Close.z = (Close-mean(Close))/sd(Close)))
}
