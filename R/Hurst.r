#
#   extraTechInds: extra Technical Indicators of TTR and quantmod
#
#   Copyright (C) 2016  Chen Chaozong
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#'hurst Index
#'
#'@aliases Hurst
#'@param price Object that is coercible to xts or matrix and contains
#'High-Low-Close prices.
#'@param n Number of periods for moving average.
#'@author Chen Chaozong
#'@seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#'options; and note Warning section.  See \code{\link{ATR}}, which uses true
#'range.  See \code{\link{chaikinVolatility}} for another volatility measure.
#'@keywords ts
#'@export
"Hurst" <-
function(price, n = 14){
  price <- try.xts(price, error = as.matrix)
  np <- length(price)
  if(np < n) stop("data is not enough")
  rssimple <- function(x) {
    n <- length(x)
    y <- x - mean(x)
    s <- cumsum(y)
    rs <- (max(s) - min(s))/sd(x)
    return(log(rs)/log(n))
  }
  result <- rep(NA,n)
  for(i in n:np){
    result[i] <- rssimple(price[(i-n+1):i])
  }
  hurst <- as.xts(result,index(price))
  names(hurst) <- "Hurst"
  reclass(hurst,price)
}
