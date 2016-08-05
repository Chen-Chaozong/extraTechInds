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

#'Divergence Index
#'
#'@aliases DI
#'@param price Object that is coercible to xts or matrix and contains
#'High-Low-Close prices.
#'@param fast ...
#'@param slow ...
#'@param n Number of periods for moving average.
#'@param f ...
#'@param m ...
#'@param maType A function or a string naming the function to be called.
#'@param \dots Other arguments to be passed to the \code{maType} function.
#'@author Chen Chaozong
#'@seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#'options; and note Warning section.  See \code{\link{ATR}}, which uses true
#'range.  See \code{\link{chaikinVolatility}} for another volatility measure.
#'@keywords ts
#'@export
"DI" <-
function (price, fast = 5, slow = 20, n = 10, f = 2, m = 1, maType, ...) {
  price <- try.xts(price, error = as.matrix)
  if (missing(maType)) {
    maType <- "EMA"
  }
  if (is.list(maType)) {
    maTypeInfo <- sapply(maType, is.list)
    if (!(all(maTypeInfo) && length(maTypeInfo) == 2)) {
      stop("If 'maType' is a list, you must specify\n ",
           "*two* MAs (see Examples section of ?RSI)")
    }
    if (!is.null(formals(maType[[1]][[1]])$n) && is.null(maType[[1]]$n)) {
      maType[[1]]$n <- fast
    }
    if (!is.null(formals(maType[[2]][[1]])$n) && is.null(maType[[2]]$n)) {
      maType[[2]]$n <- slow
    }
    maFast <- do.call(maType[[1]][[1]], c(list(price), maType[[1]][-1]))
    maSlow <- do.call(maType[[2]][[1]], c(list(price), maType[[2]][-1]))
  }
  else {
    maFast <- do.call(maType, c(list(price), list(n = fast)))
    maSlow <- do.call(maType, c(list(price), list(n = slow)))
  }
  mtm <- momentum(price, n = m, na.pad = TRUE)
  sdmtm <- runSD(mtm, n= n)
  di <- (maFast - maSlow)/(sdmtm**2)
  band <- runSD(di, n = n)
  result <- cbind(di,f*band,-f*band)
  colnames(result) <- c("DI","BandUp","BandDown")
  reclass(result, price)
}
