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

#'True Strength Index
#'
#'@aliases TSI
#'@param x Object that is coercible to xts or matrix and contains
#'High-Low-Close prices.
#'@param n Number of periods for moving average.
#'@param m ...
#'@param d ...
#'@param maType A function or a string naming the function to be called.
#'@param \dots Other arguments to be passed to the \code{maType} function.
#'@author Chen Chaozong
#'@seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#'options; and note Warning section.  See \code{\link{ATR}}, which uses true
#'range.  See \code{\link{chaikinVolatility}} for another volatility measure.
#'@keywords ts
#'@export
"TSI" <-
function (x, n = 20, m = 6, d = 1, maType, ...)
{
  x <- try.xts(x, error = as.matrix)
  mtm <- momentum(x, n = d)
  netmtm <- runSum(mtm, n = n)
  allmtm <- runSum(abs(mtm), n = n)
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
      maType[[1]]$n <- n
    }
    if (!is.null(formals(maType[[2]][[1]])$n) && is.null(maType[[2]]$n)) {
      maType[[2]]$n <- m
    }
    net <- do.call(maType[[1]][[1]], c(list(netmtm), maType[[1]][-1]))
    net <- do.call(maType[[2]][[1]], c(list(net), maType[[2]][-1]))
    all <- do.call(maType[[1]][[1]], c(list(allmtm), maType[[1]][-1]))
    all <- do.call(maType[[2]][[1]], c(list(all), maType[[2]][-1]))
  }
  else {
    net <- do.call(maType, c(list(netmtm), list(n = n)))
    net <- do.call(maType, c(list(net), list(n = m)))
    all <- do.call(maType, c(list(allmtm), list(n = n)))
    all <- do.call(maType, c(list(all), list(n = m)))
  }
  tsi <- 100 * net/all
  names(tsi) <- "TSI"
  reclass(tsi, x)
}
