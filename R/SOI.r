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

#'Shock Oscillator Index
#'
#'@aliases SOI
#'@param HLC Object that is coercible to xts or matrix and contains
#'High-Low-Close prices.
#'@param n Number of periods for moving average.
#'@param maType A function or a string naming the function to be called.
#'@param \dots Other arguments to be passed to the \code{maType} function.
#'@author Chen Chaozong
#'@seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#'options; and note Warning section.  See \code{\link{ATR}}, which uses true
#'range.  See \code{\link{chaikinVolatility}} for another volatility measure.
#'@keywords ts
#'@export
"SOI" <-
function (HLC, n = 14, maType, ...) {
  HLC <- try.xts(HLC, error = as.matrix)
  if (NCOL(HLC) == 3) {
    mtm <- momentum(Cl(HLC), n = 1, na.pad = TRUE)
    htl <- Hi(HLC)-Lo(HLC)
  } else stop("Price series must be High-Low-Close")
  maArgs <- list(n = n, ...)
  if (missing(maType)) {
    maType <- "EMA"
    maArgs$wilder <- TRUE
  }
  if (is.list(maType)) {
    maTypeInfo <- sapply(maType, is.list)
    if (!(all(maTypeInfo) && length(maTypeInfo) == 2)) {
      stop("If 'maType' is a list, you must specify\n ",
           "*two* MAs (see Examples section of ?RSI)")
    }
    for (i in 1:length(maType)) {
      if (!is.null(formals(maType[[i]])$n) && is.null(maType[[i]]$n)) {
        maType[[i]]$n <- n
      }
    }
    mavgmtm <- do.call(maType[[1]][[1]], c(list(mtm), maType[[1]][-1]))
    mavghtl <- do.call(maType[[2]][[1]], c(list(htl), maType[[2]][-1]))
  }
  else {
    mavgmtm <- do.call(maType, c(list(mtm), maArgs))
    mavghtl <- do.call(maType, c(list(htl), maArgs))
  }
  soi <- 100 * mavgmtm/mavghtl
  names(soi) <- "SOI"
  reclass(soi, HLC)
}
