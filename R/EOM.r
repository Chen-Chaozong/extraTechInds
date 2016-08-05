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

#'Energy of Movement
#'
#'@aliases EOM
#'@param OHLCV Object that is coercible to xts or matrix and contains
#'High-Low-Close prices.
#'@param n Number of periods for moving average.
#'@param m ...
#'@param maType A function or a string naming the function to be called.
#'@param \dots Other arguments to be passed to the \code{maType} function.
#'@author Chen Chaozong
#'@seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#'options; and note Warning section.  See \code{\link{ATR}}, which uses true
#'range.  See \code{\link{chaikinVolatility}} for another volatility measure.
#'@keywords ts
#'@export
"EOM" <-
function (OHLCV, n = 14, m = 1, maType, ...) {
  OHLCV <- try.xts(OHLCV, error = as.matrix)
  if (NCOL(OHLCV) == 5) {
    mp <- (Hi(OHLCV)+Lo(OHLCV))/2
  } else stop("Price series must be Open-High-Low-Close-Volume")
  if (missing(maType)) maType <- "EMA"
  roc <- Roc(mp, n = m)
  vmd <- Vo(OHLCV)/do.call(maType, c(list(Vo(OHLCV)), list(n = n, ...)))
  rmd <- (Hi(OHLCV)-Lo(OHLCV))/do.call(maType, c(list(Hi(OHLCV)-Lo(OHLCV)), list(n = n, ...)))
  eom <- roc * rmd / vmd
  names(eom) <- "EOM"
  reclass(eom, OHLCV)
}
