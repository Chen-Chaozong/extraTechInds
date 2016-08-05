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

#'Chaikin Money Flow
#'
#'@aliases Cmf
#'@param OHLCV Object that is coercible to xts or matrix and contains
#'High-Low-Close prices.
#'@param n Number of periods for moving average.
#'@author Chen Chaozong
#'@seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#'options; and note Warning section.  See \code{\link{ATR}}, which uses true
#'range.  See \code{\link{chaikinVolatility}} for another volatility measure.
#'@keywords ts
#'@export
"Cmf" <-
function (OHLCV, n = 20) {
  OHLCV <- try.xts(OHLCV, error = as.matrix)
  if (NCOL(OHLCV) == 5) {
    ad  <- ifelse(Hi(OHLCV)>Lo(OHLCV),(2*Cl(OHLCV)-Lo(OHLCV)-Hi(OHLCV))/(Hi(OHLCV)-Lo(OHLCV))*Vo(OHLCV),0)
  } else stop("Price series must be Open-High-Low-Close-Volume")
  cmf <- runSum(ad, n)/runSum(Vo(OHLCV), n)
  names(cmf) <- "CMF"
  reclass(cmf, OHLCV)
}
