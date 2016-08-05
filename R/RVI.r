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

#'Relative Vigor Index
#'
#'@aliases RVI
#'@param OHLC Object that is coercible to xts or matrix and contains
#'High-Low-Close prices.
#'@param n Number of periods for moving average.
#'@author Chen Chaozong
#'@seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#'options; and note Warning section.  See \code{\link{ATR}}, which uses true
#'range.  See \code{\link{chaikinVolatility}} for another volatility measure.
#'@keywords ts
#'@export
"RVI" <-
function(OHLC, n = 14){
  HLC <- try.xts(OHLC, error = as.matrix)
  if (NCOL(OHLC) == 4) {
    open <- Op(OHLC)
    high <- Hi(OHLC)
    low  <- Lo(OHLC)
    close<- Cl(OHLC)
  } else stop("Price series must be Open-High-Low-Close")
  wts <- if(n%%2==0) c(1:(n/2),(n/2):1) else c(1:ceiling(n/2),floor(n/2):1)
  rvi <- WMA(close-open, n = n, wts = wts)/WMA(high-low, n = n, wts = wts)*100
  names(rvi) <- "RVI"
  reclass(rvi,OHLC)
}
