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

#'Modified Bollinger Bands
#'
#'@aliases MBBands
#'@param x Object that is coercible to xts or matrix and contains
#'High-Low-Close prices.
#'@param n Number of periods for moving average.
#'@param f ...
#'@author Chen Chaozong
#'@seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#'options; and note Warning section.  See \code{\link{ATR}}, which uses true
#'range.  See \code{\link{chaikinVolatility}} for another volatility measure.
#'@keywords ts
#'@export
"MBBands" <-
function(x, n = 20, f = 2){
  x <- try.xts(x, error = as.matrix)
  if(!ncol(x)==1) stop("Price series must be one column")
  a <- 2/(n+1)
  Mt <- EMA(x, n = n)
  Ut <- EMA(Mt, n = n)
  Dt <- ((2-a)*Mt-Ut)/(1-a)
  mt <- EMA(abs(x-Dt), n = n)
  ut <- EMA(mt, n = n)
  dt <- ((2-a)*mt-ut)/(1-a)
  bu <- Dt + f * dt
  bl <- Dt - f * dt
  result <- cbind(Dt, bu, bl)
  colnames(result) <- c("Mid", "Upper", "Lower")
  reclass(result,x)
}
