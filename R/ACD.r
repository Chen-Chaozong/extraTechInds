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

#'Accumulate Collection & Distribution
#'
#'@aliases ACD
#'@param HLC Object that is coercible to xts or matrix and contains
#'High-Low-Close prices.
#'@param \dots Other arguments to be passed to the \code{maType} function.
#'@author Chen Chaozong
#'@seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#'options; and note Warning section.  See \code{\link{ATR}}, which uses true
#'range.  See \code{\link{chaikinVolatility}} for another volatility measure.
#'@keywords ts
#'@export
"ACD" <-
function (HLC, ...) {
  HLC <- try.xts(HLC, error = as.matrix)
  if (NCOL(HLC) == 3) {
    mth <- Cl(HLC)-pmax(Hi(HLC), lag.xts(Cl(HLC)), na.rm = FALSE)
    mtl <- Cl(HLC)-pmin(Lo(HLC), lag.xts(Cl(HLC)), na.rm = FALSE)
    mtm <- momentum(Cl(HLC), n = 1, na.pad = TRUE)
  } else stop("Price series must be High-Low-Close")
  cd <- ifelse(mtm > 0,mtl,ifelse(mtm<0,mth,0))
  acd <- runSum(cd,cumulative = T)
  names(acd) <- "ACD"
  reclass(acd, HLC)
}
