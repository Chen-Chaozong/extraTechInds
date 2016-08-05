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

#'Directional Movement Index
#'
#'@aliases DMI
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
"DMI" <-
function(HLC, n = 14, maType, ...){
  HLC <- try.xts(HLC, error = as.matrix)
  if(!ncol(HLC)==3) stop("Price series must be either High-Low-Close")
  dH <-  momentum(Hi(HLC))
  dL <- -momentum(Lo(HLC))
  DMIp <- ifelse(dH == dL | (dH < 0 & dL < 0), 0, ifelse(dH > dL, dH, 0))
  DMIn <- ifelse(dH == dL | (dH < 0 & dL < 0), 0, ifelse(dH < dL, dL, 0))
  TR <- ATR(HLC)[, "tr"]
  TRsum <- wilderSum(TR, n = n)
  DIp <- 100 * wilderSum(DMIp, n = n)/TRsum
  DIn <- 100 * wilderSum(DMIn, n = n)/TRsum
  DX  <- 100 * (abs(DIp - DIn)/(DIp + DIn))
  maArgs <- list(n = n, ...)
  if (missing(maType)) {
    maType <- "EMA"
    maArgs$wilder <- TRUE
  }
  ADX <- do.call(maType, c(list(DX), maArgs))
  ADXR<- (ADX+lag.xts(ADX,n))/2
  result <- cbind(DIp, DIn, DX, ADX, ADXR)
  colnames(result) <- c("DIp", "DIn", "DX", "ADX", "ADXR")
  reclass(result, HLC)
}
