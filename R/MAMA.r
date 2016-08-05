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

#'MESA Adaptive Moving Average
#'
#'@aliases MAMA
#'@param x Object that is coercible to xts or matrix and contains
#'High-Low-Close prices.
#'@param fastlimit ...
#'@param slowlimit ...
#'@author Chen Chaozong
#'@seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#'options; and note Warning section.  See \code{\link{ATR}}, which uses true
#'range.  See \code{\link{chaikinVolatility}} for another volatility measure.
#'@keywords ts
#'@export
"MAMA" <-
function(x, fastlimit = 0.5, slowlimit = 0.05){
  x <- try.xts(x, error = as.matrix)
  if(!ncol(x)==1) stop("Price series must be one column")
  theta <- atan( (0.0962*(x-lag.xts(x,6))+0.5769*(lag.xts(x,2)-lag.xts(x,4)))/lag.xts(x,3) )
  thetam<- momentum(theta)
  SC <- fastlimit/thetam
  SC <- ifelse(is.na(SC),fastlimit,ifelse(SC<0, fastlimit, ifelse(SC>slowlimit, slowlimit, SC)))
  mama <- as.xts(rep(NA,nrow(x)),index(x))
  for(i in 2:nrow(x)){
    if(i==2){
      mama[i] <- SC[i]*x[i]+(1-SC[i])*lag.xts(x,1)
    } else
      mama[i] <- SC[i]*x[i]+(1-SC[i])*lag.xts(mama,1)
  }
  names(mama) <- "MAMA"
  reclass(mama,x)
}
