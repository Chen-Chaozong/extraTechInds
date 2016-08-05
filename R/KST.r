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

#'Know Sure Thing
#'
#'@aliases KST
#'@param x Object that is coercible to xts or matrix and contains
#'High-Low-Close prices.
#'@param n1 Number of periods for moving average.
#'@param n2 ...
#'@param n3 ...
#'@param n4 ...
#'@param maType A function or a string naming the function to be called.
#'@param \dots Other arguments to be passed to the \code{maType} function.
#'@author Chen Chaozong
#'@seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#'options; and note Warning section.  See \code{\link{ATR}}, which uses true
#'range.  See \code{\link{chaikinVolatility}} for another volatility measure.
#'@keywords ts
#'@export
"KST" <-
function(x,n1=10,n2=13,n3=15,n4=20, maType, ...){
  x <- try.xts(x, error = as.matrix)
  if(!ncol(x)==1) stop("Price series must be one column")
  if (missing(maType)) maType <- "EMA"
  kst <- (1*do.call(maType, c(list(Roc(x,n1)), list(n = n1, ...)))+
            2*do.call(maType, c(list(Roc(x,n2)), list(n = n2, ...)))+
            3*do.call(maType, c(list(Roc(x,n3)), list(n = n3, ...)))+
            4*do.call(maType, c(list(Roc(x,n4)), list(n = n4, ...))) )/10
  names(kst) <- "KST"
  reclass(kst,x)
}
