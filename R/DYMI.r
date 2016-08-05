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

#'Dynamic Relative Momentum Index
#'
#'@aliases DYMI
#'@param price Object that is coercible to xts or matrix and contains
#'High-Low-Close prices.
#'@param n Number of periods for moving average.
#'@param d ...
#'@param m ...
#'@param maType A function or a string naming the function to be called.
#'@param \dots Other arguments to be passed to the \code{maType} function.
#'@author Chen Chaozong
#'@seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#'options; and note Warning section.  See \code{\link{ATR}}, which uses true
#'range.  See \code{\link{chaikinVolatility}} for another volatility measure.
#'@keywords ts
#'@export
"DYMI" <-
function (price, n = 14, d = 5, m = 1, maType, ...) {
  price <- try.xts(price, error = as.matrix)
  up <- momentum(price, n = m, na.pad = TRUE)
  which.dn <- which(up < 0)
  dn <- up * 0
  dn[which.dn] <- -up[which.dn]
  up[which.dn] <- 0
  sdp <- runSD(price, n = d )
  t <- floor(n * SMA(sdp, n = d)/sdp)
  t <- ifelse(is.na(t),n,ifelse(t>30, 30, ifelse(t<5 , 5, t)))
  if (missing(maType)) maType <- "EMA"
  if (!is.character(maType)) stop("'maType' should be a string!")
  mavgUp <- NULL
  mavgDn <- NULL
  for(i in 1:nrow(price) ){
    if( i <= m ){
      mavgUp <- rbind(mavgUp, up[i] )
      mavgDn <- rbind(mavgDn, dn[i] )
      next
    }
    tmp <- min(i-1,t[i])
    mavgUp <- rbind(mavgUp, tail(do.call(maType, c(list(up[(i-tmp+1):i]), n=tmp)),1) )
    mavgDn <- rbind(mavgDn, tail(do.call(maType, c(list(dn[(i-tmp+1):i]), n=tmp)),1) )
  }
  dymi <- 100 * mavgUp/(mavgUp + mavgDn)
  names(dymi) <- "DYMI"
  reclass(dymi, price)
}
