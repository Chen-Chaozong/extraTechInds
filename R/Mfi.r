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

#'Money Flow Index ( different from TTR)
#'
#'@aliases Mfi
#'@param OHLCV Object that is coercible to xts or matrix and contains
#'High-Low-Close prices.
#'@param n Number of periods for moving average.
#'@author Chen Chaozong
#'@seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#'options; and note Warning section.  See \code{\link{ATR}}, which uses true
#'range.  See \code{\link{chaikinVolatility}} for another volatility measure.
#'@keywords ts
#'@export
"Mfi" <-
function (OHLCV, n = 20) {
  OHLCV <- try.xts(OHLCV, error = as.matrix)
  if (NCOL(OHLCV) == 5) {
    tp <- (Hi(OHLCV)+Lo(OHLCV)+Cl(OHLCV))/3
    mf <- tp*Vo(OHLCV)
  } else stop("Price series must be Open-High-Low-Close-Volume")
  mtm <- momentum(tp, n = 1, na.pad = TRUE)
  pmf <- ifelse(mtm > 0, mf, 0)
  nmf <- ifelse(mtm < 0, mf, 0)
  apmf <- runSum(pmf, n)
  anmf <- runSum(nmf, n)
  mfi <- 100 * (apmf/(apmf + anmf))
  names(mfi) <- "MFI"
  reclass(mfi, OHLCV)
}
