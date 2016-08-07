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

#' Momentum (more flexible)
#'
#' @aliases Momentum
#' @param p1 first price column
#' @param p2 second price column
#' @param nlag lag period
#' @param type momentum type "diff" or "ratio"
#' @param lable name of the momentum
#' @param maType A function or a string naming the function to be called.
#' @param \dots Other arguments to be passed to the \code{maType} function.
#' @author Chen Chaozong
#' @export

"Momentum" <- function(p1,p2=p1,nlag,type="diff",lable=NULL,maType=NULL,...){
	if(!any(type==c("diff","ratio"))) stop ('type must be one of c("diff","ratio")')
	if(missing(nlag) | !is.numeric(nlag) | nlag <0 ) stop("nlag must be a nonnegative numeric")
	
	# only the first column of p1 and p2 will be used
	p1 <- try.xts(p1[,1],error = as.matrix)
	p2 <- lag(try.xts(p2[,1],error = as.matrix),nlag)
	
	# if not use moving average
	if (is.null(maType)) {    
        momentum <- 
	        switch(type,
	        	"diff"=p1-p2,
	        	"ratio"=p1/p2-1
	        	)
		colnames(momentum) <- if(!is.null(lable)) lable else "momentum" 
        return(momentum)
    }

    # use moving average
    if (!is.list(maType)) stop("'maType' should be a list contains MAs and the arguments e.g. list('EMA',n=10)  etc.")
    maTypeInfo <- sapply(maType, is.list)
    if(length(maTypeInfo)>2) stop(" max quantity of maType is two")
    if(all(maTypeInfo) & length(maTypeInfo)==2){
        if (!is.null(formals(maType[[1]][[1]])$n) && is.null(maType[[1]]$n)) {
            stop(paste("missing arguments n for ",maType[[1]][[1]]))
        }
        if (!is.null(formals(maType[[2]][[1]])$n) && is.null(maType[[2]]$n)) {
            stop(paste("missing arguments n for ",maType[[2]][[1]]))
        }
        mavg.p1 <- do.call(maType[[1]][[1]], c(list(p1), maType[[1]][-1]))
        mavg.p2 <- do.call(maType[[2]][[1]], c(list(p2), maType[[2]][-1]))
    } else if(!is.list(maType[[1]]) & !any(maTypeInfo)) {
    	if (!is.null(formals(maType[[1]])$n) && is.null(maType$n)) {
            stop(paste("missing arguments n for ",maType[[1]]))
        }
        mavg.p1 <- do.call(maType[[1]], c(list(p1), maType[-1]))
        mavg.p2 <- do.call(maType[[1]], c(list(p2), maType[-1]))
    } else stop("Incorrect maType!")

    momentum <- 
    switch(type,
    	"diff"=mavg.p1-mavg.p2,
    	"ratio"=mavg.p1/mavg.p2-1
    	)
	colnames(momentum) <- if(!is.null(lable)) lable else "momentum" 
	return(momentum)
}


#' MomRatio (more flexible momentum ratio)
#'
#' @aliases MomRatio
#' @param positive first price column
#' @param negative second price column
#' @param type momentum type in "positive","negative","net"
#' @param maType A function or a string naming the function to be called. for positive and negative momentum
#' @param lable name of the momentum
#' @param ratiomaType A function or a string naming the function to be called. for numerator and denominator
#' @param \dots Other arguments to be passed to the \code{maType} function.
#' @author Chen Chaozong
#' @export
"MomRatio" <- function(positive, negative=positive, type="positive", maType, lable=NULL,  ratiomaType=NULL, ...){
	if(!any(type==c("positive","negative","net"))) stop ('type must be one of c("positive","negative","net")')
	if (missing(maType)) stop("you must specify at least one MA to calculate positive and negative momentums, e.g. list('EMA',n=10)")
	positive <- try.xts(positive[,1],error = as.matrix)
	negative <- try.xts(negative[,1],error = as.matrix)
	positive <- ifelse(positive>0, positive,0)
	negative <- ifelse(negative<0,-negative,0)
	
	# calculate moving average positive and negative momentums
	if (!is.list(maType)) stop("'maType' should be a list contains MAs and the arguments e.g. list('EMA',n=10)  etc.")
    maTypeInfo <- sapply(maType, is.list)
    if(length(maTypeInfo)>2) stop(" max quantity of maType is two")
    if(all(maTypeInfo) & length(maTypeInfo)==2){
        if (!is.null(formals(maType[[1]][[1]])$n) && is.null(maType[[1]]$n)) {
            stop(paste("missing arguments n for ",maType[[1]][[1]]))
        }
        if (!is.null(formals(maType[[2]][[1]])$n) && is.null(maType[[2]]$n)) {
            stop(paste("missing arguments n for ",maType[[2]][[1]]))
        }
        mavg.positive <- do.call(maType[[1]][[1]], c(list(positive), maType[[1]][-1]))
        mavg.negative <- do.call(maType[[2]][[1]], c(list(negative), maType[[2]][-1]))
    } else if(!is.list(maType[[1]]) & !any(maTypeInfo)) {
    	if (!is.null(formals(maType[[1]])$n) && is.null(maType$n)) {
            stop(paste("missing arguments n for ",maType[[1]]))
        }
        mavg.positive <- do.call(maType[[1]], c(list(positive), maType[-1]))
        mavg.negative <- do.call(maType[[1]], c(list(negative), maType[-1]))
    } else stop("Incorrect maType!")

    numerator  <- 
        switch(type,
        	"positive"= mavg.positive,
        	"negative"=-mavg.negative,
        	"net"=(mavg.positive-mavg.negative)
        	)
    denominator <- mavg.positive + mavg.negative
	
	# if not use moving average to calculate the ratio
	if (is.null(ratiomaType)) {    
	    momratio <- numerator/denominator*100
		colnames(momratio) <- if(!is.null(lable)) lable else paste(type,"ratio",sep=".")
        return(momratio)
    }

    # use moving average
    if (is.null(ratiomaType)) stop("you must specify at least one MA to calculate positive and negative momentums, e.g. list('EMA',n=10)")
	if (!is.list(ratiomaType)) stop("'ratiomaType' should be a list contains MAs and the arguments e.g. list('EMA',n=10)  etc.")
    ratiomaTypeInfo <- sapply(ratiomaType, is.list)
    if(length(ratiomaTypeInfo)>2) stop(" max quantity of ratiomaType is two")
    if(all(ratiomaTypeInfo) & length(ratiomaTypeInfo)==2){
        if (!is.null(formals(ratiomaType[[1]][[1]])$n) && is.null(ratiomaType[[1]]$n)) {
            stop(paste("missing arguments n for ",ratiomaType[[1]][[1]]))
        }
        if (!is.null(formals(ratiomaType[[2]][[1]])$n) && is.null(ratiomaType[[2]]$n)) {
            stop(paste("missing arguments n for ",ratiomaType[[2]][[1]]))
        }
        mavg.numerator <- do.call(ratiomaType[[1]][[1]], c(list(numerator), ratiomaType[[1]][-1]))
        mavg.denominator <- do.call(ratiomaType[[2]][[1]], c(list(denominator), ratiomaType[[2]][-1]))
    } else if(!is.list(ratiomaType[[1]]) & !any(ratiomaTypeInfo)) {
    	if (!is.null(formals(ratiomaType[[1]])$n) && is.null(ratiomaType$n)) {
            stop(paste("missing arguments n for ",ratiomaType[[1]]))
        }
        mavg.numerator <- do.call(ratiomaType[[1]], c(list(numerator), ratiomaType[-1]))
        mavg.denominator <- do.call(ratiomaType[[1]], c(list(denominator), ratiomaType[-1]))
    } else stop("Incorrect ratiomaType!")

    momratio <- mavg.numerator/mavg.denominator*100
	colnames(momratio) <- if(!is.null(lable)) lable else paste(type,"ratio",sep=".")
	return(momratio)
}