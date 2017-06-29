#' Lapply version of NSE
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
NSEfun   <- function(x){
  require(hydroGOF)
  return(NSE(sim   = x[, 1],
             obs   = x[, 2],
             na.rm = T))
}

#' Lapply version of RMSE
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
RMSEfun  <- function(x){
  require(hydroGOF)
  return(rmse(sim   = x[, 1], 
              obs   = x[, 2],
              na.rm = T))
}

#' Lapply version of PBIAS
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
PBIASfun <- function(x){
  require(hydroGOF)
  return(pbias(sim   = x[, 1],
               obs   = x[, 2],
               na.rm = T))
}

#' Lapply version of kendall correlation
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
COR.Kfun <- function(z){
  return(cor(x      = as.numeric(z[, 1]),
             y      = as.numeric(z[, 2]),
             method = 'kendall'))
  }

#' Lapply version of spearman correlation
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
COR.Sfun <- function(z){
  return(cor(x      = as.numeric(z[, 1]),
             y      = as.numeric(z[, 2]),
             method = 'spearman'))
}

#' Lapply version of difference
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
DIFfun   <- function(z){
  return(mean(x     = (z[, 1] - z[, 2]),
              na.rm = T))
}

#' Lapply version of peak function
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
PEAKfun  <- function(x){
  require(zoo)
  #Split into wateryears
  OCT <- which(regexpr(pattern = 'Oct',
                       text    = zoo::yearmon(index(x))) == 1)
  OCT <- c(1, OCT, (nrow(x)+1))
  dd  <- matrix(data = NA,
                nrow = (length(OCT) - 1),
                ncol = 2)
  for (i in 1:(length(OCT) - 1)){
    wy.sub   <- x[OCT[i]:(OCT[i+1] - 1), ]
    dd[i, 1] <- base::which.max(wy.sub[, 1])
    dd[i, 2] <- base::which.max(wy.sub[, 2])
  }
  return(mean(dd[, 1] - dd[, 2]))
}

#' Lapply version of peak2 function
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
PEAKfun2 <- function(foo){ 
  require(zoo)
  require(lubridate)
  #New method via function to apply
  years <- lubridate::year(zoo::yearmon(funIndex))
  lubridate::years[lubridate::month(zoo::yearmon(funIndex)) >= 10] =
    (lubridate::years[lubridate::month(zoo::yearmon(funIndex)) >= 10]) + 1
  bb1 <- split(x = foo[, 1],
               f = years)
  bb2 <- split(x = foo[, 2],
               f = years)
  bb3 <- Map(cbind, bb1, bb2)
  maxtiming <- function(xx){
    return(
      (apply(X      = xx,
             MARGIN = 2,
             FUN    = which.max))[1] -
        (apply(X      = xx,
               MARGIN = 2,
               FUN    = which.max))[2]
      )}
  mean(as.numeric(unlist(lapply(X   = bb3,
                                FUN = maxtiming))))
}

#' Lapply version of KS test
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
KSfun    <- function(z){
  if (nrow(na.omit(z)) == 0){
    val <- NA
    return(val)
    } else{
    val <- (suppressWarnings(
      ks.test(x = as.numeric(z[, 1]),
              y = as.numeric(z[, 2]))))$statistic
    return(val)
  }
}

#' Lapply Version of Standard Deviation
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
SDfun    <- function(z){
  return(mean(apply(X      = z,
                    MARGIN = 1,
                    FUN    = sd,
                    na.rm  = T)))
}

#' Lapply Version of Coefficient of Variation
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
CVfun    <- function(z){
  #require(raster)
  return(mean(apply(X      = z,
                    MARGIN = 1,
                    #FUN    = raster::cv,
                    FUN    = function(x, na.rm = F){return(
                      log10((sd(x, na.rm = na.rm)/mean(x, na.rm = na.rm))*100)
                      )
                      },
                    na.rm  = T)))
  }
