# Routine functions for estimation of treatment effects
# July 23, 2021


library(parallel)
library(boot)
library(MASS)


# parallel replicate
# refer to https://rdrr.io/github/grayclhn/dbframe-R-library/man/RepParallel.html
# parallel replicate

source("mclapply.hack.R")

parReplicate <- function(n, expr, simplify = "array",...) {
  answer <-
    mclapply(integer(n), eval.parent(substitute(function(...) expr)),...)
  if (!identical(simplify, FALSE) && length(answer)) 
    return(simplify2array(answer, higher = (simplify == "array")))
  else return(answer)
}


# estimation
# function for the null treatments estimation
esti.null <- function(y,x,v=NULL, nfact, dta=NULL, subset=NULL){
  if(is.null(dta)){
    if(is.null(subset)) subset <- 1:nrow(as.matrix(x))
    y <- as.matrix(y)[subset,]
    x <- as.matrix(x)[subset,]
  }else{
    if(is.null(subset)) subset <- 1:nrow(as.matrix(dta))
    y <- as.matrix(dta[subset,y])
    x <- as.matrix(dta[subset,x])
  }
  
  n <- nrow(as.matrix(x))
  dimx <- ncol(as.matrix(x))
  lambda <- log(n)/n
  
  # factor analysis
  if(is.null(v)){
    x.res <- x
    # ols Y~X
    lm.y <-lm(y~x) 
  }else{
    x.res <- lm(x~v)$res
    # ols Y~X+V
    lm.y <-lm(y~x+v) 
  }
  efa.fit <- as.matrix(factanal(x.res, factors=nfact)$loadings)
  halpha <- efa.fit*sqrt(diag(var(x.res)))
  hgamma <- solve(var(x.res))%*%halpha
  hxi <- lm.y$coef[2:(1+dimx)]
  # select confounded treatments
  confd.x <- apply(hgamma^2, 1, sum) > lambda
  if(sum(confd.x) < 2*nfact+1){
    hbeta <- hxi
    hdelta <- rep(0, nfact)
  }else{
    # estimate confounding bias using LMS
    delta.ini <-  lqs(y=hxi[confd.x], x=as.matrix(hgamma[confd.x,]), intercept=FALSE, method='lts')$coefficients
    # estimate effects
    beta.ini <- hxi - as.matrix(hgamma)%*%as.matrix(delta.ini)
    # selection of confounded null treatments
    confd.null.x <- confd.x
    confd.null.x[confd.x] <- order((beta.ini[confd.x])^2, decreasing=TRUE) > ((sum(confd.x)-nfact)/2)
    # update delta
    hgamma.confd.null <- as.matrix(hgamma[confd.null.x,])
    hxi.confd.null <-  hxi[confd.null.x]
    hdelta <- solve(t(hgamma.confd.null)%*%hgamma.confd.null) %*% t(hgamma.confd.null) %*% hxi.confd.null
    # final estimate
    hbeta <- hxi - as.matrix(hgamma)%*%hdelta
  }

  c(hbeta)
}


# function for the conventional IV estimation
esti.iv <- function(y,z,x,v=NULL, dta=NULL, subset=NULL){
  if(is.null(dta)){
    if(is.null(subset)) subset <- 1:nrow(as.matrix(x))
    y <- as.matrix(y)[subset,]
    z <- as.matrix(z)[subset,]
    x <- as.matrix(x)[subset,]
  }else{
    if(is.null(subset)) subset <- 1:nrow(as.matrix(dta))
    y <- as.matrix(dta[subset,y])
    z <- as.matrix(dta[subset,z])
    x <- as.matrix(dta[subset,x])
  }
   
  # two stage least square
  if(is.null(v)){
    lm.x <- lm(x~z)
    hatx <- lm.x$fit
    hbeta <- lm(y~hatx)$coef[-1]
  }else{
    if(is.null(dta))
      v <- v[subset,]
    else
      v <- dta[subset, v]
    lm.x <- lm(x~z+v)  
    hatx <- lm.x$fit
    hbeta <- lm(y~hatx+v)$coef[-1]
  }
  
  c(hbeta)
}


# function for the auxiliary variables estimation
esti.aux <- function(y,z,x,v=NULL, nfact, dta=NULL, subset=NULL){
  if(is.null(dta)){
    if(is.null(subset)) subset <- 1:nrow(as.matrix(x))
    y <- as.matrix(y)[subset,]
    z <- as.matrix(z)[subset,]
    x <- as.matrix(x)[subset,]
  }else{
    if(is.null(subset)) subset <- 1:nrow(as.matrix(dta))
    y <- as.matrix(dta[subset,y])
    z <- as.matrix(dta[subset,z])
    x <- as.matrix(dta[subset,x])
  }
  
  dimz <- ncol(as.matrix(z))
  dimx <- ncol(as.matrix(x))
  
  if(is.null(v)){
    lm.x <- lm(x~z)
    hxi <- lm(y~x+z)$coef[-1]
  }else{
    if(is.null(dta)){
      v <- v[subset,]
    }else{
      v <- dta[subset, v]
    }
    lm.x <- lm(x~z+v)
    hxi <- lm(y~x+z+v)$coef[-1]
  }
  # factor analysis
  x.res <- lm.x$res
  efa.fit <- as.matrix(factanal(x.res, factors=nfact)$loadings)
  halpha <- efa.fit*sqrt(diag(var(x.res)))
  hgamma <- solve(var(x.res))%*%halpha
  heta <- as.matrix(t(lm.x$coef)[,c(2:(dimz+1))])
  
  # estimate confounding bias
  hdelta <- -solve(t(hgamma)%*%heta%*%t(heta)%*%hgamma) %*% (t(hgamma)%*%heta) %*% (hxi[(dimx+1):(dimx+dimz)])
  # estimate effects
  hbeta <- hxi[1:dimx] - hgamma%*%hdelta
  
  c(hbeta)
}


# function for the proximal inference estimation
esti.pi <- function(y,z,x,w,v=NULL, dta=NULL, subset=NULL){
  if(is.null(dta)){
    if(is.null(subset)) subset <- 1:nrow(as.matrix(x))
    y <- as.matrix(y)[subset,]
    z <- as.matrix(z)[subset,]
    x <- as.matrix(x)[subset,]
    w <- as.matrix(w)[subset,]
  }else{
    if(is.null(subset)) subset <- 1:nrow(as.matrix(dta))
    y <- as.matrix(dta[subset,y])
    z <- as.matrix(dta[subset,z])
    x <- as.matrix(dta[subset,x])
    w <- as.matrix(dta[subset,w])
  }
  
  dimx <- ncol(as.matrix(x))
  
  # proximal inference method
  if(is.null(v)){
    lm.w <- lm(w~x+z)
    hatw <- lm.w$fit
    lm.y <- lm(y~x+hatw)
  }else{
    if(is.null(dta)){
      v <- v[subset,]
    }else{
      v <- dta[subset, v]
    }
    
    lm.w <- lm(w~x+v+z)
    hatw <- lm.w$fit
    lm.y <- lm(y~x+v+hatw)
  }
  hbeta <-  lm.y$coef[2:(dimx+1)]
  
  c(hbeta)
}


# function for the crude estimation
esti.ols <- function(y,x, dta=NULL, subset=NULL){
  if(is.null(dta)){
    if(is.null(subset)) subset <- 1:nrow(as.matrix(x))
    y <- as.matrix(y)[subset,]
    x <- as.matrix(x)[subset,]
  }else{
    if(is.null(subset)) subset <- 1:nrow(as.matrix(dta))
    y <- as.matrix(dta[subset,y])
    x <- as.matrix(dta[subset,x])
  }
  
  # crude estimation with ols
  lm.y <- lm(y~x)
  hxi <-  lm.y$coef[-1]
  
  c(hxi)
}


# bootstrap confidence interval
bt.ci <- function(level=NULL, ...){
  # bootstrap estimates
  bt.esti <- boot(...)$t
  # percentile confidence interval
  if(is.null(level)){
    return(bt.esti)
  }else{
    ci <- apply(bt.esti, 2, quantile,
                probs=c((1-level)/2, (1+level)/2))
    
    return(ci)
    }
}

