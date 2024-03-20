library(tidyverse)
library(readxl)
library(lubridate)
library(mgcv)
library(ggh4x)
library(maxLik)
options(scipen=999)

# estimation of baseline dts with bootstrapped prediction intervals
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
est_baseline_pi <- function(chunk){
  
  chunk2 <- 
    chunk %>% 
    arrange(year) %>% 
    mutate(w = ifelse(year >= 2020, 0, 1),
           t = 1:n())
  
  model <- glm(dts ~ year + offset(log(exposure)), 
               weights = w,
               family = "quasipoisson",
               data = chunk2)
  
  test <- 
    try(
      res <- 
        predict(model, 
                newdata = chunk2,
                type = "response", 
                se.fit = TRUE)
    )
  
  try(
    chunk3 <- 
      chunk2 %>% 
      mutate(bsn = res$fit,
             lc = bsn - 1.96 * res$se.fit,
             uc = bsn + 1.96 * res$se.fit) %>% 
      left_join(simul_intvals(model, 
                              model_type = "glm", 
                              db = chunk2, 
                              nsim = 200,
                              p = 0.95),
                by = "t")
  )
  
  if(class(test) == "try-error"){
    chunk3 <- 
      chunk2 %>% 
      mutate(bsn = NA,
             lp = NA,
             up = NA,
             lc = NA,
             uc = NA)
  }
  
  return(chunk3)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function for bootstrapping ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# bootstrapping process, adapted from Jonas schoeley's method 
# https://github.com/jschoeley/rbx2020
simul_intvals <- 
  function(
    # fitted model 
    model, 
    # either GLM or GAM (needed for model matrix extraction step)
    model_type, 
    # prediction data
    db, 
    # number of iterations
    nsim, 
    # prediction intervals' uncertainty level (between 0 and 1)
    p
  ){
    
    # defining upper and lower prediction quantiles
    lp <- (1 - p) / 2
    up <- 1 - lp
    
    # matrix model extraction
    if(model_type == "glm"){
      X_prd <- model.matrix(model, data = db, na.action = na.pass)
    }
    if(model_type == "gam"){
      X_prd <- predict(model, newdata = db, type = 'lpmatrix')
    }
    
    # estimated coefficients
    beta <- coef(model)
    
    # offsets extracted directly from the prediction data
    offset_prd <- matrix(log(db$exposure))
    # model.offset(x)
    
    # extracting variance covariance matrix
    beta_sim <- MASS::mvrnorm(nsim, 
                              coef(model), 
                              suppressWarnings(vcov(model)))
    
    # simulation process
    Ey_sim <- apply(beta_sim, 1, FUN = function (b) exp(X_prd %*% b + offset_prd))
    
    y_sim <- apply(Ey_sim, 2, FUN = function (Ey) {
      y <- mu <- Ey
      # NA's can't be passed to the simulation functions, so keep them out
      idx_na <- is.na(mu) 
      mu_ <- mu[!idx_na] 
      N <- length(mu_)
      phi <- suppressWarnings(summary(model)$dispersion)
      # in case of under-dispersion, sample from Poisson
      if (phi < 1) { phi = 1 }
      y[!idx_na] <- rnbinom(n = N, mu = mu_, size = mu_/(phi-1))      
      return(y)
    })
    
    # from wide to tidy format
    ints_simul <- 
      db %>% 
      select(t)
    
    colnames_y_sim <- paste0('dts_sim', 1:nsim)
    
    ints_simul[,colnames_y_sim] <- y_sim
    
    # prediction intervals output
    ints_simul <-
      ints_simul %>%
      pivot_longer(cols = starts_with('dts_sim'),
                   names_to = 'sim_id', values_to = 'dts_sim') %>%
      group_by(t) %>%
      summarise(
        lp = quantile(dts_sim, lp, na.rm = TRUE),
        up = quantile(dts_sim, up, na.rm = TRUE), 
        .groups = 'drop'
      ) 
    
    return(ints_simul)
  }


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# functions for fractional multinomial logit model
# imported from fmlogit package: https://github.com/f1kidd/fmlogit/
# author: Xinde James Ji
# based on  Papke and Wooldridge (1996).

# function to fit the model in this data
fit_fractions <- function(chunk){
  
  ct <- unique(chunk$country)
  sx <- unique(chunk$sex)
  ag <- unique(chunk$age)
  
  chunk2 <- 
    chunk %>%  
    select(year, cause, frc) %>% 
    spread(cause, frc) 
  
  X <- chunk2 %>% pull(year)
  y <- chunk2[,2:ncol(chunk2)] %>% as.data.frame()
  
  mod <- fmlogit(y,X)
  # summary.fmlogit(model)
  
  X2 <- 2020
  
  fit <- predict.fmlogit(mod, newdata = X2)
  
  chunk3 <- 
    chunk2 %>% 
    bind_rows(fit %>% as_tibble() %>% mutate(year = X2)) %>% 
    mutate(country = ct,
           sex = sx,
           age = ag) %>% 
    gather(-year, -country, -sex, -age, key = cause, value = frc)
  
  return(chunk3)
}

fmlogit=function(y, X, beta0 = NULL, MLEmethod = "CG", maxit = 1e+03, 
                 abstol = 1e-05,cluster=NULL,reps=1000, ...){
  start.time = proc.time()
  
  if(length(cluster)!=nrow(y) & !is.null(cluster)){
    warning("Length of the cluster does not match the data. Cluster is ignored.")
    cluster = NULL
  }
  Xclass = sapply(X, class)
  Xfac = which(Xclass %in% c("factor", "character"))
  if (length(Xfac) > 0) {
    Xfacnames = colnames(X)[Xfac]
    strformFac = paste(Xfacnames, collapse = "+")
    Xdum = model.matrix(as.formula(paste("~", strformFac, 
                                         sep = "")), data = X)[, -1]
    X = cbind(X, Xdum)
    X = X[, -Xfac]
  }
  Xnames = colnames(X)
  ynames = colnames(y)
  X = as.matrix(X)
  y = as.matrix(y)
  n = dim(X)[1]
  j = dim(y)[2]
  k = dim(X)[2]
  xy = cbind(X, y)
  xy = na.omit(xy)
  row.remain = setdiff(1:n, attr(xy, "na.action"))
  X = xy[, 1:k]
  y = xy[, (k + 1):(k + j)]
  n = dim(y)[1]
  remove(xy)
  # adding in the constant term
  if(k==1){
    # check if the input X is constant
    if(length(unique(X))==1){ # X is constant
      Xnames = "constant"
      X = as.matrix(as.numeric(X),nrow=1)
      colnames(X) = Xnames
      X = as.matrix(X)
      k=0
    }else{ # one single variable of input
      Xnames = "X1"
      X = as.matrix(X)
      k = dim(X)[2]
      X = cbind(X, rep(1, n))
      Xnames = c(Xnames, "constant")
      colnames(X) = Xnames
    }
  }else{ # normal cases
    X = X[, apply(X, 2, function(x) length(unique(x)) != 1)]
    Xnames = colnames(X)
    k = dim(X)[2]
    X = cbind(X, rep(1, n))
    Xnames = c(Xnames, "constant")
    colnames(X) = Xnames
  }
  
  
  testcols <- function(X) {
    m = crossprod(as.matrix(X))
    ee = eigen(m)
    evecs <- split(zapsmall(ee$vectors), col(ee$vectors))
    mapply(function(val, vec) {
      if (val != 0) 
        NULL
      else which(vec != 0)
    }, zapsmall(ee$values), evecs)
  }
  collinear = unique(unlist(testcols(X)))
  while (length(collinear) > 0) {
    if (qr(X)$rank == dim(X)[2]) 
      print("Model may suffer from multicollinearity problems.")
    break
    if ((k + 1) %in% collinear) 
      collinear = collinear[-length(collinear)]
    X = X[, -collinear[length(collinear)]]
    Xnames = colnames(X)
    k = k - 1
    collinear = unique(unlist(testcols(X)))
  }
  QMLE <- function(betas) {
    betas = matrix(betas, nrow = j - 1, byrow = T)
    betamat = rbind(rep(0, k + 1), betas)
    llf = 0
    for (i in 1:j) {
      L = y[, i] * ((X %*% betamat[i, ]) - log(rowSums(exp(X %*% 
                                                             t(betamat)))))
      llf = llf + sum(L)
    }
    return(llf)
  }
  QMLE_Obs <- function(betas) {
    betas = matrix(betas, nrow = j - 1, byrow = T)
    betamat = rbind(rep(0, k + 1), betas)
    llf = rep(0, n)
    for (i in 1:j) {
      L = y[, i] * ((X %*% betamat[i, ]) - log(rowSums(exp(X %*% 
                                                             t(betamat)))))
      llf = llf + L
    }
    return(llf)
  }
  if (length(beta0) == 0){
    beta0 = rep(0, (k + 1) * (j - 1))
  }
  if (length(beta0) != (k + 1) * (j - 1)) {
    beta0 = rep(0, (k + 1) * (j - 1))
    warning("Wrong length of beta0 given. Use default setting instead.")
  }
  opt <- maxLik(QMLE_Obs, start = beta0, method = MLEmethod, 
                control = list(iterlim = maxit, tol = abstol), ...)
  betamat = matrix(opt$estimate, ncol = k + 1, byrow = T)
  betamat_aug = rbind(rep(0, k + 1), betamat)
  colnames(betamat_aug) = Xnames
  rownames(betamat_aug) = ynames
  sigmat = matrix(nrow = j - 1, ncol = k + 1)
  vcov = list()
  
  ###insert--nonparametric bootstrap procedure (clustered SE and vcov)
  
  if(is.null(cluster)==F){
    cluster = cluster[row.remain]
    clusters <- names(table(cluster))
    for (i in 1:j) {
      # cluster should preferably be coming from a same data frame with the original y and X. 
      sterrs <- matrix(NA, nrow=reps, ncol=k + 1)
      vcov_j_list=list()
      
      b=1
      no_singular_error=c()
      while(b<=reps){
        
        index <- sample(1:length(clusters), length(clusters), replace=TRUE)
        aa <- clusters[index]
        bb <- table(aa)
        bootdat <- NULL
        dat=cbind(y,X)
        for(b1 in 1:max(bb)){
          cc <- dat[cluster %in% names(bb[bb %in% b1]),]
          for(b2 in 1:b1){
            bootdat <- rbind(bootdat, cc)
          }
        }
        
        bootdatX=matrix(bootdat[,(j+1):ncol(bootdat)],nrow=nrow(bootdat))
        bootdaty=bootdat[,1:j]
        
        sum_expxb = rowSums(exp(bootdatX %*% t(betamat_aug)))
        expxb = exp(bootdatX %*% betamat_aug[i, ])
        G = expxb/sum_expxb
        g = (expxb * sum_expxb - expxb^2)/sum_expxb^2
        X_a = bootdatX * as.vector(sqrt(g^2/(G * (1 - G))))
        A = t(X_a) %*% X_a
        mu = bootdaty[, i] - G
        X_b = bootdatX * as.vector(mu * g/G/(1 - G))
        B = t(X_b) %*% X_b
        
        a_solve_error = tryCatch(solve(A),error=function(e){NULL})
        if(is.null(a_solve_error)){
          no_singular_error=c(no_singular_error,b)
          next
        }
        
        Var_b = solve(A) %*% B %*% solve(A)
        std_b = sqrt(diag(Var_b))
        sterrs[b,]=std_b
        vcov_j_list[[b]]=Var_b
        
        b=b+1
      }
      if(length(no_singular_error)>0){warning(paste('Error in solve.default(A) : Lapack routine dgesv: system is exactly singular: U[28,28] = 0" Appeared',length(no_singular_error),'times within cluster bootstrap for outcome #',i))}
      std_b=apply(sterrs,2,mean)
      vcov[[i]] = Reduce("+", vcov_j_list) / length(vcov_j_list)
      if (i > 1) 
        sigmat[i - 1, ] = std_b
    }
  }else{
    for(i in 1:j){
      # start calculation  
      sum_expxb = rowSums(exp(X %*% t(betamat_aug))) # sum of the exp(x'b)s
      expxb = exp(X %*% betamat_aug[i,]) # individual exp(x'b)
      G = expxb / sum_expxb # exp(X'bj) / sum^J(exp(X'bj))
      g = (expxb * sum_expxb - expxb^2) / sum_expxb^2 # derivative of the logit function
      
      # Here the diagonal of A is the 'standard' standard error
      # hat(A) = sum hat(gi)^2 * xi'xi / hat(Gi)(1-hat(Gi))
      # or, Xtilde = X * sqrt(g^2/G(1-G)), A = Xtilde'Xtilde
      X_a = X * as.vector(sqrt(g^2/(G*(1-G))))
      A = t(X_a) %*% X_a
      
      # robust standard error, again following PW(1996)
      mu = y[,i] - G
      X_b = X * as.vector(mu * g / G / (1-G))
      B = t(X_b) %*% X_b
      Var_b = solve(A) %*% B %*% solve(A)
      std_b = sqrt(diag(Var_b))
      # std_b= sqrt(diag(solve(A))) is the "unrobust" standard error. 
      vcov[[i]] = Var_b
      if(i>1) sigmat[i-1,] = std_b
    }
  }
  
  ###end of insert--nonparametric bootstrap procedure (clustered SE and vcov)
  
  listmat = list()
  for (i in 1:(j - 1)) {
    tabout = matrix(ncol = 4, nrow = k + 1)
    tabout[, 1:2] = t(rbind(betamat[i, ], sigmat[i, ]))
    tabout[, 3] = tabout[, 1]/tabout[, 2]
    tabout[, 4] = 2 * (1 - pnorm(abs(tabout[, 3])))
    colnames(tabout) = c("estimate", "std", "z", "p-value")
    if (length(Xnames) > 0) 
      rownames(tabout) = Xnames
    listmat[[i]] = tabout
  }
  if (length(ynames) > 0) 
    names(listmat) = ynames[2:j]
  outlist = list()
  outlist$estimates = listmat
  outlist$baseline = ynames[1]
  outlist$likelihood = opt$maximum
  outlist$conv_code = opt$code
  outlist$convergence = paste(opt$type, paste(as.character(opt$iterations), 
                                              "iterations"), opt$message, sep = ",")
  outlist$count = c(Obs = n, Explanatories = k, Choices = j)
  outlist$y = y
  outlist$X = X
  outlist$rowNo = row.remain
  outlist$coefficient = betamat_aug
  names(vcov) = ynames
  outlist$vcov = vcov
  outlist$cluster = cluster
  outlist$reps=ifelse(is.null(cluster),0,reps)
  
  print(paste("Fractional logit model estimation completed. Time:", 
              round(proc.time()[3] - start.time[3], 1), "seconds"))
  return(structure(outlist, class = "fmlogit"))
}



fitted.fmlogit <-function(object){
  j=length(object$estimates)+1; k=dim(object$estimates[[1]])[1]; N=dim(object$y)[1]
  betamat_aug = object$coefficient; X=object$X; y=object$y
  sum_expxb = rowSums(exp(X %*% t(betamat_aug))) # sum of the exp(x'b)s
  yhat = y
  for(i in 1:j){
    expxb = exp(X %*% betamat_aug[i,]) # individual exp(x'b)
    yhat[,i] = expxb / sum_expxb
  }
  return(as.data.frame(yhat))
}

#' @rdname fitted.fmlogit
#' @export residuals.fmlogit
#' 
residuals.fmlogit <- function(object){
  yhat = fitted(object)
  return(as.data.frame(object$y-yhat))
}

#' @rdname fitted.fmlogit
#' @export predict.fmlogit
#' 
predict.fmlogit <- function(object,newdata=NULL,newbeta = NULL){
  if(length(newdata)==0) return(fitted(object))
  if(length(newbeta)>0) object$coefficient = newbeta
  j=length(object$estimates)+1; k=dim(object$estimates[[1]])[1]; N=dim(object$y)[1]
  betamat_aug = object$coefficient;
  newdata = as.matrix(newdata)
  if(length(newdata) == dim(newdata)[1]) newdata = t(newdata) # vector
  if(k != dim(newdata)[2]+1) stop(paste("Dimension of newdata is wrong. Should be",k-1,"instead of",dim(newdata)[2]))
  X = cbind(newdata,1); N = dim(X)[1]
  yhat = matrix(ncol=j,nrow=N); colnames(yhat) = colnames(object$y)
  sum_expxb = rowSums(exp(X %*% t(betamat_aug))) # sum of the exp(x'b)s
  for(i in 1:j){
    expxb = exp(X %*% betamat_aug[i,]) # individual exp(x'b)
    yhat[,i] = expxb / sum_expxb
  }
  return(as.data.frame(yhat))
}



#' Generate summary tables for fmlogit objects
#' 
#' Generate tables of coefficient estimates, partial effects, and willingness to pay from
#' fmlogit-type objects. 
#' 
#' @name summary.fmlogit
#' @aliases summary.fmlogit.margins
#' @aliases summary.fmlogit.wtp
#' 
#' @param object an object with class "fmlogit", "fmlogit.margins", or "fmlogit.wtp". 
#' @param varlist select a subset of variable names to be processed. Default to NULL, of which all variables will
#' be processed.
#' @param sepline whether the output table uses separate lines for coefficients and standard errors. 
#' @param digits number of digits to be signifed. Default to show 3 digits. 
#' @param add.info whether to add additional descriptive information to the output. 
#' @param list whether to output a list object, or a single data frame. 
#' @param sigcode the significance code to be used. Has to be a three-component vector. 
#' @return Either a list (for display purposes) or a data.frame (for csv output purposes). If list return (which is
#' the default) is selected, then the list will contain 4 components: $estimates the estimate; $N number of 
#' observations, $llf value of the log-likelihood function; and $baseline the name of the baseline choice. 
#' 
#' @details This module provides summary methods for three fmlogit objects: \code{fmlogit}, \code{fmlogit.margins}
#' , and \code{fmlogit.wtp}. 
#' 
#' The summary method offers several options to the users. The user can choose for a list output \code{list=T}, which is
#'  good for display and quoting purposes, or a data frame output \code{list=F}, which is good for table outputs. The user
#' can also specify whether to provide additional information other than the parameter estimates, whether to use 
#' seperate lines for the estimates and the standard errors (which mimics the output style in Stata),
#'  as well as the significance code. 
#' 
#' @examples 
#' # generate fmlogit summary
#' #results1 = fmlogit(y,X)
#' 
#' # generate marginal effects summary
#' #effects1 = effects(results1,effect="marginal")
#' summary(effects1)
#' 
#' # generate latex style output
#' # require(xtable)
#' xtable(summary(effects1,list=F,sepline=T))
#' @rdname summary.fmlogit
#' @export summary.fmlogit

############
# generate fmlogit style table
###########

summary.fmlogit = function(object,varlist=NULL,sepline=F,digits=3,add.info=T,list=T,sigcode=c(0.05,0.01,0.001),
                           print=F){
  # define significance code first. 
  asterisk = function(x,k=sigcode){
    if(x>k[1]) return("")
    if(x>k[2]) return("*")
    if(x>k[3]){return("**")}else
    {return("***")}
  }
  # main text  
  # pre matters
  if(!class(object)=="fmlogit") stop("Expect an fmlogit object. Wrong object type given.")
  ynames = names(object[[1]]); Xnames = rownames(object[[1]][[1]])
  if(length(varlist)==0){varlist=Xnames}
  var_colNo = which(Xnames %in% varlist)
  j = object$count[3]; K = length(var_colNo)
  if(K < length(varlist)) warning("Some variables requested are not in the variable list. Those variables are omitted.")
  varlist = Xnames[var_colNo]
  # generating tables
  if(!sepline){
    store_mat = matrix(ncol=j-1,nrow=K)
    colnames(store_mat)=ynames
    rownames(store_mat)=Xnames[var_colNo]
    for(i in 1:(j-1)){
      temp_data = signif(object$estimates[[i]][var_colNo,],digits=digits)
      if(is.null(dim(temp_data))){
        store_mat[,i] = paste(temp_data[1],"(",temp_data[2],")",asterisk(temp_data[4]),sep="")
        next
      }
      store_mat[,i]=apply(temp_data, 1, function(x) paste(x[1],"(",x[2],")",asterisk(x[4]),sep=""))    
    }}else{
      store_beta = store_se = matrix(ncol=j-1,nrow=K)   
      colnames(store_beta)=ynames
      rownames(store_beta)=varlist
      for(i in 1:(j-1)){
        temp_data = signif(object$estimates[[i]][var_colNo,],digits=digits)
        if(is.null(dim(temp_data))) temp_data = as.matrix(temp_data)
        store_beta[,i]=apply(temp_data,1, function(x) paste(x[1],asterisk(x[4]),sep=""))
        store_se[,i]=apply(temp_data, 1, function(x) paste("(",x[2],")",sep=""))
      }
      for(i in 1:K){
        if(i==1) store_mat=matrix(ncol=j-1)
        store_mat = rbind(store_mat,store_beta[i,],store_se[i,])
      }
      store_mat=store_mat[-1,]
      rownames(store_mat) = rep(" ",length=nrow(store_mat))
      rownames(store_mat)[seq(1,K*2,2)] = varlist
    }
  # output matters
  sig.print = paste("Significance code: 0", "'***'", sigcode[3], "'**'", sigcode[2], "'*'", sigcode[1], "' ", 1)
  if(add.info){
    nc = paste("N=",object$count[1],sep="")
    llf = paste("log pseudo-likelihood=",round(object$likelihood,digits=2),sep="")
    bl = paste("Baseline choice:", object$baseline)
  }
  if(list){
    outlist = list(estimates=store_mat)
    if(add.info){
      outlist$N = nc
      outlist$llf = llf
      outlist$baseline = bl
      outlist$sigcode = sig.print
    }
    if(print){print(outlist)}
    return(outlist)
  }else{
    if(add.info){
      info = matrix(ncol=j-1,nrow=4)
      info[,1] = c(nc,llf,bl,sig.print)
      store_mat = rbind(store_mat,info)
    }
    if(print){print(store_mat)}
    return(as.data.frame(store_mat))
  }
}

##########
# summary for fmlogit.margins
##########

#' @rdname summary.fmlogit
#' @export summary.fmlogit.margins

summary.fmlogit.margins = function(object,varlist=NULL,sepline=F,digits=3,add.info=T,list=T,sigcode=c(0.05,0.01,0.001),
                                   print=F){
  # define significance code first. 
  asterisk = function(x,k=sigcode){
    if(x>k[1]) return("")
    if(x>k[2]) return("*")
    if(x>k[3]){return("**")}else
    {return("***")}
  }
  # main text  
  if(!class(object)=="fmlogit.margins") stop("Expect an fmlogit.margins object. Wrong object type given.")
  ynames = rownames(object[[1]]); Xnames = colnames(object[[1]])
  if(length(varlist)==0) varlist=Xnames
  var_colNo = which(Xnames %in% varlist)
  j = length(ynames); K = length(var_colNo)
  if(K < length(varlist)) warning("Some variables requested are not in the variable list. Those variables are omitted.")
  varlist = Xnames[var_colNo]
  
  # table process
  if(object$R==0) sepline=FALSE
  if(!sepline){
    store_mat = matrix(ncol=j,nrow=K)
    colnames(store_mat)=ynames
    rownames(store_mat)=Xnames
    if(object$R>0){
      for(i in var_colNo){
        temp_data = signif(object$ztable[[i]],digits=digits)
        store_mat[i,]=apply(temp_data, 1, function(x) paste(x[1],"(",x[2],")",asterisk(x[4]),sep=""))    
      }}else{
        store_mat = signif(t(object$effects),digits=digits)
      }
  }else{
    store_beta = store_se = matrix(ncol=j,nrow=K)   
    colnames(store_beta)=ynames
    rownames(store_beta)=Xnames
    for(i in var_colNo){
      temp_data = signif(object$ztable[[i]],digits=digits)
      store_beta[i,]=apply(temp_data,1, function(x) paste(x[1],asterisk(x[4]),sep=""))
      store_se[i,]=apply(temp_data, 1, function(x) paste("(",x[2],")",sep=""))
    }
    for(i in 1:K){
      if(i==1) store_mat=matrix(ncol=j)
      store_mat = rbind(store_mat,store_beta[i,],store_se[i,])
    }
    store_mat=store_mat[-1,]
    rownames(store_mat) = rep("",length=nrow(store_mat))
    rownames(store_mat)[seq(1,K*2,2)] = varlist
  }
  # output matters
  sig.print = paste("Significance code: 0", "'***'", sigcode[3], "'**'", sigcode[2], "'*'", sigcode[1], "' ", 1)
  if(add.info){
    expl = object$expl
  }
  if(list){
    outlist = list(estimates=store_mat)
    if(add.info){
      outlist$expl = expl
      outlist$sigcode = sig.print
    }
    if(print){print(outlist)}
    return(outlist)
  }else{
    if(add.info){
      info = matrix(ncol=j,nrow=2)
      info[,1] = c(expl,sig.print)
      store_mat = rbind(store_mat,info)
    }
    if(print){print(store_mat)}
    return(as.data.frame(store_mat))
  }
}

############
# generate willingness to pay tables
############

#' @rdname summary.fmlogit
#' @export summary.fmlogit.wtp

summary.fmlogit.wtp = function(object,varlist=NULL,sepline=F,digits=3,sigcode=c(0.05,0.01,0.001),
                               print=F){
  # define significance code first. 
  asterisk = function(x,k=sigcode){
    if(x>k[1]) return("")
    if(x>k[2]) return("*")
    if(x>k[3]){return("**")}else
    {return("***")}
  }
  # main text  
  if(!class(object)=="fmlogit.wtp") stop("Expect an fmlogit.wtp object. Wrong object type given.")
  if(colnames(object$wtp)[1]!="estimate") return(object$wtp) # no need to summary. 
  Xnames = rownames(object$wtp)
  if(length(varlist)==0) varlist=Xnames
  var_colNo = which(Xnames %in% varlist)
  K = length(var_colNo)
  if(K < length(varlist)) warning("Some variables requested are not in the variable list. Those variables are omitted.")
  varlist = Xnames[var_colNo]
  sig.print = paste("Significance code: 0", "'***'", sigcode[3], "'**'", sigcode[2], "'*'", sigcode[1], "' ", 1)
  if(!sepline){
    # table process
    store_mat = apply(signif(object$wtp[var_colNo,],digits=digits), 1, function(x) paste(x[1],"(",x[2],")",asterisk(x[4]),sep=""))
    store_mat = as.data.frame(store_mat)
    colnames(store_mat)=NULL
    # output matters
  }else{          
    store_beta=apply(signif(object$wtp[var_colNo,],digits=digits),1, function(x) paste(x[1],asterisk(x[4]),sep=""))
    store_se=apply(signif(object$wtp[var_colNo,],digits=digits), 1, function(x) paste("(",x[2],")",sep=""))
    for(i in 1:K){
      if(i==1) store_mat=vector()
      store_mat = c(store_mat,store_beta[i],store_se[i])
    }
    names(store_mat) = rep("",length=length(store_mat))
    names(store_mat)[seq(1,K*2,2)] = varlist
  }
  if(print){print(store_mat);print(sig.print)}    
  return(store_mat)
}