cv.grpreg <- function(X, y, group=1:ncol(X), ..., nfolds=10, seed, fold, returnY=FALSE, trace=FALSE) {
  
  # Complete data fit
  fit.args <- list(...)
  fit.args$X <- X
  fit.args$y <- y
  fit.args$group <- group
  fit.args$returnX <- TRUE
  fit <- do.call("grpreg", fit.args)
  
  # Get standardized X, y
  XG <- fit$XG
  X <- XG$X
  y <- fit$y
  m <- attr(fit$y, "m")
  returnX <- list(...)$returnX
  if (is.null(returnX) || !returnX) fit$XG <- NULL
  
  # Set up folds
  if (!missing(seed)) set.seed(seed)
  n <- length(y)
  
  
  if (missing(fold)) {
    if (m > 1) {
      nn <- n/m
      fold_ <- sample(1:nn %% (nfolds))
      fold_[fold_==0] <- nfolds
      fold <- rep(fold_, each=m)
    } else if (fit$family=="binomial") {
      ind1 <- which(y==1)
      ind0 <- which(y==0)
      n1 <- length(ind1)
      n0 <- length(ind0)
      fold1 <- 1:n1 %% nfolds
      fold0 <- (n1 + 1:n0) %% nfolds
      fold1[fold1==0] <- nfolds
      fold0[fold0==0] <- nfolds
      fold <- numeric(n)
      fold[y==1] <- sample(fold1)
      fold[y==0] <- sample(fold0)
    } else {
      fold <- sample(1:n %% nfolds)
      fold[fold==0] <- nfolds
    }
  } else {
    nfolds <- max(fold)
  }
  
  # Do cross-validation
  E <- Y <- matrix(NA, nrow=length(y), ncol=length(fit$lambda))
  if (fit$family=="binomial") PE <- E
  cv.args <- list(...)
  cv.args$lambda <- fit$lambda
  cv.args$group <- XG$g
  cv.args$group.multiplier <- XG$m
  cv.args$warn <- TRUE
  for (i in 1:nfolds) {
    if (trace) cat("Starting CV fold #",i,sep="","\n")
    res <- cvf(i, X, y, fold, cv.args)
    Y[fold==i, 1:res$nl] <- res$yhat
    E[fold==i, 1:res$nl] <- res$loss
    if (fit$family=="binomial") PE[fold==i, 1:res$nl] <- res$pe
  }
  
  # Eliminate saturated lambda values, if any
  ind <- which(apply(is.finite(E), 2, all))
  E <- E[, ind, drop=FALSE]
  Y <- Y[,ind]
  lambda <- fit$lambda[ind]
  
  # Return
  cve <- apply(E, 2, mean)
  cvse <- apply(E, 2, sd) / sqrt(n)
  min <- which.min(cve)
  null.dev <- calcNullDev(X, y, group=XG$g, family=fit$family)
  
  val <- list(cve=cve, cvse=cvse, lambda=lambda, fit=fit, fold=fold, min=min, lambda.min=lambda[min], null.dev=null.dev)
  if (fit$family=="binomial") val$pe <- apply(PE[,ind], 2, mean)
  if (returnY) {
    if (fit$family=="gaussian") val$Y <- Y + attr(y, "mean")
    else val$Y <- Y
  }
  structure(val, class="cv.grpreg")
}
cvf <- function(i, X, y, fold, cv.args) {
  cv.args$X <- X[fold!=i, , drop=FALSE]
  cv.args$y <- y[fold!=i]
  fit.i <- do.call("grpreg", cv.args)
  
  X2 <- X[fold==i, , drop=FALSE]
  y2 <- y[fold==i]
  yhat <- matrix(predict(fit.i, X2, type="response"), length(y2))
  loss <- loss.grpreg(y2, yhat, fit.i$family)
  pe <- if (fit.i$family=="binomial") {(yhat < 0.5) == y2} else NULL
  list(loss=loss, pe=pe, nl=length(fit.i$lambda), yhat=yhat)
}
loss.grpreg <- function(y, yhat, family) {
  n <- length(y)
  if (family=="gaussian") {
    val <- (y-yhat)^2
  } else if (family=="binomial") {
    yhat[yhat < 0.00001] <- 0.00001
    yhat[yhat > 0.99999] <- 0.99999
    if (is.matrix(yhat)) {
      val <- matrix(NA, nrow=nrow(yhat), ncol=ncol(yhat))
      if (sum(y==1)) val[y==1,] <- -2*log(yhat[y==1, , drop=FALSE])
      if (sum(y==0)) val[y==0,] <- -2*log(1-yhat[y==0, , drop=FALSE])
    } else {
      val <- numeric(length(y))
      if (sum(y==1)) val[y==1] <- -2*log(yhat[y==1])
      if (sum(y==0)) val[y==0] <- -2*log(1-yhat[y==0])
    }
  } else if (family=="poisson") {
    yly <- y*log(y)
    yly[y==0] <- 0
    val <- 2*(yly - y + yhat - y*log(yhat))
  }
  val
}
calcNullDev <- function(X, y, group, family) {
  form <- if (any(group==0)) formula(y~X[,group==0]) else formula(y~1)
  ##XX <- if (any(group==0)) X[,group==0] else 1
  fit <- glm(form, family=family)
  mean(loss.grpreg(y, predict(fit, type="response"), family))
}

