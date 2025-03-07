f_df_theta2tc <- function(in_theta) {
  
  tc <- matrix(data = NA, nrow = length(u.journal), ncol = length(u.year))
  for (i1 in 1:length(u.journal)) {
    for (i2 in 1:length(u.year)) {
      id1 <- data$abbrev == u.journal[i1]
      id2 <- data$Year == u.year[i2]
      id  <- which(id1 & id2) 
      if (length(id) > 0) {
        theta <- colMeans(in_theta[id,-pos_rm,drop = FALSE])
        theta <- theta / sum(theta)
        hh <- sum(theta^2)
        hh <- (hh - (1 / length(theta))) / (1 - (1/length(theta)))
        tc[i1,i2] <- hh
      }
    }
  }
  dimnames(tc) <- list(u.journal, u.year)
  #print(!is.na(tc))
  
  startyear <- rep(NA, 32)
  for (i in 1:32) {
    startyear[i] <- min(data$Year[data$abbrev == u.journal[i]])
  }
  names(startyear) <- rownames(tc)
  
  df <- reshape2::melt(tc)
  colnames(df) <- c("journal", "dyear", "tc")
  df$dyear <- df$dyear - startyear[df$journal]
  pos <- apply(!is.na(df), 1, all)
  df <- df[pos,]
  df
}

f_fit <- function(fml, df, type = c("lm", "betareg")) {
  if (type == "lm") {
    fit <- summary(lm(fml, data = df))
    out <- list(est  = coef(fit)[,1],
                vcov = vcov(fit))
  }
  if (type == "betareg") {
    fit <- summary(betareg::betareg(fml, data = df))
    m   <- fit$coefficients$mean[,1]
    d   <- length(m)
    S   <- fit$vcov
    S   <- S[1:d,1:d]
    out <- list(est  = m,
                vcov = S)
  }
  out
}

f_post_list_fit <- function(list_fit, R = NULL, nsims = 100) {
  sims <- lapply(list_fit, 
                 function(x) stm:::rmvnorm(nsims, x$est, x$vcov))
  sims <- do.call(rbind, sims)
  if (!is.null(R)) {
    sims <- sims %*% R
  }
  est  <- colMeans(sims)
  se   <- sqrt(apply(sims, 2, stats::var))
  tval <- est/se
  p    <- 2 * stats::pnorm(abs(tval), lower.tail = FALSE)
  coef <- cbind(est, se, tval, p)
  if (is.null(R)) {
    rownames(coef) <- names(list_fit[[1]]$est)
  }
  colnames(coef) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  coef
}

f_estimateEffect <- function(formula, stmobj, metadata = NULL, 
                              uncertainty = c("Global", "Local", "None"), documents = NULL, nsims = 25, 
                              prior = NULL, type = c("lm", "betareg")) {
  origcall <- match.call()
  thetatype <- match.arg(uncertainty)
  if (thetatype == "None") 
    nsims <- 1
  if (!is.null(documents)) {
    args <- asSTMCorpus(documents, data = metadata)
    documents <- args$documents
    metadata <- args$data
  }
  if (!inherits(formula, "formula")) 
    stop("formula must be a formula object.")
  if (!is.null(metadata) & !is.data.frame(metadata)) 
    metadata <- as.data.frame(metadata)
  termobj <- terms(formula, data = metadata)
  if (attr(termobj, "response") == 1) {
    response <- as.character(formula)[2]
    K <- eval(parse(text = response))
    if (!(posint(K) && max(K) <= stmobj$settings$dim$K)) 
      stop("Topics specified as response in formula must be a set of positive 
           integers equal to or less than the number of topics in the model.")
    formula <- formula(paste(as.character(formula)[c(1, 3)], 
                             collapse = " "))
    termobj <- terms(formula, data = metadata)
  }
  else {
    K <- 1:stmobj$settings$dim$K
  }
  mf <- model.frame(termobj, data = metadata)
  xmat <- model.matrix(termobj, data = metadata)
  varlist <- all.vars(termobj)
  if (!is.null(metadata)) {
    data <- metadata[, varlist, drop = FALSE]
  }
  else {
    templist <- list()
    for (i in 1:length(varlist)) {
      templist[[i]] <- get(varlist[i])
    }
    data <- data.frame(templist)
    names(data) <- varlist
    rm(templist)
  }
  metadata <- data
  rm(data)
  if (!is.null(prior)) {
    if (!is.matrix(prior)) {
      prior <- diag(prior, nrow = ncol(xmat))
    }
    if (ncol(prior) != ncol(xmat)) 
      stop("number of columns in prior does not match columns in design matrix")
    prior.pseudo <- chol(prior)
    xmat <- rbind(xmat, prior.pseudo)
  }
  qx <- qr(xmat)
  if (qx$rank < ncol(xmat)) {
    prior <- diag(1e-05, nrow = ncol(xmat))
    prior.pseudo <- chol(prior)
    xmat <- rbind(xmat, prior.pseudo)
    qx <- qr(xmat)
    warning("Covariate matrix is singular.  See the details of ?estimateEffect() for some 
            common causes.\n             Adding a small prior 1e-5 for numerical stability.")
  }
  storage <- vector(mode = "list", length = length(K))
  for (i in 1:nsims) {
    
    if (thetatype == "None") 
      thetasims <- stmobj$theta
    else {
      thetasims <- thetaPosterior(stmobj, nsims = 1, type = thetatype, 
                                  documents = documents)
      thetasims <- do.call(rbind, thetasims)
    }
    
    for (k in K) {
      cat("sim i: ", i, " topic k :", k, "\n")
      if (type == "lm") {
        lm.mod <- stm:::qr.lm(thetasims[, k], qx)
        storage[[which(k == K)]][[i]] <- stm:::summary.qr.lm(lm.mod)
      }
      if (type == "betareg") {
        betareg.mod <- betareg::betareg.fit(x = xmat, y = thetasims[, k])
        m   <- betareg.mod$coefficients$mean
        d   <- length(m)
        S   <- betareg.mod$vcov
        S   <- S[1:d,1:d]
        storage[[which(k == K)]][[i]] <- list(est = m, vcov = S)
      }
    }
  }
  toreturn <- list(parameters = storage, topics = K, call = origcall, 
                   uncertainty = thetatype, formula = formula, data = metadata, 
                   modelframe = mf, varlist = varlist)
  class(toreturn) <- "estimateEffect"
  return(toreturn)
}

f_sumEstimateEffect <- function(object, nsims = 100) {
  
  sims <- lapply(object, 
                 function(x) stm:::rmvnorm(nsims, x$est, x$vcov))
  sims <- do.call(rbind, sims)
  est  <- colMeans(sims)
  se   <- sqrt(apply(sims, 2, stats::var))
  tval <- est/se
  p    <- 2 * stats::pnorm(abs(tval), lower.tail = FALSE)
  coefficients <- cbind(est, se, tval, p)
  rownames(coefficients) <- names(object[[1]]$est)
  colnames(coefficients) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  colnames(sims) <- names(object[[1]]$est)
  rownames(sims) <- paste0("sim ", 1:nrow(sims))
  out <- list(sims = sims, 
              coefficients = coefficients)
  return(out)
}