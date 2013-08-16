ME.Lenth <- function(b, simulated=TRUE, alpha=NULL){
    s0 <- 1.5 * median(abs(b))
    cj <- as.numeric(b[abs(b) < 2.5 * s0])
    PSE <- 1.5 * median(abs(cj))
    m <- length(b)
      if (is.null(alpha)) alpha <- seq(0.25,0.01,by=-0.01)
      else{
         if (!is.numeric(alpha)) stop("alpha must be numeric")
         if (min(alpha)<0 || max(alpha)>1) stop("alpha must be between 0 and 1")
      }
      if (simulated){
        if (length(setdiff(alpha, seq(0.01,0.25,0.01)))>0 || m<7 || m>143){
        simulated <- FALSE
        message("simulated critical values not available for all requests, used conservative ones")
        }
      }
    ## cover simulated critical values, where available
    if (simulated){
      ME <- crit.ME[as.character(m),as.character(alpha)]*PSE
      SME <- crit.SME[as.character(m),as.character(alpha)]*PSE
    }
    else{
      gamma <- (1 + (1 - alpha)^(1/m))/2
      ME <- qt(1-alpha/2, m/3)*PSE
      SME <- qt(gamma, m/3)*PSE
      names(ME) <- names(SME) <- alpha
    }
    list(s0=s0, PSE=PSE, ME=ME, SME=SME)
  }

CME.LW98 <- function(b, sterr, dfe, simulated=TRUE, alpha=NULL){
  ## sterr = sqrt(K*MSE), obtainable from linear model analysis
  ## dfe = residual df
  s0 <- 1.5 * median(abs(b))
  cj <- as.numeric(b[abs(b) < 2.5 * s0])
  m <- length(b)
  d <- m/3
  wPSE <- d / (d + dfe)
  CPSE <- sqrt((1.5 * median(abs(cj)))^2*wPSE + (1-wPSE)*sterr^2)
      if (is.null(alpha)) alpha <- seq(0.25,0.01,by=-0.01)
      else{
         if (!is.numeric(alpha)) stop("alpha must be numeric")
         if (min(alpha)<0 || max(alpha)>1) stop("alpha must be between 0 and 1")
      }
      if (simulated){
        if (length(setdiff(alpha, seq(0.01,0.25,0.01)))>0 || m<7 || m>143){
        simulated <- FALSE
        message("simulated critical values not available for all requests, used conservative ones")
        }
      }
  if (simulated){
    CME <- crit.ME[as.character(m),as.character(alpha)]*CPSE
    CSME <- crit.SME[as.character(m),as.character(alpha)]*CPSE
  }
  else{
    gamma <- (1 + (1 - alpha)^(1/m))/2
    CME <- qt(1-alpha/2, d)*CPSE
    CSME <- qt(gamma, m/3)*CPSE
    names(CME) <- names(CSME) <- alpha
  }
  list(s0=s0, CPSE=CPSE, CME=CME, CSME=CSME)
}

CME.EM08 <- function(b, sterr, dfe, simulated=TRUE, weight0=5, alpha=NULL){
  ## sterr = sqrt(K*MSE), obtainable from linear model analysis
  ## dfe = residual df
  ## weight0 <- multiplier for dfe in weighting sterr^2 for s0
  m <- length(b)
  d <- length(b)/3
  ws0 <- d / (d + weight0 * dfe)
  wPSE <- d / (d + dfe)
  s0 <- 1.5 * median(abs(b))
  s0 <- sqrt(ws0*s0^2 + (1-ws0)*sterr^2)
  cj <- as.numeric(b[abs(b) < 2.5 * s0])
  CPSE <- sqrt((1.5 * median(abs(cj)))^2*wPSE + (1-wPSE)*sterr^2)
      if (is.null(alpha)) alpha <- seq(0.25,0.01,by=-0.01)
      else{
         if (!is.numeric(alpha)) stop("alpha must be numeric")
         if (min(alpha)<0 || max(alpha)>1) stop("alpha must be between 0 and 1")
      }
      if (simulated){
        if (length(setdiff(alpha, seq(0.01,0.25,0.01)))>0 || m<7 || m>143){
        simulated <- FALSE
        message("simulated critical values not available for all requests, used conservative ones")
        }
      }
  if (simulated){
    CME <- crit.ME[as.character(m),as.character(alpha)]*CPSE
    CSME <- crit.SME[as.character(m),as.character(alpha)]*CPSE
  }
  else{
    gamma <- (1 + (1 - alpha)^(1/m))/2
    CME <- qt(1-alpha/2, d)*CPSE
    CSME <- qt(gamma, m/3)*CPSE
    names(CME) <- names(CSME) <- alpha
  }
  list(Cs0=s0, CPSE=CPSE, CME=CME, CSME=CSME)
}
