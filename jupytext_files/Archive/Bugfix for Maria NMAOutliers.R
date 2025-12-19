# -*- coding: utf-8 -*-
# ---
# jupyter:
#   jupytext:
#     formats: ../Archive//ipynb,../jupytext_files/Archive//R:percent
#     text_representation:
#       extension: .R
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.18.1
#   kernelspec:
#     display_name: R
#     language: R
#     name: ir
# ---

# %%
df <- read.csv("data for bugfix in NMAoutlier.measures.csv")
df

# %%
library(netmeta)
res <- netmeta(
  TE = TE,
  seTE = seTE,
  treat1 = treat1,
  treat2 = treat2,
  studlab = studlab,
  data = df,
  sm = "SMD",
  fixed = F,
  random = T,
  reference.group = "passive control",
  sep.trts = " vs. ",
  tol.multiarm = 1  # lower tolerance mentioned in sensitivity analyses
)

# %%
# reproduction of error
library(NMAoutlier)
outl.res <- NMAoutlier.measures(
  TE = TE,
  seTE = seTE,
  treat1 = treat1,
  treat2 = treat2,
  studlab = studlab,
  data = df,
  sm = "SMD",
  reference = "passive control",
  measure = "deletion",
  tol.multiarm = 1
)

# %%
# bugfix

# load internal functions of the MASS and NMAoutlier packages
chkchar <- NMAoutlier:::chkchar
createB <- NMAoutlier:::createB
res_multi <- NMAoutlier:::res_multi
ginv <- MASS:::ginv

# fixed function
NMAoutlier.measures.single <- function(  # just a bugfix for the original function (deletes every single entry of data when measure = "deletion")
  TE, seTE, treat1, treat2, studlab, data = NULL, sm,
  reference = "", measure = "simple", tol.multiarm = 1
){

  ## Check arguments
  ##
  chkchar(reference)
  
  ## Read data
  ##
  ##
  nulldata <- is.null(data)
  ##
  if (nulldata)
    data <- sys.frame(sys.parent())
  ##
  mf <- match.call()
  ##
  ## Catch TE, treat1, treat2, seTE, studlab from data:
  ##
  TE <- eval(mf[[match("TE", names(mf))]],
             data, enclos = sys.frame(sys.parent()))
  ##
  if (inherits(TE, "pairwise") ||
      is.data.frame(TE) & !is.null(attr(TE, "pairwise"))) {
    sm <- attr(TE, "sm")
    ##
    seTE <- TE$seTE
    treat1 <- TE$treat1
    treat2 <- TE$treat2
    studlab <- TE$studlab
    ##
    if (!is.null(TE$n1))
      n1 <- TE$n1
    if (!is.null(TE$n2))
      n2 <- TE$n2
    if (!is.null(TE$event1))
      event1 <- TE$event1
    if (!is.null(TE$event2))
      event2 <- TE$event2
    ##
    is.pairwise <- TRUE
    pairdata <- TE
    data <- TE
    ##
    TE <- TE$TE
  } else {
    is.pairwise <- FALSE
    if (missing(sm))
      if (!is.null(data) && !is.null(attr(data, "sm")))
        sm <- attr(data, "sm")
      else
        sm <- ""
      ##
      seTE <- eval(mf[[match("seTE", names(mf))]],
                   data, enclos = sys.frame(sys.parent()))
      ##
      treat1 <- eval(mf[[match("treat1", names(mf))]],
                     data, enclos = sys.frame(sys.parent()))
      ##
      treat2 <- eval(mf[[match("treat2", names(mf))]],
                     data, enclos = sys.frame(sys.parent()))
      ##
      studlab <- eval(mf[[match("studlab", names(mf))]],
                      data, enclos = sys.frame(sys.parent()))
      ##
      n1 <- eval(mf[[match("n1", names(mf))]],
                 data, enclos = sys.frame(sys.parent()))
      ##
      n2 <- eval(mf[[match("n2", names(mf))]],
                 data, enclos = sys.frame(sys.parent()))
      ##
      event1 <- eval(mf[[match("event1", names(mf))]],
                     data, enclos = sys.frame(sys.parent()))
      ##
      event2 <- eval(mf[[match("event2", names(mf))]],
                     data, enclos = sys.frame(sys.parent()))
  }
  ##
  if (is.factor(treat1))
    treat1 <- as.character(treat1)
  ##
  if (is.factor(treat2))
    treat2 <- as.character(treat2)
  ##
  if (!is.numeric(studlab))
    studlab <- as.numeric(as.factor(studlab))



  ## Additional checks
  ##
  ## Check NAs and zero standard errors
  ##
  excl <- is.na(TE) | is.na(seTE) | seTE <= 0
  ##
  if (any(excl)) {
    dat.NAs <- data.frame(studlab = studlab[excl],
                          treat1 = treat1[excl],
                          treat2 = treat2[excl],
                          TE = format(round(TE[excl], 4)),
                          seTE = format(round(seTE[excl], 4))
    )
    warning("Comparison",
            if (sum(excl) > 1) "seTE",
            " with missing TE / seTE or zero seTE not considered in network meta-analysis.",
            call. = FALSE)
    cat(paste("Comparison",
              if (sum(excl) > 1) "s",
              " not considered in network meta-analysis:\n", sep = ""))
    prmatrix(dat.NAs, quote = FALSE, right = TRUE,
             rowlab = rep("", sum(excl)))
    ##
    studlab <- studlab[!(excl)]
    treat1  <- treat1[!(excl)]
    treat2  <- treat2[!(excl)]
    TE      <- TE[!(excl)]
    seTE    <- seTE[!(excl)]
  }
  ## Check for correct number of comparisons (after removing
  ## comparisons with missing data)
  ##
  is.wholenumber <-
    function(x, tol = .Machine$double.eps ^ 0.5)
      abs(x - round(x)) < tol
  ##
  tabnarms <- table(studlab)
  sel.narms <- !is.wholenumber((1 + sqrt(8 * tabnarms + 1)) / 2)
  ##
  if (sum(sel.narms) == 1)
    stop(paste("After removing comparisons with missing treatment effects",
               " or standard errors,\n  study '",
               names(tabnarms)[sel.narms],
               "' has a wrong number of comparisons.",
               " Please check data and\n  consider to remove study",
               " from network meta-analysis.",
               sep = ""))
  if (sum(sel.narms) > 1)
    stop(paste("After removing comparisons with missing treatment effects",
               " or standard errors,\n  the following studies have",
               " a wrong number of comparisons: ",
               paste(paste("'", names(tabnarms)[sel.narms], "'", sep = ""),
                     collapse = ", "),
               "\n  Please check data and consider to remove studies",
               " from network meta-analysis.",
               sep = ""))
  ##
  ## Check number of subgraphs
  ##
  n.subnets <- netconnection(treat1, treat2, studlab)$n.subnets
  ##
  if (n.subnets > 1)
    stop(paste("After removing comparisons with missing treatment effects",
               " or standard errors,\n  network consists of ",
               n.subnets, " separate sub-networks.\n  ",
               "Please check data and consider to remove studies",
               " from network meta-analysis.",
               sep = ""))
  ##
  ## Check for correct treatment order within comparison
  ##
  wo <- treat1 > treat2
  ##
  if (any(wo)) {
    warning("Note, treatments within a comparison have been re-sorted in increasing order.",
            call. = FALSE)
    TE[wo] <- -TE[wo]
    ttreat1 <- treat1
    treat1[wo] <- treat2[wo]
    treat2[wo] <- ttreat1[wo]
  }


  # names of treatments
  names.treat <- sort(unique(c(treat1, treat2)))

  ## if no option exist, set reference treatment as the first in
  ## alphabetic / numeric order
  ##
  if (reference == "")
    reference <- names.treat[1]


  ## Conduct network meta-analysis (NMA) with random effects model,
  ## Rücker model
  model <- netmeta(
    TE, seTE, treat1, treat2, studlab,
    comb.random = TRUE, reference.group = reference,
    tol.multiarm = tol.multiarm
  )


  ## Model objects
  ##
  ## number of treatments
  ##
  nt <- model$n


  ## treatment positions
  tr1 <- model$treat1.pos
  tr2 <- model$treat2.pos

  ## effect
  y.m <- model$TE



  ##
  dat <- noquote(cbind(TE, seTE, studlab, treat1, treat2))
  ##
  rownames(dat) <- c(1:length(TE))
  ##
  ## predicted estimate
  y.m.est <- model$TE.nma.random


  if (measure == "simple") {


    ## Outlier and influence diagnostics measures
    ##
    ## Raw residuals for each pairwise comparison
    ##
    rawres <- y.m - y.m.est

    ## Raw residuals for each study
    ##
    eraw <- res_multi(studlab, rawres)$res


    ##
    ## Standardized residuals for each pairwise comparison
    ##
    standres <- sqrt(model$w.random) * rawres


    ## Standardized residuals for each study
    ##
    estand <- res_multi(studlab, standres)$res

    ##
    ## Studentized residuals for each pairwise comparison
    ##
    if (!is.null(model$H.matrix.random))
      H.matrix <- model$H.matrix.random
    else
      H.matrix <- model$H.matrix
    studres <- 1/sqrt(1 - diag(H.matrix)) * sqrt(model$w.random) * rawres


    ## Studentized residuals for each study
    ##
    estud <- res_multi(studlab, studres)$res


    ## Mahalanobis distance for each pairwise comparison
    ##
    Mah <- model$Q.fixed

    ## Qi contribution
    Q.pooled <- model$w.fixed * (model$TE - model$TE.nma.fixed)^2
    Q.random <- model$w.random * (model$TE - model$TE.nma.random)^2

    ## Mahalanobis distance for each study
    ##
    Mahalanobis.distance <- res_multi(studlab, Mah)$res

    # leverage for each pairwise comparison
    lev <- as.numeric(diag(H.matrix))

    # leverage for each study
    leverage <- res_multi(studlab, lev)$res


    res <- list(dat = dat,
                eraw = eraw,
                estand = estand,
                estud = estud,
                Mah = Mah,
                Mahalanobis.distance = Mahalanobis.distance,
                lev = lev,
                leverage = leverage, measure = measure)

  } else if (measure == "deletion") {

    s.m <-  model$seTE
    
    
    ## B is the design matrix, the edge-vertex incidence matrix (mxn)
    ##
    B <- createB(tr1, tr2, nt)
    ##
    t <- (model$tau)^2                   # heterogeneity
    ##
    b <- model$TE.random[, reference]    # summary estimate of treatment effects


    ## Computations for variance-covariance matrix (random effects model)
    ##
    ind <- which(names.treat == reference) # index of reference treatment
    t.pos1 <- rep(ind, (nt - 1))
    t.pos2 <- setdiff(1:nt, ind)
    ##
    B.r <- createB(t.pos1, t.pos2, ncol = nt) # Restricted matrix B
    # Laplacian matrix

    L.random <- t(B) %*% diag(model$w.random) %*% B
    Lplus.random <- solve(L.random - 1 / nt) + 1 / nt
    

    # Variance-covariance matrix (random effects model)
    Cov <- B.r %*% Lplus.random %*% t(B.r)


    ## Q statistics
    ##
    Q.standard <- model$Q                                       # overall
    Qh.standard <- model$Q.heterogeneity                        # within-designs
    Qi.standard <- decomp.design(model)$Q.inc.random$Q          # between-designs


    ## Outlier and influence diagnostics measures considered deletion


    studies <- unique(studlab)


    heterog.leaveoneout <- w.leaveoneout <- H.leaveoneout <- eraw.deleted <- estand.deleted <- estud.deleted <- Cooks.distance <- Covratio <- Rstat.heterogeneity <- RQtotal <- RQhet <- RQinc <- list()
    DFbetas <- Rstat.estimates <- NULL
    
    
    for (i in 1:length(studies)) {
      # deleted study
      deleted <- studies[i]
      remaining <- studies[studies != deleted]
      remaining.ind <- which(studlab %in% remaining)
      netmeta.res <- netmeta(TE, seTE, treat1, treat2, studlab, comb.random = TRUE,
                             reference.group = reference, subset = remaining.ind, tol.multiarm = tol.multiarm)


      estimate <- netmeta.res$TE.random[,reference]                # summary estimates
      
      # calculate .new variables in which the treatments are cut out that are lost by the deletion of the ith study
      if (length(estimate) != nt){  #############################################################################
        names.treat.new <- names(estimate)
        treats.left <- names.treat %in% names.treat.new  # T for all treatments but deleted trements with F
        ind.trt.del <- which(F == treats.left)  # indices of deleted treatments
        ind.trt.del.ref <- which(F == treats.left[-ind])  # indices of deleted treatments without reference treatment 
        nt.new <- length(names.treat) - (length(names.treat) - length(names.treat.new))
        B.new <- B[,treats.left]
        b.new <- b[treats.left]
        ind.new <- which(names.treat[-ind.trt.del] == reference)  # index of reference treatment
        t.pos1.new <- rep(ind.new, (nt.new - 1))
        t.pos2.new <- setdiff(1:nt.new, ind.new)
        B.r.new <- createB(t.pos1.new, t.pos2.new, ncol = nt.new)
        L.random.new <- t(B.new) %*% diag(model$w.random) %*% B.new
        Lplus.random.new <- solve(L.random.new - 1 / nt.new) + 1 / nt.new
        Cov.new <- B.r.new %*% Lplus.random.new %*% t(B.r.new)
      }

      heterog <- (netmeta.res$tau)^2                               # heterogeneity
      heterog.leaveoneout[[i]] <- heterog


      ## Q statistics
      Qt <- netmeta.res$Q                                          # overall
      Qhe <- netmeta.res$Q.heterogeneity                           # within-designs
      Qin <- decomp.design(netmeta.res)$Q.inc.random$Q             # between-designs

      # index of study deleted
      ind.deleted <- which(studlab == deleted)

      # weight
      w.leave <- 1/(s.m[ind.deleted]^2 + heterog)

      ## Standardized study deleted residuals
      w.leaveoneout[[i]] <- res_multi(studlab[ind.deleted], w.leave)$res
      
      # hat values
      #
      if (length(estimate) == nt){  #############################################################################
        Bi <- B[ind.deleted,]
        Bi.matrix <- matrix(Bi, ncol = nt)
      } else {
        Bi <- B.new[ind.deleted,]  #############################################################################
        Bi.matrix <- matrix(Bi, ncol = nt.new)  #############################################################################
      }
      

      # leverage "leave-one-out"
      wi.matrix <- diag(w.leave,  nrow = length(w.leave), ncol = length(w.leave))
      hii <- diag(Bi.matrix %*% ginv(t(Bi.matrix) %*% wi.matrix %*% Bi.matrix) %*% t(Bi.matrix) %*% wi.matrix)

      H.leaveoneout[[i]] <- res_multi(studlab[ind.deleted], hii)$res

      n <- netmeta.res$n                                           # number of treatments (= nt.new)

      t1 <- netmeta.res$treat1.pos                                 # treatment positions
      t2 <- netmeta.res$treat2.pos                                 # treatment positions

      ## B is the design matrix, the edge-vertex incidence matrix (mxn)
      Brem <- createB(t1, t2, n)

      ## Laplacian matrix
      ##
      L.r <- t(Brem) %*% diag(netmeta.res$w.random) %*% Brem
      Lplus <- solve(L.r - 1 / n) + 1 / n



      ## Computations for variance-covariance matrix (random effects model)
      ##
      if (length(estimate) == nt){  #############################################################################
        ind <- which(names.treat == reference)    # index of reference treatment
      } else {
        ind <- which(names.treat[-ind.trt.del] == reference)  #############################################################################
      }
      t.pos1 <- rep(ind, (n - 1))
      t.pos2 <- setdiff(1:n, ind)
      ##
      Br.remove <- createB(t.pos1, t.pos2, ncol = n)  # Restricted matrix B

      # Variance-covariance matrix (random effects model)
      Cov.remove <- Br.remove %*% Lplus %*% t(Br.remove)

      
      ## Raw pairwise deleted residuals
      if (length(estimate) == nt){  #############################################################################
        rawres <- c(y.m[ind.deleted] - B[ind.deleted,] %*% estimate)
      } else {
        rawres <- c(y.m[ind.deleted] - B.new[ind.deleted,] %*% estimate)  #############################################################################
      }

      ## Raw study deleted residuals
      eraw.deleted[[i]] <- res_multi(studlab[ind.deleted], rawres)$res


      ## Standardized pairwise deleted residuals
      standres <- sqrt(w.leave) * rawres

      ## Standardized study deleted residuals
      estand.deleted[[i]] <- res_multi(studlab[ind.deleted], standres)$res


      ## Studentized pairwise deleted residuals
      studres <- 1/sqrt(s.m[ind.deleted]^2 + t + hii *  w.leave) * rawres

      ## Studentized study deleted residuals
      estud.deleted[[i]] <- res_multi(studlab[ind.deleted], studres)$res


      ## Cook's statistic considered deletion
      if (length(estimate) == nt){  #############################################################################
        Cooks.distance[[i]] <- c(t(b[-ind] - estimate[-ind]) %*% ginv(Cov) %*% (b[-ind] - estimate[-ind]))  #############################################################################
      } else {
        Cooks.distance[[i]] <- c(t(b.new[-ind] - estimate[-ind]) %*% ginv(Cov.new) %*% (b.new[-ind] - estimate[-ind]))  #############################################################################
      }

      ## Covratio considered deletion

      ## Ratio of the determinants of the variance-covariance matrix
      if (length(estimate) == nt){  #############################################################################
        Covratio[[i]] <- det(Cov.remove) / det(Cov)
      } else {
        Covratio[[i]] <- det(Cov.remove) / det(Cov.new)  #############################################################################
      }

      ## R statistic for heterogeneity
      Rstat.heterogeneity[[i]] <- ((t - heterog) / t) * 100

      ## R statistic for summary estimates
      if (length(estimate) == nt){  #############################################################################
        Rstat.estimate <- ((b[-ind] - estimate[-ind]) / b[-ind]) * 100  #############################################################################
      } else {
        Rstat.estimate <- ((b.new[-ind] - estimate[-ind]) / b.new[-ind]) * 100  #############################################################################
        
        # insert NAs where deleted treatments should have been
        for (j in ind.trt.del.ref){  #############################################################################
          Rstat.estimate <- append(Rstat.estimate, NA, after = j - 1)  #############################################################################
          names(Rstat.estimate)[j] <- names.treat[-which(names.treat == reference)][j]  #############################################################################
        }
      }
      Rstat.estimates <- cbind(Rstat.estimates, Rstat.estimate)

      ## R statistic total
      RQtotal[[i]] <- ((Q.standard - Qt) / Q.standard) * 100

      ## R statistic hererogeneity
      RQhet[[i]] <- ((Qh.standard - Qhe) / Qh.standard) * 100

      ## R statistic inconsistency
      RQinc[[i]] <- ((Qi.standard - Qin) / Qi.standard) * 100

      ## DFbetas
      if (length(estimate) == nt){  #############################################################################
        DFbeta <- (b[-ind] - estimate[-ind]) * sqrt(sum(w.leave)/length(w.leave))
      } else {
        DFbeta <- (b.new[-ind] - estimate[-ind]) * sqrt(sum(w.leave)/length(w.leave))  #############################################################################
        
        # append NA where deleted treatments should have been
        for (j in ind.trt.del.ref){  #############################################################################
          DFbeta <- append(DFbeta, NA, after = j - 1)  #############################################################################
          names(DFbeta)[j] <- names.treat[-which(names.treat == reference)][j]  #############################################################################
        }
      }
      DFbetas <- cbind(DFbetas, DFbeta)
    }


    res <- list(dat = dat,
                eraw.deleted = unlist(eraw.deleted),
                estand.deleted = unlist(estand.deleted),
                estud.deleted = unlist(estud.deleted),
                Cooks.distance = unlist(Cooks.distance),
                Covratio = unlist(Covratio),
                w.leaveoneout = unlist(w.leaveoneout),
                H.leaveoneout = unlist(H.leaveoneout),
                heterog.leaveoneout = unlist(heterog.leaveoneout),
                Rheterogeneity = unlist(Rstat.heterogeneity),
                Restimates = Rstat.estimates,
                RQtotal = unlist(RQtotal),
                RQhet = unlist(RQhet),
                RQinc = unlist(RQinc),
                DFbetas = DFbetas,
                measure = measure)

  }


  class(res) <- "NMAoutlier.measures"

  res


}

# %%
outl.res <- NMAoutlier.measures.single(
  TE = TE,
  seTE = seTE,
  treat1 = treat1,
  treat2 = treat2,
  studlab = studlab,
  data = df,
  sm = "SMD",
  reference = "passive control",
  measure = "deletion"
)
outl.res
