### Get vector to fill sensitivity analysis df
# with network analysis included
# This function extracts and consolidates sensitivity analysis results from meta-analysis objects
# (metafor, meta, or netmeta classes) into a single vector. It handles both fixed/random effects
# models and subgroup/moderator analyses, extracting study counts, treatment effects, confidence
# intervals, p-values, and heterogeneity metrics. The output vector is formatted for populating
# sensitivity analysis dataframes and accommodates different analysis types and methodologies.
get.sens.anal.vec <- function(res, model = F, model.meta = NULL, subgroup.method = NULL, sub.model.metafor.fixed = F, model.metafor.fixed = F){ 
  
  if ("rma.uni" %in%  class(res)){  # for {metafor} results
    n.cont <- sum(res$data[, "n.control"])
    n.int <- sum(res$data[, "n.int"])
    n.total <- n.cont + n.int
    pval.Q <- res$QEp
    # pval.Q <- ifelse(pval.Q == 0, "< .0001" ,pval.Q)
    pval <- res$pval
    # pval <- ifelse(pval == 0, "< .0001" ,pval)
    
    if (model == "rand.fix"){
      
      if (model.metafor.fixed){
        het.metrics <- c(rep(NA, 9)[1:9])
        pred.inter.lb <- NA
        pred.inter.ub <- NA
      } else {
        het.metrics <- c(res$tau2, NA, NA, res$se.tau2, res$I2, NA, NA, res$QE, pval.Q)
        pred.inter.lb <- predict(res)$pi.lb
        pred.inter.ub <- predict(res)$pi.ub
      }

      sens.anal <- c(
        res$k, n.total, n.int, n.cont, res$b[1,1], res$ci.lb, res$ci.ub,
        pred.inter.lb, pred.inter.ub, res$se, res$zval, pval, het.metrics
      )
    } else if (model == "mixed"){
      Q.val.omnibus.test.moderators <- res$QM
      p.val.omnibus.test.moderators <- res$QMp
      is.categorical.mod <- grepl("delivery.mode", names(res$b[2,1])) | grepl("meditation.type", names(res$b[2,1]))
        # is true if moderator is delivery.mode or meditation.type

      # add number of studies per subgroup to results object if moderator is categorical
      if (is.categorical.mod){
        res$k.w <- c(0)
        for (lvl in 2:length(res$b)){  # length(res$b) = number of model coefficients
          res$k.w[lvl] <- sum(res$X[,lvl])
        }
        res$k.w[1] <- res$k - sum(res$k.w)
      } else {
        res$k.w <- NULL
      }
      
      # generate vector with model results
      mod.vals <- c()
      for (lvl in 1:length(res$b)){
        mod.vals <- append(
          mod.vals,
          as.double(c(
            res$b[lvl,1], res$ci.lb[lvl], res$ci.ub[lvl], res$se[lvl], res$zval[lvl], pval[lvl],
            if(!is.categorical.mod & length(res$b) == 2 & lvl == 2){rep(NA, 6)[1:6]}else{NULL}  # generate NAs for 2nd moderator coefficient of linear regressions of continous moderators
          ))
        )
      }
      
      # get heterogeneity metrics
      if (sub.model.metafor.fixed){
        het.metrics <- c(rep(NA, 5)[1:5])
      } else {
        het.metrics <- c(res$tau2, res$se.tau2, res$I2, res$QE, pval.Q)
      }
        
      sens.anal <- c(
        res$k, res$k.w, n.total, n.int, n.cont,
        mod.vals, res$R2, het.metrics, Q.val.omnibus.test.moderators, p.val.omnibus.test.moderators
      )
      
    } else {
      print("error in get.sens.anal.vec(): set parameter 'model' to 'rand.fix' or 'mixed' when using {metafor} results")
    }
    
  } else if ("metacont" %in%  class(res) | "metagen" %in%  class(res)){  # for {meta} resutls
    
    n.cont <- sum(res$n.c)
    n.int <- sum(res$n.e)
    n.total <- n.cont + n.int
    pval <- res$pval.random
    # pval <- ifelse(pval == 0, "< .0001" ,pval)
    # pval.Q <- ifelse(pval.Q == 0, "< .0001" ,pval.Q)
    pred.inter.lb <- res$lower.predict
    pred.inter.ub <- res$upper.predict
    
    
    
    if (model == "rand.fix"){
      if (model.meta == "random"){
        sens.anal <- c(
          res$k, n.total, n.int, n.cont, res$TE.random, res$lower.random, res$upper.random, pred.inter.lb, pred.inter.ub,
          res$seTE.random, res$zval.random, pval,
          res$tau2, res$lower.tau2, res$upper.tau2, NA,
          res$I2 * 100, res$lower.I2 * 100, res$upper.I2 * 100,
          res$Q, res$pval.Q
        )
      } else if (model.meta == "common"){
        sens.anal <- c(
          res$k, n.total, n.int, n.cont, res$TE.common, res$lower.common, res$upper.common, pred.inter.lb, pred.inter.ub,
          res$seTE.common, res$zval.common, pval,
          NA, NA, NA, NA,
          NA, NA, NA,
          NA, NA
        )
      } else {
        print("error in get.sens.anal.vec(): set parameter 'model.meta' to 'common' or 'random' if {meta} results are passed")
      }
    } else if (model == "mixed"){
      print("error in get.sens.anal.vec(): there is no regression calculated by {meta} on its own (only a wrapper function working with {metafor}). Set parameter 'model' to 'rand.fix' or 'subgroup' instead.")
    } else if (model == "subgroup"){
      
      # get number of studies per subgroup, treatment effect, and within group/between study heterogeneity metrics from result object (res)
      k.w <- c()
      TE.metrics <- c()
      het.metrics.w <- c()
      for (i in 1:length(res$k.w)){  # iterate over number of subgroup levls
        k.w[i] <- res$k.w[i]

        if (subgroup.method == "fixed"){
          TE.metrics <- append(TE.metrics, c(res$TE.common.w[i], res$lower.common.w[i], res$upper.common.w[i], res$seTE.common.w[i], res$zval.common.w[i]))
          het.metrics.w <- append(het.metrics.w, c(rep(NA, 8)[1:8]))
        } else {
          TE.metrics <- append(TE.metrics, c(res$TE.random.w[i], res$lower.random.w[i], res$upper.random.w[i], res$seTE.random.w[i], res$zval.random.w[i]))

          if ((subgroup.method == "random.common.tau2" & i == 1) | subgroup.method %in% c("random.separate.tau2", "random")){
            Q.stats.within <- if(subgroup.method == "random.common.tau2"){c(res$Q, res$pval.Q)}else if(subgroup.method %in% c("random.separate.tau2", "random")){c(res$Q.w[i], res$pval.Q.w[i])}
            tau2.metrics.within <- if(subgroup.method %in% c("random.common.tau2", "random")){c(res$tau2.w[i], NA, NA)}else if(subgroup.method == "random.separate.tau2"){c(res$tau2.w[i], res$lower.tau2.w[i], res$upper.tau2.w[i])}
             # tau2 confidence intervals of subgroups seem to be not reported  by {meta} result object for subgroup.method = "random.common.tau2"
            het.metrics.w <- append(
              het.metrics.w,
              c(
                tau2.metrics.within,
                res$I2.w[i] * 100, res$lower.I2.w[i] * 100, res$upper.I2.w[i] * 100,
                Q.stats.within
              )
            )
          } else {
            het.metrics.w <- append(het.metrics.w, c(rep(NA, 8)[1:8]))
          }
        }
      }
      
      # get between group Q-statistics
      if (subgroup.method == "fixed"){
        Q.stats.between <- c(res$Q.b.common, res$pval.Q.b.common)
      } else if (subgroup.method == "random.separate.tau2"){
        Q.stats.between <- c(res$Q, res$pval.Q)
      } else if (subgroup.method %in% c("random.common.tau2", "random")){
        Q.stats.between <- c(res$Q.b.random, res$pval.Q.b.random)
      }
      sens.anal <- as.double(c(
        res$k, k.w, n.total, n.int, n.cont,
        TE.metrics, het.metrics.w, Q.stats.between
      ))

    } else {
      print("error in get.sens.anal.vec(): set parameter 'model' to 'rand.fix' or 'subgroup' when using {meta} results")
    }
  } else if (class(res) == "netmeta"){  # for network analysis
    
    trts <- net.res.all$trts
    
    k <- length(unique(gsub("\\ #.*","", res$studlab)))  # gsub("\\ #.*","", studlab) cuts out every " #" and the following
    
    sens.anal <- c(
      k, res$m, res$n, res$d,
      as.double(res$pval.random[, "passive control"][trts][-6]),  # pvals comparing interventions with passive control
      as.double(res$pval.random[, "meditation (exclusive)"][trts][-c(4, 6)]),  # pvals comparing interventions with exclusive meditation
      as.double(res$TE.random[, "meditation (exclusive)"][trts][-4]) * -1,  # differences in SMD to exclusive meditation
        # index 4 = passive control, index 6 = meditation (exclusive)
      res$tau2, res$I2 * 100, res$lower.I2 * 100, res$upper.I2 * 100,
      res$Q, res$df.Q, res$pval.Q,
      res$Q.heterogeneity, res$df.Q.heterogeneity, res$pval.Q.heterogeneity,
      res$Q.inconsistency, res$df.Q.inconsistency, res$pval.Q.inconsistency
    )
  } else {
    cat("error in get.sens.anal.vec(): unknown input object of class:", class(res))
  }
  return(sens.anal)
}