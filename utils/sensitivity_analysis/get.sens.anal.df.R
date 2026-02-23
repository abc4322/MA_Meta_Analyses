# Generates sensitivity analysis data frames across four model types:
# 1) "rand.fix": random/fixed-effects meta-analysis with outlier/influential case exclusion
# 2) "mixed": mixed-effects meta-regression with moderator analysis, study weighting variations
# 3) "subgroup": subgroup meta-analysis with different tau^2 specifications (separate/common/fixed)
# 4) "net": network meta-analysis with tolerance thresholds for multi-arm study inconsistencies
# Returns formatted data frame with rounded metrics, decision codes, and optional colored gt() table output
get.sens.anal.df <- function(
  outcome, model, moderator.vec = c("sessions.duration", "sessions.frequency", "programs.duration", "meditation.total", "follow.up.period", "delivery.mode", "meditation.type"),
  save.as = F, saving.path = "", subgroup = F,
  .sens.anal.df = F  # works only for model = "net"
){
  
  if (model == "rand.fix"){
    
    # get results
    restuls.meta.o. <- print.meta.results(
      outcome = outcome, preferred.scale = get.1st.preferred.scale(outcome),
      regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
      return.data = "results.meta"
    )
    
    if (restuls.meta.o.$k > 0){
      
      outliers <- outlier.list[[outcome]]
      
      # get column names of sensitivity analysis data frame
      cnames <- c(
        "Number of included studies", "Number of obeservations", "... in experimental groups", "... in control groups", "SMD [Hedge's g]",
        "SMD CI lower threshold", "SMD CI upper threshold", "SMD PI lower threshold", "SMD PI upper threshold",
        "SMD's standard errror", "SMD's z-value", "p-value (testing SMD differs from zero)", "tau^2", "tau^2 CI lower threshold", "tau^2 CI upper threshold", "tau^2's standard error", "I^2 [%]",
        "I^2 CI lower threshold", "I^2 CI upper threshold", "Q-value", "p-value (testing between-study heterogeneity)"
      )
      
      # get empty sensitivity analysis data frame
      sens.anal.df <- t(data.frame(
        rep(NA, length(cnames)),
        row.names = cnames
      ))
      
      rnames <- c()
      
      # fill sensitivity analysis data frame
      if (outcome == "Stress"){
        for (preferred.scale in c("DASS", "PSS")){
          for (out.inf in c("out.inf.incl", "out.inf.excl")){
            if (length(outliers) == 0 & out.inf == "out.inf.excl"){
              next  # skip if no outliers/influential cases are present
            }
            for (lib in c("meta", "metafor")){
              for (model.used in c("random", "common")){
                # get results object
                res <- print.meta.results(
                  outcome = outcome, preferred.scale = preferred.scale,
                  regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
                  return.data = paste("results.", lib, sep = ""), results.metafor.fixed = ifelse(model.used == "common" & lib == "metafor", T, F),
                  filter.forest..funnel.vec = if(out.inf == "out.inf.incl"){F}else{if(length(outliers) == 0){FALSE}else{-outliers}}
                )

                # get vector of result values
                sens.anal.vec.df <- t(data.frame(
                  get.sens.anal.vec(res, model, model.used, model.metafor.fixed = ifelse(model.used == "common" & lib == "metafor", T, F)),
                  row.names = cnames
                ))

                sens.anal.df <- rbind(sens.anal.df, sens.anal.vec.df)

                # generate codes for rownames
                rname <- paste(
                  ifelse(preferred.scale == "DASS", "1", "0"),
                  ifelse(length(outliers) == 0, "", ifelse(out.inf == "out.inf.incl", "1", "0")),
                  ifelse(lib == "meta", "1", "0"),
                  ifelse(model.used == "random", "1", "0")
                )

                # add rowname to rownames of sens.anal.df
                rnames <- append(rnames, rname)
              }
            }
          }
        }
      } else {
        for (out.inf in c("out.inf.incl", "out.inf.excl")){
          if (length(outliers) == 0 & out.inf == "out.inf.excl"){
            next  # skip if no outliers/influential cases are present
          }
          for (lib in c("meta", "metafor")){
            for (model.used in c("random", "common")){
             
              # get results object
              res <- print.meta.results(
                outcome = outcome, preferred.scale = F,
                regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
                return.data = paste("results.", lib, sep = ""), results.metafor.fixed = ifelse(model.used == "common" & lib == "metafor", T, F),
                filter.forest..funnel.vec = if(out.inf == "out.inf.incl"){F}else{if(length(outliers) == 0){F}else{-outliers}}
              )
              
              # get vector of result values
              sens.anal.vec.df <- t(data.frame(
                get.sens.anal.vec(res, model, model.used, model.metafor.fixed = ifelse(model.used == "common" & lib == "metafor", T, F)),
                row.names = cnames
              ))

              sens.anal.df <- rbind(sens.anal.df, sens.anal.vec.df)

              # generate codes for rownames
              rname <- paste(
                ifelse(length(outliers) == 0, "", ifelse(out.inf == "out.inf.incl", "1", "0")),
                ifelse(lib == "meta", "1", "0"),
                ifelse(model.used == "random", "1", "0")
              )

              # add rowname to rownames of sens.anal.df
              rnames <- append(rnames, rname)
            }
          }
        }
      }
      
      ## delete first NA row and delete rownames 
      sens.anal.df <- sens.anal.df[-1, 1:ncol(sens.anal.df)]
      rownames(sens.anal.df) <- NULL  # delete rownames
      
      ## add rownames column
      choices.df <- data.frame(
        rnames,
        row.names = NULL
      )
      
      colnames(choices.df) <- "Decision Codes"
      
      sens.anal.df <- cbind(choices.df, sens.anal.df)
      
      ## round values
      ### n
      i.start <- 2
      i.end <- 5
      sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 0)
      
      ### SMD
      i.start <- i.end + 1
      i.end <- i.start + 6
      sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 4)
      
      ### tau2
      i.start <- i.end + 1
      i.end <- i.start + 4
      sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 4)
      
      ### I2 and Q val
      i.start <- i.end + 1
      i.end <- i.start + 3
      sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 2)
      
      ### pval (Q stats)
      sens.anal.df[, i.end + 1] <- round(sens.anal.df[, i.end + 1], 4)
      
      # save colored table
      if (save.as != F){
        ## cut out not wanted columns for coloring
        colrange <- 2:ncol(sens.anal.df)

        for (col in colrange){  # cut cols for coloring that are NA only
          if (all_na(sens.anal.df[, col])){
            colrange <- colrange[!colrange == col]
          }
        }
      
        gt.object <- sens.anal.df |>
          gt() |>
          data_color(
            columns = colrange,
            palette = "RdYlGn",
            na_color = "gray85",
            alpha = .8
          ) |>
          tab_footnote(
            footnote = paste(
              "Meaning of digits in decision codes (same digit order): ",
              ifelse(outcome == "Stress", "preferred Scale - DASS = 1, PSS = 0; ", ""),
              ifelse(length(outliers) == 0, "", "outliers and influential cases included - yes = 1, no = 0; "),
              "library - meta = 1, metafor = 0; model - random-effects = 1, fixed-effects = 0",
              sep = ""
            ),
            cells_column_labels(columns = `Decision Codes`)
          ) |>
          tab_footnote(
            footnote = "SMD = standardized mean difference",
            cells_column_labels(columns = `SMD [Hedge's g]`:`p-value (testing SMD differs from zero)`)
          ) |>
          tab_footnote(
            footnote = "CI = 95% confidence interval",
            cells_column_labels(columns = c(
              `SMD CI lower threshold`, `SMD CI upper threshold`, `tau^2 CI lower threshold`, `tau^2 CI upper threshold`,
              `I^2 CI lower threshold`, `I^2 CI upper threshold`
            ))
          ) |>
          tab_footnote(
            footnote = "PI = 95% prediction interval",
            cells_column_labels(columns = c(`SMD PI lower threshold`, `SMD PI upper threshold`))
          )
        
        if (save.as == "png"){
          gtsave(
            gt.object,
            paste(
              saving.path, "Sens.anal.table.", outcome, ".",
              save.as, sep = ""
            ),
            vwidth = 1750,
            vheight = round(nrow(sens.anal.df) / 16 * 2000, 0)
          )
        } else {
          gtsave(
            gt.object,
            paste(
              saving.path, "Sens.anal.table.", outcome, ".",
              save.as, sep = ""
            )
          )
        }
      }
      
    } else {
      sens.anal.df <- NA
    }
    
# Regression
  } else if (model == "mixed"){
    for (moderator in moderator.vec){
      
      # create sensitivity analysis data frame
      if (moderator %in% c("delivery.mode", "meditation.type")){  # in this case cnames without columns of second moderator coefficient

        res <- print.meta.results(
          outcome, preferred.scale = ifelse(outcome == "Stress", "DASS", F),
          basic = F, moderator.vec = c(moderator), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
          regression.label = T, return.data = "regression.results.linear"
        )

        lvl.names <- unique(res$data[, moderator])
        
        mod.res.names <- c()
        k.w.names <- c()
        for (lvl.name in lvl.names){
          # generate column names for moderator results of lvl.name
          col.sub.names <- c("coefficient", "coefficient CI lower threshold", "coefficient CI upper threshold", "coefficient's standard errror", "coefficient's z-value", "coefficient's p-value")
          i <- 1
          for (col.sub.name in col.sub.names){
            col.sub.names[i] <- paste(lvl.name, col.sub.name)
            i <- i + 1
          }
          mod.res.names <- append(mod.res.names, col.sub.names)
          k.w.names <- append(k.w.names, paste("Number of studies in subroup", lvl.name))
        }
        
        cnames <- c(
          "Number of included studies", k.w.names, "Number of obeservations", "... in experimental groups", "... in control groups",
          mod.res.names,
          "R^2 [%]", "tau^2", "tau^2's standard error",
          "I^2 [%]", "Q-value (testing residual heterogeneity)", "p-value (testing residual heterogeneity)", "Q-value for omnibus-test of moderators", "p-value for omnibus-test of moderators"        
        )
      } else {
        cnames <- c(
          "Number of included studies", "Number of obeservations", "... in experimental groups", "... in control groups",
          "Intercept", "Intercept CI lower threshold", "Intercept CI upper threshold", "Intercept's standard errror", "Intercept's z-value", "Intercept's p-value",
          "Moderator's coefficient", "Moderator's coefficient CI lower threshold", "Moderator's coefficient CI upper threshold",
          "Moderator's coefficient's standard errror", "Moderator's coefficient's z-value", "Moderator's coefficient's p-value",
          "2nd Moderator's coefficient", "2nd Moderator's coefficient CI lower threshold", "2nd Moderator's coefficient CI upper threshold",
          "2nd Moderator's coefficient's standard errror", "2nd Moderator's coefficient's z-value", "2nd Moderator's coefficient's p-value",
          "R^2 [%]", "tau^2", "tau^2's standard error",
          "I^2 [%]", "Q-value (testing residual heterogeneity)", "p-value (testing residual heterogeneity)", "Q-value for omnibus-test of moderators", "p-value for omnibus-test of moderators"        
        )
      }
      
      sens.anal.df <- t(data.frame(
        rep(NA, length(cnames)),
        row.names = cnames
      ))
      
      rnames <- c()
      
      i <- 1
      
      if (outcome == "Stress"){
        for (preferred.scale in c("DASS", "PSS")){
          for (out.inf in c("out.inf.incl", "out.inf.excl")){
            if (length(outliers) == 0 & out.inf == "out.inf.excl"){
              next  # skip if no outliers/influential cases are present
            }
            for (sub.model in c("random", "common")){
              for (mean.range in c("mean.range.incl", "mean.range.excl")){
                if (moderator %in% c("delivery.mode", "meditation.type") & mean.range == "mean.range.excl"){
                   next  # categorical moderators are not influenced by mean range values
                }
                for (degree in c("linear", "squared")){
                  if (moderator %in% c("delivery.mode", "meditation.type") & degree == "squared"){
                    next  # squared models shell not be calculated for categorical moderators
                  }

                  if (moderator %in% c("delivery.mode", "meditation.type")){
                    outliers <- outlier.list[[moderator]][[outcome]]
                  } else{
                    outliers <- outlier.list[[paste(moderator, ifelse(degree == "linear", ".lin", ".sq"), sep = "")]][[outcome]]
                  }
                  
                  res <- print.meta.results(
                    outcome, preferred.scale = preferred.scale,
                    basic = F, moderator.vec = c(moderator), print.regplot = F, print.baujat.regression = F, print.regression.results = F,
                    regression.degree.1 = ifelse(degree == "linear", T, F), regression.degree.2 = ifelse(degree == "squared", T, F),
                    regression.label = T, return.data = ifelse(degree == "linear", "regression.results.linear", "regression.results.poly"),
                    without.mean.r = ifelse(mean.range == "mean.range.incl", F, T), results.metafor.fixed = ifelse(sub.model == "random", F, T),
                    filter.regression.linear.list = ifelse(out.inf == "out.inf.incl" & degree == "linear", F, if(length(outliers) == 0){F}else{list(-outliers)}),
                    filter.regression.poly.list = ifelse(out.inf == "out.inf.incl" & degree == "squared", F, if(length(outliers) == 0){F}else{list(-outliers)})
                  )
                  
                  sens.anal.vec.df <- t(data.frame(
                    get.sens.anal.vec(
                      res, "mixed",
                      sub.model.metafor.fixed = ifelse(sub.model == "random", F, T)
                    ),
                    row.names = cnames
                  ))

                  sens.anal.df <- rbind(sens.anal.df, sens.anal.vec.df)

                  # generate codes for rownames
                  rname <- paste(
                    ifelse(preferred.scale == "DASS", "1", "0"),
                    ifelse(length(outliers) == 0, "", ifelse(out.inf == "out.inf.incl", "1", "0")),
                    ifelse(sub.model == "random", "1", "0"),
                    ifelse(moderator %in% c("delivery.mode", "meditation.type"), "", ifelse(mean.range == "mean.range.incl", "1", "0")),
                    if(moderator %in% c("delivery.mode", "meditation.type")){""}else if(degree == "squared"){"0"}else if(degree == "linear"){"1"}
                  )

                # add rowname to rownames of sens.anal.df
                rnames[i] <- rname
                i <- i + 1
                }
              }
            }
          }
        }
      } else {
        for (out.inf in c("out.inf.incl", "out.inf.excl")){
          if (length(outliers) == 0 & out.inf == "out.inf.excl"){
            next  # skip if no outliers/influential cases are present
          }
          for (sub.model in c("random", "common")){
            for (mean.range in c("mean.range.incl", "mean.range.excl")){
              for (degree in c("linear", "squared")){
                if (moderator %in% c("delivery.mode", "meditation.type") & degree == "squared"){
                  next  # squared models shell not be calculated for categorical moderators
                }
                
                if (moderator %in% c("delivery.mode", "meditation.type")){
                  outliers <- outlier.list[[moderator]][[outcome]]
                } else{
                  outliers <- outlier.list[[paste(moderator, ifelse(degree == "linear", ".lin", ".sq"), sep = "")]][[outcome]]
                }

                res <- print.meta.results(
                  outcome,
                  basic = F, moderator.vec = c(moderator), print.regplot = F, print.baujat.regression = F, print.regression.results = F,
                  regression.degree.1 = ifelse(degree == "linear", T, F), regression.degree.2 = ifelse(degree == "squared", T, F),
                  regression.label = T, return.data = ifelse(degree == "linear", "regression.results.linear", "regression.results.poly"),
                  without.mean.r = ifelse(mean.range == "mean.range.incl", F, T), results.metafor.fixed = ifelse(sub.model == "random", F, T),
                  filter.regression.linear.list = ifelse(out.inf == "out.inf.incl", F, if(length(outliers) == 0){F}else{list(-outliers)}),
                  filter.regression.poly.list = ifelse(out.inf == "out.inf.incl" & degree == "squared", F, if(length(outliers) == 0){F}else{list(-outliers)})
                )

                sens.anal.vec.df <- t(data.frame(
                  get.sens.anal.vec(res, "mixed"),
                  row.names = cnames
                ))

                sens.anal.df <- rbind(sens.anal.df, sens.anal.vec.df)

                # generate codes for rownames
                rname <- paste(
                  ifelse(length(outliers) == 0, "", ifelse(out.inf == "out.inf.incl", "1", "0")),
                  ifelse(sub.model == "random", "1", "0"),
                  ifelse(mean.range == "mean.range.incl", "1", "0"),
                  if(moderator %in% c("delivery.mode", "meditation.type")){""}else if(degree == "squared"){"0"}else if(degree == "linear"){"1"}
                )

                # add rowname to rownames of sens.anal.df
                rnames[i] <- rname
                i <- i + 1
              }
            }
          }
        }
      }
      
      ## delete first NA row and delete rownames 
      sens.anal.df <- sens.anal.df[-1, 1:ncol(sens.anal.df)]
      rownames(sens.anal.df) <- NULL  # delete rownames
      
      ## add rownames column
      choices.df <- data.frame(
        rnames,
        row.names = NULL
      )
      
      colnames(choices.df) <- "Decision Codes"
      
      sens.anal.df <- cbind(choices.df, sens.anal.df)
      
      ## round values
      ### k and n
      lo.lim <- 2
      up.lim <- ifelse(moderator %in% c("delivery.mode", "meditation.type"), 5 + length(res$b), 5)
      sens.anal.df[, lo.lim:up.lim] <- round(sens.anal.df[, lo.lim:up.lim], 0)
      
      ### coefficients
      for (i in 1:length(res$b)){
        lo.lim <- up.lim + 1
        up.lim <- lo.lim + 5
        sens.anal.df[, lo.lim:up.lim] <- round(sens.anal.df[, lo.lim:up.lim], 4)
      }
      
      ### R^2
      R.index <- which(colnames(sens.anal.df) == "R^2 [%]")
      sens.anal.df[, R.index] <- round(sens.anal.df[, R.index], 2)
      
      ### tau^2 and its se
      lo.lim <- R.index + 1
      up.lim <- lo.lim + 1
      sens.anal.df[, lo.lim:up.lim] <- round(sens.anal.df[, lo.lim:up.lim], 4)
      
      ### I^2 and Q-value
      lo.lim <- up.lim + 1
      up.lim <- lo.lim + 1
      sens.anal.df[, lo.lim:up.lim] <- round(sens.anal.df[, lo.lim:up.lim], 2)
      
      ### p-values of Q-statistics and omnibus test
      lo.lim <- up.lim + 1
      up.lim <- lo.lim + 2
      sens.anal.df[, lo.lim:up.lim] <- round(sens.anal.df[, lo.lim:up.lim], 4)
      
      # save colored table
      if (save.as != F){
        ## cut out not wanted columns for coloring
        colrange <- 2:ncol(sens.anal.df)

        for (col in 1:ncol(sens.anal.df)){  # cut cols for coloring that are NA only
          if (all_na(sens.anal.df[, col])){
            colrange <- colrange[!colrange == col]
          }
        }
        
        # get column names of CI
        cnames.CI <- cnames[grepl("CI", cnames)]
        
        gt.object <- sens.anal.df |>
          gt() |>
          data_color(
            columns = colrange,
            palette = "RdYlGn",
            na_color = "gray85",
            alpha = .8
          ) |>
          tab_footnote(
            footnote = if(outcome == "Stress"){
              paste(
                "Meaning of digits in decision codes (same digit order): ",
                "preferred Scale - DASS = 1, PSS = 0; ",
                ifelse(length(outliers) == 0, "", "outliers and influential cases included - yes = 1, no = 0; "),
                "'sub-model' used in mixed-effects model - random-effects = 1, fixed-effects = 0",
                ifelse(moderator %in% c("delivery.mode", "meditation.type"), "", "; mean range values - included = 1, excluded = 0; degree of model - linear = 1, squared = 0"),
                sep = ""
              )
            } else if (outcome == "Anxiety"){
              paste(
                "Meaning of digits in decision codes (same digit order): ",
                ifelse(length(outliers) == 0, "", "outliers and influential cases included - yes = 1, no = 0; "),
                "'sub-model' used in mixed-effects model - random-effects = 1, fixed-effects = 0",
                ifelse(moderator %in% c("delivery.mode", "meditation.type"), "", "; mean range values - included = 1, excluded = 0; degree of model - linear = 1, squared = 0"),
                sep = ""
              )
            } else {""},
            cells_column_labels(columns = "Decision Codes")
          ) |>
          tab_footnote(
            footnote = "CI = confidence interval",
            cells_column_labels(columns = cnames.CI)
          )
        
        if (save.as == "png"){
          gtsave(
            gt.object,
            paste(
              saving.path, "Sens.anal.table.", outcome, ".",
              moderator, ".reg.", save.as, sep = ""
            ),
            vwidth = round(ncol(sens.anal.df) / 31 * 3300, 0),
            vheight = round(sqrt(nrow(sens.anal.df) / 32) * 2000, 0)
          )
        } else {
          gtsave(
            gt.object,
            paste(
              saving.path, "Sens.anal.table.", outcome, ".",
              moderator, ".reg.", save.as, sep = ""
            )
          )
        }
      }
    }
# Subgroup Analysis
  } else if (model == "subgroup"){
    
    # get results
    restuls.meta.o. <- print.meta.results(
      outcome = outcome, preferred.scale = ifelse(outcome == "Stress", "DASS", F),
      regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F,
      split.subgroups = T, print.forest.sub.single = subgroup, subgroup.method = "fixed", print.meta.results = F,
      return.data = "results.meta"
    )
    
    if (restuls.meta.o.$k > 0){
      
      if (subgroup != F){
        outliers <- outlier.list[[subgroup]][[outcome]]
        if (!subgroup %in% c("delivery.mode", "meditation.type")){
          print("error in get.sens.anal.df(): set parameter'subgroup' to 'delivery.mode' or 'meditation.type'")
        }
      } else {
        print("error in get.sens.anal.df(): set parameter'subgroup' to 'delivery.mode' while parameter 'model' == 'subgroup'")
      }
      
      # generate column names for study numbers, treatment effects, and heterogeneity metrics of all subgroup levels
      cnames.k.w <- c()
      cnames.TE.w <- c()
      cnames.TE.w.base <- c(
        "SMD [Hedge's g]", "SMD CI lower threshold", "SMD CI upper threshold", "SMD's standard errror", "SMD's z-value"
      )
      cnames.het.w <- c()
      cnames.het.w.base <- c(
        "tau^2", "tau^2 CI lower threshold", "tau^2 CI upper threshold",
        "I^2 [%]", "I^2 CI lower threshold", "I^2 CI upper threshold", "Q-value", "p-value (testing between-study heterogeneity)"
      )
      
      for (sub.lvl in restuls.meta.o.$subgroup.levels){
        
        # study numbers
        cnames.k.w <- append(cnames.k.w, paste("Study number in", sub.lvl))
        
        # treatment effect metrics       
        cnames.TE.w.lvl <- c()
        for (i in 1:length(cnames.TE.w.base)){
          cnames.TE.w.lvl[i] <- paste(cnames.TE.w.base[i], " (", sub.lvl, ")", sep = "")
        }
        cnames.TE.w <- append(cnames.TE.w, cnames.TE.w.lvl)
        
        # heterogeneity metrics
        cnames.het.w.lvl <- c()
        for (i in 1:length(cnames.het.w.base)){
          cnames.het.w.lvl[i] <- paste(cnames.het.w.base[i], " (", sub.lvl, ")", sep = "")
        }
        cnames.het.w <- append(cnames.het.w, cnames.het.w.lvl)
      }
      
      # column names of sensitivity analysis data frame
      cnames <- c(
        "Number of included studies", cnames.k.w, "Number of obeservations", "... in experimental groups", "... in control groups",
        cnames.TE.w, cnames.het.w, "Q-value (between group)", "p-value (between group)"
      )
      
      # get empty sensitivity analysis data frame
      sens.anal.df <- t(data.frame(
        rep(NA, length(cnames)),
        row.names = cnames
      ))
      
      rnames <- c()
      
      # fill sensitivity analysis data frame
      if (outcome == "Stress"){
        for (preferred.scale in c("DASS", "PSS")){
          for (out.inf in c("out.inf.incl", "out.inf.excl")){
            if (length(outliers) == 0 & out.inf == "out.inf.excl"){
              next  # skip if no outliers/influential cases are present
            }
            for (subgroup.method in c("random.separate.tau2", "random.common.tau2", "fixed")){
              # get results object
              res <- print.meta.results(
                outcome = outcome, preferred.scale = preferred.scale,
                regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F,
                split.subgroups = T, print.forest.sub.single = subgroup, subgroup.method = subgroup.method, print.meta.results = F,
                return.data = "results.meta",
                filter.forest..funnel.vec = if(out.inf == "out.inf.incl"){F}else{if(length(outliers) == 0){FALSE}else{-outliers}}
              )
              sens.anal.vec.df <- t(data.frame(
                get.sens.anal.vec(res, model, subgroup.method = subgroup.method),
                row.names = cnames
              ))

              sens.anal.df <- rbind(sens.anal.df, sens.anal.vec.df)

              # generate codes for rownames
              rname <- paste(
                ifelse(preferred.scale == "DASS", "1", "0"),
                ifelse(length(outliers) == 0, "", ifelse(out.inf == "out.inf.incl", "1", "0")),
                if(subgroup.method == "random.separate.tau2"){"1"}else if(subgroup.method == "random.common.tau2"){"2"}else if(subgroup.method ==  "fixed"){"0"}
              )

              # add rowname to rownames of sens.anal.df
              rnames <- append(rnames, rname)
            }
          }
        }
      } else {
        for (out.inf in c("out.inf.incl", "out.inf.excl")){
          if (length(outliers) == 0 & out.inf == "out.inf.excl"){
            next  # skip if no outliers/influential cases are present
          }
          for (subgroup.method in c("random.separate.tau2", "random.common.tau2", "fixed")){

            # get results object
            res <- print.meta.results(
              outcome = outcome, preferred.scale = F,
              regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F,
              split.subgroups = T, print.forest.sub.single = subgroup, subgroup.method = subgroup.method, print.meta.results = F,
              return.data = "results.meta",
              filter.forest..funnel.vec = if(out.inf == "out.inf.incl"){F}else{if(length(outliers) == 0){FALSE}else{-outliers}}
            )

            # get vector of result values
            sens.anal.vec.df <- t(data.frame(
              get.sens.anal.vec(res, model, subgroup.method = subgroup.method),
              row.names = cnames
            ))

            sens.anal.df <- rbind(sens.anal.df, sens.anal.vec.df)

            # generate codes for rownames
            rname <- paste(
              ifelse(length(outliers) == 0, "", ifelse(out.inf == "out.inf.incl", "1", "0")),
              if(subgroup.method == "random.separate.tau2"){"1"}else if(subgroup.method == "random.common.tau2"){"2"}else if(subgroup.method ==  "fixed"){"0"}
            )

            # add rowname to rownames of sens.anal.df
            rnames <- append(rnames, rname)
          }
        }
      }
      
      ## delete first NA row and delete rownames 
      sens.anal.df <- sens.anal.df[-1, 1:ncol(sens.anal.df)]
      rownames(sens.anal.df) <- NULL  # delete rownames
      
      ## add rownames column
      choices.df <- data.frame(
        rnames,
        row.names = NULL
      )
      
      colnames(choices.df) <- "Decision Codes"
      
      sens.anal.df <- cbind(choices.df, sens.anal.df)
      
      ## round values
      lvl.n <- length(restuls.meta.o.$subgroup.levels)
      
      # n        
      i.start <- 2
      i.end <- 5 + lvl.n
      sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 0)
      
      # TE vals
      for (i in 1:lvl.n){
        i.start <- i.end + 1
        i.end <- i.start + 4
        sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 4)
      }
      
      # heterogeneity vals
      for (i in 1:lvl.n){
        # tau2 vals
        i.start <- i.end + 1
        i.end <- i.start + 2
        sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 4)
        
        # I2 vals
        i.start <- i.end + 1
        i.end <- i.start + 2
        sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 2)
        
        # Q stats between study
        i.start <- i.end + 1
        i.end <- i.start + 1
        sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 4)
      }
      
      # Q stats between groups
      i.start <- i.end + 1
      i.end <- i.start + 1
      sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 4)
      
      # save colored table
      if (save.as != F){
        ## cut out not wanted columns for coloring
        colrange <- 2:ncol(sens.anal.df)

        for (col in colrange){  # cut cols for coloring that are NA only
          if (all_na(sens.anal.df[, col])){
            colrange <- colrange[!colrange == col]
          }
        }
        
        
        # get column names of SMD, CI, and PI
        cnames.SMD <- cnames[grepl("SMD", cnames)]
        cnames.CI <- cnames[grepl("CI", cnames)]
        
        gt.object <- sens.anal.df |>
          gt() |>
          data_color(
            columns = colrange,
            palette = "RdYlGn",
            na_color = "gray85",
            alpha = .8
          ) |>
          tab_footnote(
            footnote = paste(
              "Meaning of digits in decision codes (same digit order): ",
              ifelse(outcome == "Stress", "preferred Scale - DASS = 1, PSS = 0; ", ""),
              ifelse(length(outliers) == 0, "", "outliers and influential cases included - yes = 1, no = 0; "),
              "subgroup method - random-effects with separate tau^2 = 1, random-effects with common tau^2 = 2, fixed = 0",
              sep = ""
            ),
            cells_column_labels(columns = "Decision Codes")
          ) |>
          tab_footnote(
            footnote = "SMD = standardized mean difference",
            cells_column_labels(columns = cnames.SMD)
          ) |>
          tab_footnote(
            footnote = "CI = confidence interval",
            cells_column_labels(columns = cnames.CI)
          )
        
        if (save.as == "png"){
          gtsave(
            gt.object,
            paste(
              saving.path, "Sens.anal.table.", outcome, ".", subgroup, ".sub.",
              save.as, sep = ""
            ),
            vwidth = round(ncol(sens.anal.df) / 35 * 4000, 0),
            vheight = round(nrow(sens.anal.df) / 12 * 1000, 0)
          )
        } else {
          gtsave(
            gt.object,
            paste(
              saving.path, "Sens.anal.table.", outcome, ".", subgroup, ".sub.",
              save.as, sep = ""
            )
          )
        }
      }
      
    } else {
      sens.anal.df <- NA
    }
    
# network meta-analysis
  } else if (model == "net"){
    
    if (is.logical(.sens.anal.df)){
      outliers <- outlier.list[["net.overall"]]

      # get column names of sensitivity analysis data frame
      cnames <- c(
        "Number of included studies", "... pairwise comparisons", "... treatments", "... designs",
        "p-val. pas. vs. bio.", "p-val. pas. vs. cog.", "p-val. pas. vs. dog", "p-val. pas. vs. med(x)", "p-val. pas. vs. med(m)", "p-val. pas. vs. PMR",
        "p-val. pas. vs. rest", "p-val. pas. vs. stress", "p-val. pas. vs. walk",
        "p-val. med(x) vs. bio.", "p-val. med(x) vs. cog.", "p-val. med(x) vs. dog", "p-val. med(x) vs. med(m)", "p-val. med(x) vs. PMR",
        "p-val. med(x) vs. rest", "p-val. med(x) vs. stress", "p-val. med(x) vs. walk",
        "SMD diff. med(x) vs. bio.", "SMD diff. med(x) vs. cog.", "SMD diff. med(x) vs. dog", "SMD diff. med(x) vs. med(m)", "SMD diff. med(x) vs. pas.", "SMD diff. med(x) vs. PMR",
        "SMD diff. med(x) vs. rest", "SMD diff. med(x) vs. stress", "SMD diff. med(x) vs. walk",
        "tau^2", "I^2 [%]", "I^2 CI lower threshold [%]", "I^2 CI upper threshold [%]",
        "Total Q", "Total Q df", "Total Q p-value",
        "Q of for heterogeneity (het.)", "Q (het.) df", "Q (het.) p-value", 
        "Q of for inconsistancy (inc.)", "Q (inc.) df", "Q (inc.) p-value"
      )

      rnames <- c()

      # get empty sensitivity analysis data frame
      sens.anal.df <- t(data.frame(
        rep(NA, length(cnames)),
        row.names = cnames
      ))

      for (preferred.scale in c("DASS", "PSS")){
        for (out.inf in c("out.inf.incl", "out.inf.excl")){
          for (tol.multiarm in c(1, 0.01, 0.001)){

            res <- net.meta.analyze(
              outcome, preferred.scale = preferred.scale, net.df = F, net.res = F, comparisons.skip.list = F,
              plot.netgraph = F, plot.forest = F, plot.direct.evidence = F, plot.netheat = F,
              reference.group = "passive control", random = T, return.data = "net.res", silent = T, tol.multiarm = tol.multiarm,
              filter.forest..funnel.vec = if (out.inf == "out.inf.incl"){
                if(tol.multiarm == 0.001){inc.mult.arm.stud.001}else if(tol.multiarm == 0.01){inc.mult.arm.stud.01}else{F}
              } else {
                if(length(outliers) == 0){
                  if(tol.multiarm == 0.001){inc.mult.arm.stud.001}else if(tol.multiarm == 0.01){inc.mult.arm.stud.01}else{F}
                } else {
                  if(tol.multiarm == 0.001){c(inc.mult.arm.stud.001, outliers)}else if(tol.multiarm == 0.01){c(inc.mult.arm.stud.01, outliers)}else{outliers}
                }
              }
            )

            # get vector of result values
            sens.anal.vec.df <- t(data.frame(
              get.sens.anal.vec(res),
              row.names = cnames
            ))

            sens.anal.df <- rbind(sens.anal.df, sens.anal.vec.df)

            # generate codes for rownames
            rname <- paste(
              ifelse(preferred.scale == "DASS", "1", "0"),
              ifelse(length(outliers) == 0, "", ifelse(out.inf == "out.inf.incl", "1", "0")),
              if(tol.multiarm == 1){"1"}else if(tol.multiarm == 0.01){"2"}else{"0"}
            )

            # add rowname to rownames of sens.anal.df
            rnames <- append(rnames, rname)
          }
        }
      }

      ## delete first NA row and delete rownames 
      sens.anal.df <- sens.anal.df[-1, 1:ncol(sens.anal.df)]
      rownames(sens.anal.df) <- NULL  # delete rownames

      ## add rownames column
      choices.df <- data.frame(
        rnames,
        row.names = NULL
      )

      colnames(choices.df) <- "Decision Codes"

      sens.anal.df <- cbind(choices.df, sens.anal.df)
    } else {
      if (is.data.frame(sens.anal.df) != T){
        cat("error in get.sens.anal.df(): set paramter sens.anal.df to an data frame, got object of type", class(sens.anal.df), "instead")
      }
      sens.anal.df <- .sens.anal.df
    }

    ## round values
    ### k, m, n, d
    i.start <- 2
    i.end <- 5
    sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 0)

    ### pvals
    i.start <- i.end + 1
    i.end <- i.start + 16
    sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 4)

    ### SMD differences
    i.start <- i.end + 1
    i.end <- i.start + 8
    sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 2)

    ### tau2
    i.start <- i.end + 1
    i.end <- i.start + 0
    sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 4)
    
    ### I2
    i.start <- i.end + 1
    i.end <- i.start + 2
    sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 2)
    
    ### Q and its pvals
    for (metric in c("Total Q", "Q heterogeneity", "Q inconsistancy")){
      # Q
      i.start <- i.end + 1
      i.end <- i.start + 0
      sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 2)
    
      # df
      i.start <- i.end + 1
      i.end <- i.start + 0
      sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 2)
    
      # p
      i.start <- i.end + 1
      i.end <- i.start + 0
      sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 4)
    }


    # save colored table
    if (save.as != F){
      ## cut out not wanted columns for coloring
      colrange <- 2:ncol(sens.anal.df)

      for (col in colrange){  # cut cols for coloring that are NA only
        if (all_na(sens.anal.df[, col])){
          colrange <- colrange[!colrange == col]
        }
      }
      
      gt.object <- sens.anal.df |>
        gt() |>
        data_color(
          columns = colrange,
          palette = "RdYlGn",
          na_color = "gray85",
          alpha = .8
        ) |>
        tab_footnote(
          footnote = paste(
            "Meaning of digits in decision codes (same digit order): ",
            "preferred Scale - DASS = 1, PSS = 0; ",
            ifelse(length(outliers) == 0, "", "outliers, influential cases, and inconsistant treatment comparisons included - yes = 1, no = 0; "),
            "tolerance of multi-arm study inconsistency - 1 (no inconsitant multi-arm studies cut) = 1, .01 (some respective studies cut) = 2, .001 (all respective studies cut) = 0",
            sep = ""
          ),
          cells_column_labels(columns = `Decision Codes`)
        ) |>
        tab_footnote(
          footnote = "bio. = biofeedback",
          cells_column_labels(columns = c(
            `p-val. pas. vs. bio.`, `p-val. med(x) vs. bio.`, `SMD diff. med(x) vs. bio.`
          ))
        ) |>
        tab_footnote(
          footnote = "pas. = passive control",
          cells_column_labels(columns = c(
            `p-val. pas. vs. cog.`:`p-val. pas. vs. walk`, `SMD diff. med(x) vs. pas.`
          ))
        ) |>
        tab_footnote(
          footnote = "cog. = cognitive control",
          cells_column_labels(columns = c(
            `p-val. pas. vs. cog.`, `p-val. med(x) vs. cog.`, `SMD diff. med(x) vs. cog.`
          ))
        ) |>
        tab_footnote(
          footnote = "dog = dog threapy",
          cells_column_labels(columns = c(
            `p-val. pas. vs. dog`, `p-val. med(x) vs. dog`, `SMD diff. med(x) vs. dog`
          ))
        ) |>
        tab_footnote(
          footnote = "med(x) = exclusive meditation",
          cells_column_labels(columns = c(
            `p-val. pas. vs. med(x)`, `p-val. med(x) vs. cog.`:`SMD diff. med(x) vs. walk`
          ))
        ) |>
        tab_footnote(
          footnote = "med(m) = meditation with movment",
          cells_column_labels(columns = c(
            `p-val. pas. vs. med(m)`, `p-val. med(x) vs. med(m)`, `SMD diff. med(x) vs. med(m)`
          ))
        ) |>
        tab_footnote(
          footnote = "PMR = Progressive Muscle Relaxation (by Jacobson)",
          cells_column_labels(columns = c(
            `p-val. pas. vs. PMR`, `p-val. med(x) vs. PMR`, `SMD diff. med(x) vs. PMR`
          ))
        ) |>
        tab_footnote(
          footnote = "stress = stress management",
          cells_column_labels(columns = c(
            `p-val. pas. vs. stress`, `p-val. med(x) vs. stress`, `SMD diff. med(x) vs. stress`
          ))
        ) |>
        tab_footnote(
          footnote = "SMD diff. = differnce of standardized mean differences",
          cells_column_labels(columns = `SMD diff. med(x) vs. cog.`:`SMD diff. med(x) vs. walk`)
        )
      
        gtsave(
          gt.object,
          paste(
            saving.path, "Sens.anal.table.network.all", ".",
            save.as, sep = ""
          )
        )      
    }
  } else {
    print("error in get.sens.anal.df(): set parameter 'model' to 'rand.fix', 'mixed', 'subgroup', or 'net'")
  }
  return(sens.anal.df)
}
