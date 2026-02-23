# This script includes utility functions for meta-analysis, e.g., effect size calculation, influence diagnostics, and regression data extraction.
# Some of these functions are used as helper functions in meta.analyze() of main_function_1_forest_funnel_return_regression.R

### Function | Return 1st preferred scale for outcomes that have multiple scales per outcome present
get.1st.preferred.scale <- function(outcome){
  if (outcome == "Stress"){
    return("DASS")
  } else if (outcome == "Positive affect"){
    return("IPANAT")
  } else if (outcome == "Self-compassion"){
    return("SCS")
  } else {
    return(F)
  }
}
### Get regression (moderator) data
get.regression.data <- function(
    m.data.list, moderator, study, intervention.no, nm.placeholder, without.mean.r
){
  moderators <- c(
    "Delivery.Mode", "Sessions.Duration.in.minutes", "Frequency.in.times.per.week",
    "Total.Duration.in.Days", "period.t1.t2", "period.t1.t3", "Meditation.Type",
    "female.percent", "male.percent", "diverse.percent"
  )
  
  if (without.mean.r){
    intervention.comparisons.df.list.str <- "intervention.comparisons.df.list.w.o.mean.r"
  } else {
    intervention.comparisons.df.list.str <- "intervention.comparisons.df.list"
  }
  
  if (!moderator %in% moderators){
    cat("use one of the following moderators in get.regression.data():\n", moderators)
  } else {
    moderator.value <- m.data.list[[intervention.comparisons.df.list.str]][[study]][intervention.no, moderator]
    if (!(
      is.na(moderator.value) |
      moderator.value %in% c("NA", nm.placeholder, as.character(nm.placeholder))
    )){
      regression.data <- moderator.value
    } else {
      regression.data <- NA
    }
    return(regression.data)
  }
}

### Get Funnel and Regression Result
get.results.metafor <- function(
  meta.df, moderator = FALSE, degree = 1, interaction = F, results.metafor.fixed = F  # , categorical.mod.trans = FALSE
){
  results.escalc <- escalc(
    data = meta.df, measure = "SMD",
    m1i = meta.df[,"mean.int"], sd1i = meta.df[,"sd.int"], n1i = meta.df[,"n.int"],
    m2i = meta.df[,"mean.control"], sd2i = meta.df[,"sd.control"], n2i = meta.df[,"n.control"]
  )
  
  if (if(is.logical(moderator)){moderator == F}else{F}){
    results.metafor <- rma.uni(
      yi, vi, measure="SMD", data = results.escalc, method = ifelse(results.metafor.fixed, "FE", "REML")  # "REML" is default method for random-effects model; "FE" fixed-effects model
    )
    
  } else {
    
    if (length(moderator) == 1){
      moderator.string <- moderator
    } else {
      if (interaction){
        moderator.string <- paste(moderator, collapse="*")
      } else {
        moderator.string <- paste(moderator, collapse="+")
      }
    }
    
    if (degree == 1){
      results.metafor <- rma.uni(
        yi, vi, mods = formula(paste("~", moderator.string)), data=results.escalc, method = ifelse(results.metafor.fixed, "FE", "REML")
      )
    } else if (degree == 2) {
      if (length(moderator) == 1){
        results.metafor <- rma.uni(
          yi, vi, mods = ~ poly(results.escalc[, moderator[1]], degree=degree, raw=TRUE), data=results.escalc, method = ifelse(results.metafor.fixed, "FE", "REML")  # formula(paste("~", moderator[1], "+I(", moderator[1], "^2)"))
        )
      } else if (length(moderator) == 2){
        
        if (interaction){
          results.metafor <- rma.uni(
            yi, vi, mods = ~
            poly(results.escalc[, moderator[1]], degree=degree, raw=TRUE) *
            poly(results.escalc[, moderator[2]], degree=degree, raw=TRUE), 
            data=results.escalc, method = ifelse(results.metafor.fixed, "FE", "REML")
          )
        } else {
          results.metafor <- rma.uni(
            yi, vi, mods = ~
            poly(results.escalc[, moderator[1]], degree=degree, raw=TRUE) +
            poly(results.escalc[, moderator[2]], degree=degree, raw=TRUE), 
            data=results.escalc, method = ifelse(results.metafor.fixed, "FE", "REML")
          )
        }
      } else if (length(moderator) == 3){
        if (interaction){
        results.metafor <- rma.uni(
            yi, vi, mods = ~
            poly(results.escalc[, moderator[1]], degree=degree, raw=TRUE) *
            poly(results.escalc[, moderator[2]], degree=degree, raw=TRUE) *
            poly(results.escalc[, moderator[3]], degree=degree, raw=TRUE), 
            data=results.escalc, method = ifelse(results.metafor.fixed, "FE", "REML")
          )
        } else {
          results.metafor <- rma.uni(
            yi, vi, mods = ~
            poly(results.escalc[, moderator[1]], degree=degree, raw=TRUE) +
            poly(results.escalc[, moderator[2]], degree=degree, raw=TRUE) +
            poly(results.escalc[, moderator[3]], degree=degree, raw=TRUE), 
            data=results.escalc, method = ifelse(results.metafor.fixed, "FE", "REML")
          )
        }
      } else if (length(moderator) == 4){
        if (interaction){
          results.metafor <- rma.uni(
            yi, vi, mods = ~
            poly(results.escalc[, moderator[1]], degree=degree, raw=TRUE) *
            poly(results.escalc[, moderator[2]], degree=degree, raw=TRUE) *
            poly(results.escalc[, moderator[3]], degree=degree, raw=TRUE) *
            poly(results.escalc[, moderator[4]], degree=degree, raw=TRUE), 
            data=results.escalc, method = ifelse(results.metafor.fixed, "FE", "REML")
          )
        } else {
          results.metafor <- rma.uni(
            yi, vi, mods = ~
            poly(results.escalc[, moderator[1]], degree=degree, raw=TRUE) +
            poly(results.escalc[, moderator[2]], degree=degree, raw=TRUE) +
            poly(results.escalc[, moderator[3]], degree=degree, raw=TRUE) +
            poly(results.escalc[, moderator[4]], degree=degree, raw=TRUE), 
            data=results.escalc, method = ifelse(results.metafor.fixed, "FE", "REML")
          )
        }
      } else if (length(moderator) == 5){
        if (interaction){
          results.metafor <- rma.uni(
            yi, vi, mods = ~
            poly(results.escalc[, moderator[1]], degree=degree, raw=TRUE) *
            poly(results.escalc[, moderator[2]], degree=degree, raw=TRUE) *
            poly(results.escalc[, moderator[3]], degree=degree, raw=TRUE) *
            poly(results.escalc[, moderator[4]], degree=degree, raw=TRUE) *
            poly(results.escalc[, moderator[5]], degree=degree, raw=TRUE),  
            data=results.escalc, method = ifelse(results.metafor.fixed, "FE", "REML")
          )
        } else {
          results.metafor <- rma.uni(
            yi, vi, mods = ~
            poly(results.escalc[, moderator[1]], degree=degree, raw=TRUE) +
            poly(results.escalc[, moderator[2]], degree=degree, raw=TRUE) +
            poly(results.escalc[, moderator[3]], degree=degree, raw=TRUE) +
            poly(results.escalc[, moderator[4]], degree=degree, raw=TRUE) +
            poly(results.escalc[, moderator[5]], degree=degree, raw=TRUE),  
            data=results.escalc, method = ifelse(results.metafor.fixed, "FE", "REML")
          )
        }
      } else {
          print("get.results.metafor() currently only works for polynomial multiple regressions of 1 to 5 moderator")
      }
    } else {
      print("get.results.metafor() currently only works for degree 1 or 2")
    }
  }
  return(results.metafor)
}
### Fill meta.df for meta.analyze()
fill.meta.df <- function(
  scale, time.point, intervention.no, control.no, outcome.no, study,
  m.data.list, nm.placeholder, descr..reg.data.list, without.mean.r
){
  
  t.with.pre <- time.point + 1
  
  # Study ID
  descr..reg.data.list[[time.point]][["study.id"]] <- append(
    descr..reg.data.list[[time.point]][["study.id"]],
    m.data.list[["one.D.info.df"]][study,"Study.ID"]
  )
  
  # Descriptive Results
  ## ...of control
  descr..reg.data.list[[time.point]][["n.control"]] <- append(
    descr..reg.data.list[[time.point]][["n.control"]],
    results.descriptive.array[control.no, "n", t.with.pre, outcome.no, scale, study]
  )
  descr..reg.data.list[[time.point]][["mean.control"]] <- append(
    descr..reg.data.list[[time.point]][["mean.control"]],
    results.descriptive.array[control.no, "Mean", t.with.pre, outcome.no, scale, study]
  )
  descr..reg.data.list[[time.point]][["sd.control"]] <- append(
    descr..reg.data.list[[time.point]][["sd.control"]],
    results.descriptive.array[control.no, "SD", t.with.pre, outcome.no, scale, study]
  )
  
  ## ...of intervention
  descr..reg.data.list[[time.point]][["n.int"]] <- append(
    descr..reg.data.list[[time.point]][["n.int"]],
    results.descriptive.array[intervention.no, "n", t.with.pre, outcome.no, scale, study]
  )
  descr..reg.data.list[[time.point]][["mean.int"]] <- append(
    descr..reg.data.list[[time.point]][["mean.int"]],
    results.descriptive.array[intervention.no, "Mean", t.with.pre, outcome.no, scale, study]
  )
  descr..reg.data.list[[time.point]][["sd.int"]] <- append(
    descr..reg.data.list[[time.point]][["sd.int"]],
    results.descriptive.array[intervention.no, "SD", t.with.pre, outcome.no, scale, study]
  )

# Results for Meta-Regression and Subgroup Analysis (this data will be drawn from the intervention group only)
  descr..reg.data.list[[time.point]][["sessions.duration"]] <- append(
    descr..reg.data.list[[time.point]][["sessions.duration"]],
    get.regression.data(m.data.list, "Sessions.Duration.in.minutes", study, intervention.no, nm.placeholder, without.mean.r)
  )

  descr..reg.data.list[[time.point]][["sessions.frequency"]] <- append(
    descr..reg.data.list[[time.point]][["sessions.frequency"]],
    get.regression.data(m.data.list, "Frequency.in.times.per.week", study, intervention.no, nm.placeholder, without.mean.r)
  )

  descr..reg.data.list[[time.point]][["programs.duration"]] <- append(
    descr..reg.data.list[[time.point]][["programs.duration"]],
    get.regression.data(m.data.list, "Total.Duration.in.Days", study, intervention.no, nm.placeholder, without.mean.r)
  )

  descr..reg.data.list[[time.point]][["delivery.mode"]] <- append(
    descr..reg.data.list[[time.point]][["delivery.mode"]],
    get.regression.data(m.data.list, "Delivery.Mode", study, intervention.no, nm.placeholder, without.mean.r)
  )

  descr..reg.data.list[[time.point]][["meditation.type"]] <- append(
    descr..reg.data.list[[time.point]][["meditation.type"]],
    get.regression.data(m.data.list, "Meditation.Type", study, intervention.no, nm.placeholder, without.mean.r)
  )

  descr..reg.data.list[[time.point]][["female.percent"]] <- append(
    descr..reg.data.list[[time.point]][["female.percent"]],
    get.regression.data(m.data.list, "female.percent", study, intervention.no, nm.placeholder, without.mean.r)
  )

  if (time.point == 1){
    descr..reg.data.list[[time.point]][["follow.up.period"]] <- append(
      descr..reg.data.list[[time.point]][["follow.up.period"]],
      0
    )
  } else if (time.point == 2){
    descr..reg.data.list[[time.point]][["follow.up.period"]] <- append(
      descr..reg.data.list[[time.point]][["follow.up.period"]],
      get.regression.data(m.data.list, "period.t1.t2", study, intervention.no, nm.placeholder, without.mean.r)
    )
    # note that t1 is post-test and t2 is 1st follow-up
  } else if (time.point == 3){
    descr..reg.data.list[[time.point]][["follow.up.period"]] <- append(
      descr..reg.data.list[[time.point]][["follow.up.period"]],
      get.regression.data(m.data.list, "period.t1.t3", study, intervention.no, nm.placeholder, without.mean.r)
    )
  } else {
    print("unknown time point in fill.meta.df()")
  }
  
  descr..reg.data.list
}
### Generate influence table
get.influence.df <- function(results.metafor){
  
  # get influence metrics
  results.metafor.inf <- influence(results.metafor)
  
  # define thresholds 
  rstudent.thresh <- qnorm(1 - (0.05 / (2 * results.metafor.inf$k))) # suggested by reporter function {metafor}
  dffits.thresh <- 3 * sqrt(results.metafor.inf$p / (results.metafor.inf$k - results.metafor.inf$p)) 
  hat.thresh <- 3 * (results.metafor.inf$p / results.metafor.inf$k)
  dfbetas.thresh <- 1 
  cook.d.thresh <- qchisq(0.5, df = results.metafor.inf$m)
  cook.d.threshs <- pchisq(results.metafor.inf$inf$cook.d, df=results.metafor.inf$m)
  cov.r.thresh <- 1
  
  # create threshold df
  threshs.df <- data.frame(matrix(
    data = c(rstudent.thresh, dffits.thresh, dfbetas.thresh, cook.d.thresh, 0.5, cov.r.thresh, NA, NA, hat.thresh, NA, NA),
    ncol = 11
  ))
  rownames(threshs.df) <- "thresh"
  colnames(threshs.df) <- c("rstudent", "dffits", "dfbetas", "cook.d", "cook.d.chi2.percent", "cov.r", "tau2.del", "QE.del", "hat", "weight", "inf")
  
  # generate data frame of all influece metrics
  inf.vals.df <- cbind(data.frame(results.metafor.inf$inf), data.frame(results.metafor.inf$dfbs)) %>%
    relocate(intrcpt, .after = dffits) %>%
    rename(dfbetas = intrcpt) %>%
    mutate(cook.d.chi2.percent = cook.d.threshs) %>%
  relocate(cook.d.chi2.percent, .after = cook.d)
  
  # add thresholds to data frame and round values
  inf.vals.df <- rbind(inf.vals.df, threshs.df) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 2))) %>%
    rename(`is.influential {metafor}` = inf) %>%
    mutate(`is.influential {metafor}` = c(results.metafor.inf$is.infl, NA)) %>%
    mutate(study = rownames(.), .before = rstudent)
  return(as.data.frame(inf.vals.df))
}
### Plot influence plots
# set plot size
# options(repr.plot.width = 11, repr.plot.height = 7, repr.plot.res = 200)

# this function is built on basis of the influence.analysis function of the dmetar package by Mathias Harrer et al.
# see: https://github.com/MathiasHarrer/dmetar/blob/master/R/influence.analysis.R

plot.influnece <- function(results.metafor, study.ids = "name", cluster = "id"){
  
  if ("rma.mv" %in% class(results.metafor)){
    class.mv <- T
  } else if ("rma.uni" %in% class(results.metafor)){
    class.mv <- F
  } else {
    cat("input has to be of class rma.uni or rma.mv got (from {metafor}) ", class(results.metafor), "instead.\n")
  }
  
  # get influence metrics
  results.metafor.inf <- if(class.mv){NA}else{influence(results.metafor)}
  
  # define thresholds 
  rstudent.thresh <- qnorm(1 - (0.05 / (2 * results.metafor$k))) # suggested by reporter function {metafor}
  dffits.thresh <- 3 * sqrt(results.metafor$p / (results.metafor$k - results.metafor$p)) 
  hat.thresh <- 3 * (results.metafor$p / results.metafor$k)
  dfbetas.thresh <- 1 
  cook.d.thresh <- qchisq(0.5, df = results.metafor$m)
  cov.r.thresh <- 1
  tau2.original <- results.metafor$tau2
  QE.original <- results.metafor$QE
  
  
  if (study.ids == "name"){
    if (class.mv) {
      # get study ids
      stud.id <- substr(results.metafor$data$study.id, 1, 4)

      # get study ids that are present more than once
      stud.id.tab <- table(stud.id)
      stud.id.multi <- names(stud.id.tab[stud.id.tab > 1])

      # add number to study ids that are present more than once
      if (length(stud.id.multi) >= 1){
        for (id.multi in stud.id.multi){
          id.m.i <- 1
          id.i <- 1
          for (id in stud.id){
            if (id %in% id.multi){
              stud.id[id.i] <- paste(stud.id[id.i], id.m.i)
              id.m.i <- id.m.i + 1
            }
          id.i <- id.i + 1
          }
        }
      } 
      # add outcome substring to id
      id.i <- 1
      for (id in stud.id){
        outc.string <- results.metafor$data[id.i, "outcome"]

        # get first 3 letters of outcome strings (if " " or "-" is present get first 3 letters ob splitted substrings)
        if (!(grepl(" ", outc.string) | grepl("-", outc.string))) {
          outc.string <- substr(results.metafor$data[id.i, "outcome"], 1, 4)
          outc.string <- paste(outc.string, id.i)
        } else if (grepl("-", outc.string[1])){
          outc.string <- substr(str_split_1(outc.string, "-")[1:2], 1, 3)
          outc.string <- paste(outc.string, sep = "-", collapse = "-")
          outc.string <- paste(outc.string, id.i)
        } else if (grepl(" ", outc.string)){
          outc.string <- substr(str_split_1(outc.string, " ")[1:2], 1, 3)
          outc.string <- paste(outc.string, sep = " ", collapse = " ")
          outc.string <- paste(outc.string, id.i)
        }

        stud.id[id.i] <- paste(stud.id[id.i], outc.string)

        id.i <- id.i + 1
      }
    } else {
      # get study ids
      stud.id <- substr(results.metafor$data$study.id, 1, 4)

      # get study ids that are present more than once
      stud.id.tab <- table(stud.id)
      stud.id.multi <- names(stud.id.tab[stud.id.tab > 1])

      # add number to study ids that are present more than once
      if (length(stud.id.multi) >= 1){
        for (id.multi in stud.id.multi){
          id.m.i <- 1
          id.i <- 1
          for (id in stud.id){
            if (id %in% id.multi){
              stud.id[id.i] <- paste(stud.id[id.i], id.m.i)
              id.m.i <- id.m.i + 1
            }
          id.i <- id.i + 1
          }
        }
      } 
    }
  } else if (study.ids == "num"){
    stud.id <- rownames(results.metafor$data)
  } else {
    print("error in plot.influnece: set parameter 'study.ids' to 'name' or 'num'")
  }
  
  # Generate plots
  y.ax.title.size <- 12
  y.ax.text.size <- 10
  
  if (!class.mv){
    
    # convert to desired data frame
    if (ncol(as.data.frame(results.metafor.inf$dfbs)) == 1){
      cheungviechtdata = cbind(study.num = rownames(as.data.frame(results.metafor.inf$inf)), study.id = stud.id, as.data.frame(results.metafor.inf$inf), is.infl = results.metafor.inf$is.infl) %>%
        add_column(as.data.frame(results.metafor.inf$dfbs), .after = "dffits") %>%
        rename(dfbetas = intrcpt)
    } else {
      cheungviechtdata = cbind(study.num = rownames(as.data.frame(results.metafor.inf$inf)), study.id = stud.id, as.data.frame(results.metafor.inf$inf), is.infl = results.metafor.inf$is.infl) %>%
        add_column(as.data.frame(results.metafor.inf$dfbs), .after = "dffits")
    }
    
    # rstudent.thresh = qnorm(1 - (0.05 / (2 * results.metafor.inf$k)))
    rstudent.plot = ggplot(cheungviechtdata, aes(y = rstudent, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
        geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
        theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
        axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab(expression(r["stud-del"])) +
        geom_hline(yintercept = rstudent.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = -rstudent.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

    # dffits.thresh = 3 * sqrt(results.metafor.inf$p/(results.metafor.inf$k - results.metafor.inf$p))
    dffits.plot = ggplot(cheungviechtdata, aes(y = dffits, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
        geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
        theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
        axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFFITS") +
        geom_hline(yintercept = dffits.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = - dffits.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')
    # cov.r.thresh = 1
    cov.r.plot = ggplot(cheungviechtdata, aes(y = cov.r, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
        geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
        theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
        axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("COVRATIO") +
        geom_hline(yintercept = cov.r.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

    tau2.del.plot = ggplot(cheungviechtdata, aes(y = tau2.del, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
        geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
        theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
        axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab(~ paste(tau ^ 2, " (L-0-0)")) +
        geom_hline(yintercept = 0, linetype='dashed', color='grey') +
        geom_hline(yintercept = tau2.original, linetype='dashed', color='grey')

    QE.del.plot = ggplot(cheungviechtdata, aes(y = QE.del, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
        geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
        theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
        axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("Q (L-0-0)") +
        geom_hline(yintercept = 0, linetype='dashed', color='grey') +
        geom_hline(yintercept = QE.original, linetype='dashed', color='grey')
    weight.plot = ggplot(cheungviechtdata, aes(y = weight, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
        geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
        theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
        axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("study weight") +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')
    
    ###
    
    # cook.d.thresh = qchisq(0.5, df=results.metafor.inf$m)
    cook.d.plot = ggplot(cheungviechtdata, aes(y = cook.d, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
        geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
        theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
        axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab(expression(D["i"])) +
        geom_hline(yintercept = cook.d.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')



    # hat.thresh = 3 * (results.metafor.inf$p / results.metafor.inf$k)
    hat.plot = ggplot(cheungviechtdata, aes(y = hat, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) + geom_line(color = "black") +
        geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) + theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1),
        legend.position = "none",  axis.title.y = element_text(size = y.ax.title.size),
        axis.text.y = element_text(size = y.ax.text.size)) + ylab("HAT") + 
        geom_hline(yintercept = hat.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')



    # dfbetas.thresh = 1
    if (ncol(as.data.frame(results.metafor.inf$dfbs)) == 1){
      dfbetas.plot = ggplot(cheungviechtdata, aes(y = dfbetas, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
          geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
          theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
          axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFBETAS") +
          geom_hline(yintercept = dfbetas.thresh, linetype='dashed', color='black') +
          geom_hline(yintercept = - dfbetas.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

    } else if (ncol(as.data.frame(results.metafor.inf$dfbs)) == 2){  # for meta-regression (linear)
      dfbetas.intrcpt.plot = ggplot(cheungviechtdata, aes(y = intrcpt, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
          geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
          theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
          axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFBETAS(inter)") +
          geom_hline(yintercept = dfbetas.thresh, linetype='dashed', color='black') +
          geom_hline(yintercept = - dfbetas.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

      dfbetas.mod.plot = ggplot(cheungviechtdata, aes(y = !!sym(colnames(as.data.frame(results.metafor.inf$dfbs)[2])), x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
          geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
          theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
          axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFBETAS(moderator)") +
          geom_hline(yintercept = dfbetas.thresh, linetype='dashed', color='black') +
          geom_hline(yintercept = - dfbetas.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

    } else if (ncol(as.data.frame(results.metafor.inf$dfbs)) == 3){  # for meta-regression (squared)
      dfbetas.intrcpt.plot = ggplot(cheungviechtdata, aes(y = intrcpt, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
          geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
          theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
          axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFBETAS(inter)") +
          geom_hline(yintercept = dfbetas.thresh, linetype='dashed', color='black') +
          geom_hline(yintercept = - dfbetas.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

      dfbetas.mod1.plot = ggplot(cheungviechtdata, aes(y = !!sym(colnames(as.data.frame(results.metafor.inf$dfbs)[2])), x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
          geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
          theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
          axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFBETAS(b1)") +
          geom_hline(yintercept = dfbetas.thresh, linetype='dashed', color='black') +
          geom_hline(yintercept = - dfbetas.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

      dfbetas.mod2.plot = ggplot(cheungviechtdata, aes(y = !!sym(colnames(as.data.frame(results.metafor.inf$dfbs)[3])), x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
          geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
          theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
          axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFBETAS(b2)") +
          geom_hline(yintercept = dfbetas.thresh, linetype='dashed', color='black') +
          geom_hline(yintercept = - dfbetas.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

    } else if (ncol(as.data.frame(results.metafor.inf$dfbs)) == 4){  # for meta-regression (e.g., subgroup analysis with 4 levels)
      dfbetas.intrcpt.plot = ggplot(cheungviechtdata, aes(y = intrcpt, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
          geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
          theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
          axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFBETAS(inter)") +
          geom_hline(yintercept = dfbetas.thresh, linetype='dashed', color='black') +
          geom_hline(yintercept = - dfbetas.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

      dfbetas.mod1.plot = ggplot(cheungviechtdata, aes(y = !!sym(colnames(as.data.frame(results.metafor.inf$dfbs)[2])), x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
          geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
          theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
          axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFBETAS(b1)") +
          geom_hline(yintercept = dfbetas.thresh, linetype='dashed', color='black') +
          geom_hline(yintercept = - dfbetas.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

      dfbetas.mod2.plot = ggplot(cheungviechtdata, aes(y = !!sym(colnames(as.data.frame(results.metafor.inf$dfbs)[3])), x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
          geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
          theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
          axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFBETAS(b2)") +
          geom_hline(yintercept = dfbetas.thresh, linetype='dashed', color='black') +
          geom_hline(yintercept = - dfbetas.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

      dfbetas.mod3.plot = ggplot(cheungviechtdata, aes(y = !!sym(colnames(as.data.frame(results.metafor.inf$dfbs)[4])), x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
          geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
          theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
          axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFBETAS(b3)") +
          geom_hline(yintercept = dfbetas.thresh, linetype='dashed', color='black') +
          geom_hline(yintercept = - dfbetas.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

    } else {
      print("error in plot.influnece(): function only works up to 4 model coefficients")
    }
    if (ncol(as.data.frame(results.metafor.inf$dfbs)) == 1){
      rma.influence.plot = arrangeGrob(rstudent.plot, dffits.plot, cook.d.plot, cov.r.plot, tau2.del.plot, QE.del.plot,
          hat.plot, weight.plot, dfbetas.plot, ncol = 2)
    } else if (ncol(as.data.frame(results.metafor.inf$dfbs)) == 2){
      rma.influence.plot = arrangeGrob(rstudent.plot, dffits.plot, cook.d.plot, cov.r.plot, tau2.del.plot, QE.del.plot,
          hat.plot, weight.plot, dfbetas.mod.plot, dfbetas.intrcpt.plot, ncol = 2)
    } else if (ncol(as.data.frame(results.metafor.inf$dfbs)) == 3){
      rma.influence.plot = arrangeGrob(rstudent.plot, dffits.plot, cook.d.plot, cov.r.plot, tau2.del.plot, QE.del.plot,
          hat.plot, weight.plot, dfbetas.mod1.plot, dfbetas.mod2.plot, dfbetas.intrcpt.plot, ncol = 2)
    } else if (ncol(as.data.frame(results.metafor.inf$dfbs)) == 4){
      rma.influence.plot = arrangeGrob(rstudent.plot, dffits.plot, cook.d.plot, cov.r.plot, tau2.del.plot, QE.del.plot,
          hat.plot, weight.plot, dfbetas.mod1.plot, dfbetas.mod2.plot, dfbetas.mod3.plot, dfbetas.intrcpt.plot, ncol = 2)
    }
    
  } else {
    if (cluster == "id"){  # no clustering
     cheungviechtdata <- data.frame(
        study.id = stud.id,
        study.num = results.metafor$id,
        cook.d = c(cooks.distance(results.metafor, cluster = eval(parse(text = cluster)))),
        hat = c(hatvalues(results.metafor, cluster = eval(parse(text = cluster)))),
        dfbetas = c(dfbetas(results.metafor, cluster = eval(parse(text = cluster)))$intrcpt),
        is.infl = c(F)
      )
    } else if (cluster == "study.id"){  # clustering per study
     cheungviechtdata <- data.frame(
        study.id = unique(results.metafor$data$study.id),
        study.num = 1:length(unique(results.metafor$data$study.id)),
        cook.d = c(cooks.distance(results.metafor, cluster = eval(parse(text = cluster)))),
        # hat = c(hatvalues(results.metafor, cluster = eval(parse(text = cluster)))),  # cluster parameter of hat values does not work
        dfbetas = c(dfbetas(results.metafor, cluster = eval(parse(text = cluster)))$intrcpt),
        is.infl = c(F)
      )
    } else if (cluster == "outcome"){  # clustering per study
     cheungviechtdata <- data.frame(
        study.id = unique(results.metafor$data$outcome),
        study.num = 1:length(unique(results.metafor$data$outcome)),
        cook.d = c(cooks.distance(results.metafor, cluster = eval(parse(text = cluster)))),
        # hat = c(hatvalues(results.metafor, cluster = eval(parse(text = cluster)))),  # cluster parameter of hat values does not work
        dfbetas = c(dfbetas(results.metafor, cluster = eval(parse(text = cluster)))$intrcpt),
        is.infl = c(F)
      )
    } else {
      print("error in plot.influence(): set parameter 'cluster' to 'id', 'study.id', or 'outcome'")
    }
 
    # cook.d.thresh = qchisq(0.5, df=results.metafor.inf$m)
    cook.d.plot = ggplot(cheungviechtdata, aes(y = cook.d, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
        geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
        theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
        axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab(expression(D["i"])) +
        geom_hline(yintercept = cook.d.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

    if (!cluster %in% c("study.id", "outcome")){
    # hat.thresh = 3 * (results.metafor.inf$p / results.metafor.inf$k)
      hat.plot = ggplot(cheungviechtdata, aes(y = hat, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) + geom_line(color = "black") +
          geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) + theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1),
          legend.position = "none",  axis.title.y = element_text(size = y.ax.title.size),
          axis.text.y = element_text(size = y.ax.text.size)) + ylab("HAT") + 
          geom_hline(yintercept = hat.thresh, linetype='dashed', color='black') +
          geom_hline(yintercept = 0, linetype='dashed', color='grey')
    }

    # dfbetas.thresh = 1
    dfbetas.plot = ggplot(cheungviechtdata, aes(y = dfbetas, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
        geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
        theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
        axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFBETAS") +
        geom_hline(yintercept = dfbetas.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = - dfbetas.thresh, linetype='dashed', color='black') +
      geom_hline(yintercept = 0, linetype='dashed', color='grey') 
    
    if (cluster == "id"){
      rma.influence.plot = arrangeGrob(cook.d.plot, hat.plot, dfbetas.plot, ncol = 2)
    } else {
      rma.influence.plot = arrangeGrob(cook.d.plot, dfbetas.plot, ncol = 2)
    }
  }
  
  grid.draw(rma.influence.plot)
}
# set plotsize to options(repr.plot.width = 30, repr.plot.height = 33, repr.plot.res = 200)

# function to split lines in plot labels
addline_format <- function(x,...){
    gsub('\\s','\n',x)
}

# load melt function it appeared to be not accessable
melt <- reshape2::melt

# this function is built on basis of the influence.analysis function of the dmetar package by Mathias Harrer et al.
# see: https://github.com/MathiasHarrer/dmetar/blob/master/R/influence.analysis.R
# ... and the measplot function of the NMAoutlier package by Maria Petropoulou
# see: https://github.com/petropouloumaria/NMAoutlier/blob/master/R/measplot.R

plot.net.influnece <- function(NMA.o.res, study.names.suff.data, use.labels.of.data.set = F){
  
  # define thresholds
  k <- length(NMA.o.res$estand.deleted)
  rstudent.thresh <- qnorm(1 - (0.05 / (2 * k)))  # suggested by reporter function of the metafor package (bonferoni correction)
  cov.r.thresh <- dfbetas.thresh <- cook.d.thresh <- 1
  
  # plot parameters
  y.ax.title.size <- 15
  y.ax.text.size <- 13
  xlabel <- "study deleted"
  
  # get data frame of all data
  stat.names <- names(NMA.o.res)[-c(1, 2, 16)]
  stats.df <- data.frame(matrix(NA, ncol = 0, nrow = k))
  for (stat in stat.names){
    if (!stat %in% c("Restimates", "DFbetas")){
      stat.data <- data.frame(NMA.o.res[[stat]])
      colnames(stat.data) <- stat
      stats.df <- cbind(stats.df, stat.data)
    }
  }
  
  for (stat in c("Restimates", "DFbetas")){
    stat.data <- t(NMA.o.res[[stat]])
    rownames(stat.data) <- NULL
    colnames(stat.data) <- paste(stat, colnames(stat.data))
    stats.df <- cbind(stats.df, stat.data)
  }
  stats.df <- t(stats.df)
  stats <- nrow(stats.df)
  
  if (use.labels.of.data.set){
    stlab <- unique(NMA.o.res$dat[ ,3])
    xlabels <- factor(as.character(stlab), levels = as.character(stlab))
  } else {
    stlab.nchars <- nchar(study.names.suff.data)
    
    # generate study labels with 1st 3 and last 2 letters of original label
    stlab <- paste(substr(study.names.suff.data, 1, 3), substr(study.names.suff.data, stlab.nchars - 1, stlab.nchars), sep = "")
    xlabels <- factor(as.character(stlab), levels = as.character(stlab)) # as factor to prevent ggplot from reordering the x labels in alphabetical order (comment by Petropoulou, M. in measplot())
  }

  graphs <- vector("list", stats - 1)

  upper <- max(stats.df)
  lower <- min(stats.df)

  limu <- round(max(upper), 2)
  liml <- round(min(lower), 2)
  
  # collect plots
  for (j in 1:(stats - 1)) {
    
    ## Localizing variables - workaround for ggplot problem with
    ## handling variables in multiple plots (variable environment
    ## scope problem; comment by Petropoulou, M. in measplot())
    local({
      
      ylabel <- rownames(stats.df)[j]
      
      if (ylabel == "estand.deleted") {

        ylabel <- expression(r["stan-del"])
        tresh.u <- rstudent.thresh
        tresh.l <- -rstudent.thresh

      } else if (ylabel == "estud.deleted") {

        ylabel <- expression(r["stud-del"])
        tresh.u <- rstudent.thresh
        tresh.l <- -rstudent.thresh

      } else if (ylabel == "Cooks.distance") {

        ylabel <- expression(D["i"])
        tresh.u <- cook.d.thresh
        tresh.l <- NULL

      } else if (ylabel == "Covratio") {

        ylabel <- "COVRATIO"
        tresh.u <- cov.r.thresh
        tresh.l <- NULL

      } else if (ylabel == "w.leaveoneout") {

        ylabel <- "study weight"
        tresh.u <- NULL
        tresh.l <- NULL

      } else if (ylabel == "H.leaveoneout") {

        ylabel <- "leverage"
        tresh.u <- NULL
        tresh.l <- NULL

      } else if (ylabel == "heterog.leaveoneout") {

        ylabel <- ~ tau ^ 2
        tresh.u <- NULL
        tresh.l <- NULL

      } else if (ylabel == "Rheterogeneity") {

        ylabel <- "R for het."
        tresh.u <- NULL
        tresh.l <- NULL

      } else if (ylabel == "RQtotal") {

        ylabel = "R for Qtotal"
        tresh.u <- NULL
        tresh.l <- NULL

      } else if (ylabel == "RQhet") {

        ylabel <- "R for Qhet."
        tresh.u <- NULL
        tresh.l <- NULL

      } else if (ylabel == "RQinc") {

        ylabel <- "R for incons."
        tresh.u <- NULL
        tresh.l <- NULL

      } else if (grepl("Restimates", ylabel)) {
        
        ylabel <- addline_format(gsub("Restimates", "R_for Qestimates", ylabel))
        tresh.u <- NULL
        tresh.l <- NULL

      } else if (grepl("DFbetas", ylabel)) {
        
        ylabel <- addline_format(ylabel)
        tresh.u <- dfbetas.thresh
        tresh.l <- -dfbetas.thresh

      }
      
      melt_data <- melt(stats.df[j, ], id.vars = 0)
      y_values <- melt_data$value
      j <- j
      g <- eval(substitute(
        ggplot(data = melt_data, aes(y = y_values, x = xlabels, group = 1)) + # eval(substitute) is another workaround for the aforementioned problem (comment by Petropoulou, M. in measplot())
          geom_line(color = "black") + geom_point(size = 2, col = "blue") + 
          theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
          axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + xlab(xlabel) + ylab(ylabel) +
          geom_hline(yintercept = tresh.u, linetype='dashed', color='black') +
          geom_hline(yintercept = tresh.l, linetype='dashed', color='black') +
          geom_hline(yintercept = 0, linetype='dashed', color='grey')
      ))
      graphs[[j]] <<- g
    })
  }
  net.influence.plot <- arrangeGrob(grobs = graphs, ncol = 1)
    
  
  grid.draw(net.influence.plot)
}
### Get pooled standard deviation from studies
get.pooled.sd <- function(n.e, sd.e, n.c, sd.c){
  # check if input data has same length
  len.vec <- c(length(n.e), length(sd.e), length(n.c), length(sd.c))
  vec.s.l <- T
  if (length(unique(len.vec)) > 1){
    cat("\nError in get.pooled.var(): Length of input has to be same length. Got n.e = ", length(n.e), ", sd.e = ", length(sd.e), ", n.c = ", length(n.c), ", sd.c = ", length(sd.c), "instead.\n", sep ="")
    vec.s.l <- F
  }
  
  if (vec.s.l){
    # valculate numerator for pooled standard deviation
    N <- n.e + n.c
    sd.pooled <- sqrt(((n.e - 1) * sd.e^2 + (n.c - 1) * sd.c^2) / (N - 2))
    return(sd.pooled)
  } else {
    return(NA)
  }
}
