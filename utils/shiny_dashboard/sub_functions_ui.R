# This script contains three utility functions for generating Shiny dashboard UI components for meta-analysis results that are used in main_functions_ui.R:
# 1. get.regression.results() - Retrieves regression results for specified moderators and outcomes
# 2. set.moderator.rows.w.o.() - Creates fluidRows displaying moderator analyses with outlier detection
# 3. set.moderator.rows.comp() - Creates fluidRows for sensitivity analysis with outlier and influence diagnostics



### Get regression results from print.meta.results()
get.regression.results <- function(
  meta.df.list, outcome, moderator, degree, filter = FALSE, preferred.scale = FALSE, without.mean.r = FALSE
){
  if (degree == 1){
    regression.results <- print.meta.results(
      outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list, return.data = "regression.results.linear",
      basic = F, moderator.vec = c(moderator), print.baujat.regression = F, print.regression.results = F,
      regression.degree.1 = T, regression.degree.2 = F, print.regplot = F, without.mean.r = without.mean.r,
      filter.regression.linear.list = if(filter){if(length(outlier.list[[paste(moderator, ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste(moderator, ".lin", sep = "")]][[outcome]])}}else{F}
    )
  } else if (degree == 2){
    regression.results <- print.meta.results(
      outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list, return.data = "regression.results.poly",
      basic = F, moderator.vec = c(moderator), print.baujat.regression = F, print.regression.results = F,
      regression.degree.1 = F, regression.degree.2 = T, print.regplot = F, without.mean.r = without.mean.r,
      filter.regression.poly.list = if(filter){if(length(outlier.list[[paste(moderator, ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste(moderator, ".sq", sep = "")]][[outcome]])}}else{F}
    )
  } else {
    cat(
      'No regression results returned from get.regression.results() for outcome "', outcome, '" and moderator "', moderator,
      "\nset degree to 1 or 2\n", sep= ""
    )
  }
  
  return(regression.results)
}


### Create fluidRows of moderators (with outliers)
set.moderator.rows.w.o. <- function(
  meta.df.list, outcome, preferred.scale, outcome.alias, sig.0.05.color, sig.0.1.color,
  moderator.vec = c("programs.duration", "sessions.duration", "sessions.frequency", "follow.up.period")
){
  
  # create variable testing if there is enough data for the moderator "follow.up.period"
  if(nrow(meta.df.list[[1]]) > 0){
    mod.data.f.up.t1 <- meta.df.list[[1]][, "follow.up.period"]
  } else {
    mod.data.f.up.t1 <- c()
  }
  
  if(nrow(meta.df.list[[2]]) > 0){
    mod.data.f.up.t2 <- meta.df.list[[2]][, "follow.up.period"]
  } else {
    mod.data.f.up.t2 <- c()
  }
  
  if(nrow(meta.df.list[[3]]) > 0){
    mod.data.f.up.t3 <- meta.df.list[[3]][, "follow.up.period"]
  } else {
    mod.data.f.up.t3 <- c()
  }
  
  f.up.isnt.na <- 
  if (          
    length(mod.data.f.up.t1[which(!is.na.or.nm(mod.data.f.up.t1))]) >= 4 &
    (
      length(mod.data.f.up.t2[which(!is.na.or.nm(mod.data.f.up.t2))]) >= 1 |
      length(mod.data.f.up.t3[which(!is.na.or.nm(mod.data.f.up.t3))]) >= 1
    )
  ){
    !is.na(print.meta.results(
      outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list, return.data = "regression.results.linear",
      basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
      print.regplot = F, filter.regression.linear.list = F
    )[1])
  } else {FALSE}
  
  fluidRow.list <- list()
  for (moderator in moderator.vec){
    if(nrow(meta.df.list[[1]]) > 0){
      mod.data <- meta.df.list[[1]][, moderator]
    } else {
      mod.data <- c()
    }
    k.4 <- length(mod.data[which(!is.na.or.nm(mod.data))]) >= 4  # check if 4 or more studies are included
    
    fluidRow.list <- append(fluidRow.list, fluidRow(
      box(
        width = 12,
        class = "well",
        h3(moderator)
      )
    ))
    
    fluidRow.list <- append(fluidRow.list, fluidRow(
      box(
        title = "linear",
        width = 6,
        background = if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){
          if(
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$QMp <= 0.05 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$pval[2] <= 0.05
          ){
            sig.0.05.color
          } else if (
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$QMp <= 0.1 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$pval[2] <= 0.1
          ){
            sig.0.1.color
          } else {NULL}
        } else {NULL},
        class = "well",
        plotOutput(paste(moderator, ".lin", outcome.alias, sep = "")),
        footer = p(paste(
          "k: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$k}else{"NA"}, "; ",
          "Test of moderators p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$QMp, digits = 4)}else{"NA"}, "; ",
          "Regression coefficient p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$pval[2], digits = 4)}else{"NA"}, "; ",
          "R^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$R2, digits = 2)}else{"NA"}, "%; ",
          "I^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$I2, digits = 2)}else{"NA"}, "%",
          sep = ""
        ), style="color:black"),
        align="center",
        collapsible = TRUE
      ),
      box(
        title = "squared",
        width = 6,
        background = if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){
          if(
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$QMp <= 0.05 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$pval[2] <= 0.05
          ){
            sig.0.05.color
          } else if (
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$QMp <= 0.1 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$pval[2] <= 0.1
          ){
            sig.0.1.color
          } else {NULL}
        } else {NULL},
        class = "well",
        plotOutput(paste(moderator, ".sq", outcome.alias, sep = "")),
        footer = p(paste(
          "k: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$k}else{"NA"}, "; ",
          "Test of moderators p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$QMp, digits = 4)}else{"NA"}, "; ",
          "Regression coefficient p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$pval[2], digits = 4)}else{"NA"}, "; ",
          "R^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$R2, digits = 2)}else{"NA"}, "%; ",
          "I^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$I2, digits = 2)}else{"NA"}, "%",
          sep = ""
        ), style="color:black"),
        align="center",
        collapsible = TRUE
      )
    ))
  }
  return(fluidRow.list)
}


### Create fluidRows of moderators (for sensitivity analysis)
set.moderator.rows.comp <- function(
  meta.df.list, outcome, preferred.scale, outcome.alias, sig.0.05.color, sig.0.1.color, without.mean.r = F,
  moderator.vec = c("programs.duration", "sessions.duration", "sessions.frequency", "follow.up.period")
){
  
  # create variable testing if there is enough data for the moderator "follow.up.period"
  if(nrow(meta.df.list[[1]]) > 0){
    mod.data.f.up.t1 <- meta.df.list[[1]][, "follow.up.period"]
  } else {
    mod.data.f.up.t1 <- c()
  }
  
  if(nrow(meta.df.list[[2]]) > 0){
    mod.data.f.up.t2 <- meta.df.list[[2]][, "follow.up.period"]
  } else {
    mod.data.f.up.t2 <- c()
  }
  
  if(nrow(meta.df.list[[3]]) > 0){
    mod.data.f.up.t3 <- meta.df.list[[3]][, "follow.up.period"]
  } else {
    mod.data.f.up.t3 <- c()
  }
  
  f.up.isnt.na <- 
  if (          
    length(mod.data.f.up.t1[which(!is.na.or.nm(mod.data.f.up.t1))]) >= 4 &
    (
      length(mod.data.f.up.t2[which(!is.na.or.nm(mod.data.f.up.t2))]) >= 1 |
      length(mod.data.f.up.t3[which(!is.na.or.nm(mod.data.f.up.t3))]) >= 1
    )
  ){
    !is.na(print.meta.results(
      outcome, preferred.scale = p>eferred.scale, meta.df.list = meta.df.list, return.data = "regression.results.linear",
      basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
      print.regplot = F, filter.regression.linear.list = F
    )[1])
  } else {FALSE}
  
  fluidRow.list <- list()
  
  fluidRow.list <- append(fluidRow.list, fluidRow(
    box(
      title = "Correlation Matrix",
      width = 12,
      class = "well",
      tableOutput(paste("cor.matrix.", outcome.alias, ifelse(without.mean.r, "w.o.m.r", ""), sep = "")),
      align="right",
      collapsible = TRUE,
      collapsed = TRUE
    )
  ))
  
  for (moderator in moderator.vec){
    if(nrow(meta.df.list[[1]]) > 0){
      mod.data <- meta.df.list[[1]][, moderator]
    } else {
      mod.data <- c()
    }
    k.4 <- length(mod.data[which(!is.na.or.nm(mod.data))]) >= 4  # check if 4 or more studies are included
    
    fluidRow.list <- append(fluidRow.list, fluidRow(
      box(
        width = 12,
        class = "well",
        h3(moderator)
      )
    ))
    
    fluidRow.list <- append(fluidRow.list, fluidRow(
      box(
        width = 12,
        class = "well",
        h4(paste(
          "Outliers [linear; squared]:",
          paste(
            paste(outlier.list[[paste(moderator, ".lin", sep = "")]][[outcome]], collapse = ", "), "; ",
            paste(outlier.list[[paste(moderator, ".sq", sep = "")]][[outcome]], collapse = ", "),
            sep = ""
          )
        ))
      )
    ))
    
    # Outlier and influence pltos
    fluidRow.list <- append(fluidRow.list, fluidRow(
      box(
        title = paste("Baujat Plot for linear", moderator),
        width = 6,
        class = "well",
        plotOutput(paste(moderator, ".baujat", outcome.alias, ifelse(without.mean.r, "w.o.m.r", ""), sep = "")),
        align="center",
        collapsible = T,
        collapsed = T
      ),
      box(
        title = paste("Influence Plots for linear", moderator),
        width = 6,
        class = "well",
        plotOutput(paste(moderator, ".influence", outcome.alias, ifelse(without.mean.r, "w.o.m.r", ""), sep = "")),
        align="center",
        collapsible = T,
        collapsed = T
      )
    ))
    
    fluidRow.list <- append(fluidRow.list, fluidRow(
      box(
        title = paste("Baujat Plot for squared", moderator),
        width = 6,
        class = "well",
        plotOutput(paste(moderator, ".baujat.sq", outcome.alias, ifelse(without.mean.r, "w.o.m.r", ""), sep = "")),
        align="center",
        collapsible = T,
        collapsed = T
      ),
      box(
        title = paste("Influence Plots for squared", moderator),
        width = 6,
        class = "well",
        plotOutput(paste(moderator, ".influence.sq", outcome.alias, ifelse(without.mean.r, "w.o.m.r", ""), sep = "")),
        align="center",
        collapsible = T,
        collapsed = T
      )
    ))
    
    # Normal Q-Q plot
    fluidRow.list <- append(fluidRow.list, fluidRow(
      box(
        title = paste("Q-Q Plot for", moderator),
        width = 12,
        class = "well",
        plotOutput(paste(moderator, ".qq", outcome.alias, ifelse(without.mean.r, "w.o.m.r", ""), sep = "")),
        align="center",
        collapsible = T,
        collapsed = T
      )
    ))
    
    # Rregression Plots
    fluidRow.list <- append(fluidRow.list, fluidRow(
      box(
        title = "linear (with outliers)",
        width = 6,
        background = if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){
          if(
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$QMp <= 0.05 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$pval[2] <= 0.05
          ){
            sig.0.05.color
          } else if (
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$QMp <= 0.1 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$pval[2] <= 0.1
          ){
            sig.0.1.color
          } else {NULL}
        } else {NULL},
        class = "well",
        plotOutput(paste(moderator, ".lin.w.o.", outcome.alias, ifelse(without.mean.r, "w.o.m.r", ""), sep = "")),
        footer = p(paste(
          "k: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$k}else{"NA"}, "; ",
          "Test of moderators p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$QMp, digits = 4)}else{"NA"}, "; ",
          "Regression coefficient p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$pval[2], digits = 4)}else{"NA"}, "; ",
          "R^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$R2, digits = 2)}else{"NA"}, "%; ",
          "I^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$I2, digits = 2)}else{"NA"}, "%",
          sep = ""
        ), style="color:black"),
        align="center",
        collapsible = TRUE
      ),
      box(
        title = "squared (with outliers)",
        width = 6,
        background = if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){
          if(
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$QMp <= 0.05 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$pval[2] <= 0.05
          ){
            sig.0.05.color
          } else if (
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$QMp <= 0.1 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$pval[2] <= 0.1
          ){
            sig.0.1.color
          } else {NULL}
        } else {NULL},
        class = "well",
        plotOutput(paste(moderator, ".sq.w.o.", outcome.alias, ifelse(without.mean.r, "w.o.m.r", ""), sep = "")),
        footer = p(paste(
          "k: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$k}else{"NA"}, "; ",
          "Test of moderators p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$QMp, digits = 4)}else{"NA"}, "; ",
          "Regression coefficient p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$pval[2], digits = 4)}else{"NA"}, "; ",
          "R^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$R2, digits = 2)}else{"NA"}, "%; ",
          "I^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$I2, digits = 2)}else{"NA"}, "%",
          sep = ""
        ), style="color:black"),
        align="center",
        collapsible = TRUE
      )
    ))
    
    fluidRow.list <- append(fluidRow.list, fluidRow(
      box(
        title = "linear (without outliers)",
        width = 6,
        background = if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){
          if(
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = T, preferred.scale = preferred.scale)$QMp <= 0.05 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = T, preferred.scale = preferred.scale)$pval[2] <= 0.05
          ){
            sig.0.05.color
          } else if (
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = T, preferred.scale = preferred.scale)$QMp <= 0.1 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = T, preferred.scale = preferred.scale)$pval[2] <= 0.1
          ){
            sig.0.1.color
          } else {NULL}
        } else {NULL},
        class = "well",
        plotOutput(paste(moderator, ".lin.n.o.", outcome.alias, ifelse(without.mean.r, "w.o.m.r", ""), sep = "")),
        footer = p(paste(
          "k: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = T, preferred.scale = preferred.scale)$k}else{"NA"}, "; ",
          "Test of moderators p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = T, preferred.scale = preferred.scale)$QMp, digits = 4)}else{"NA"}, "; ",
          "Regression coefficient p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = T, preferred.scale = preferred.scale)$pval[2], digits = 4)}else{"NA"}, "; ",
          "R^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = T, preferred.scale = preferred.scale)$R2, digits = 2)}else{"NA"}, "%; ",
          "I^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = T, preferred.scale = preferred.scale)$I2, digits = 2)}else{"NA"}, "%",
          sep = ""
        ), style="color:black"),
        align="center",
        collapsible = TRUE
      ),
      box(
        title = "squared (without outliers)",
        width = 6,
        background = if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){
          if(
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = T, preferred.scale = preferred.scale)$QMp <= 0.05 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = T, preferred.scale = preferred.scale)$pval[2] <= 0.05
          ){
            sig.0.05.color
          } else if (
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = T, preferred.scale = preferred.scale)$QMp <= 0.1 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = T, preferred.scale = preferred.scale)$pval[2] <= 0.1
          ){
            sig.0.1.color
          } else {NULL}
        } else {NULL},
        class = "well",
        plotOutput(paste(moderator, ".sq.n.o.", outcome.alias, ifelse(without.mean.r, "w.o.m.r", ""), sep = "")),
        footer = p(paste(
          "k: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = T, preferred.scale = preferred.scale)$k}else{"NA"}, "; ",
          "Test of moderators p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = T, preferred.scale = preferred.scale)$QMp, digits = 4)}else{"NA"}, "; ",
          "Regression coefficient p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = T, preferred.scale = preferred.scale)$pval[2], digits = 4)}else{"NA"}, "; ",
          "R^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = T, preferred.scale = preferred.scale)$R2, digits = 2)}else{"NA"}, "%; ",
          "I^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = T, preferred.scale = preferred.scale)$I2, digits = 2)}else{"NA"}, "%",
          sep = ""
        ), style="color:black"),
        align="center",
        collapsible = TRUE
      )
    ))

  }
  return(fluidRow.list)
}


# # function not verified
# set.moderator.rows.comp <- function(
#   meta.df.list, outcome, preferred.scale, outcome.alias, sig.0.05.color, sig.0.1.color,
#   moderator.vec = c("programs.duration", "sessions.duration", "sessions.frequency", "follow.up.period")){
  
#   # set variables for better readability of code below
#   f.up.isnt.na <- 
#   if (
#     nrow(meta.df.list[[1]]) >= 4 &
#     (
#       nrow(meta.df.list[[2]]) >= 1 |
#       nrow(meta.df.list[[3]]) >= 1
#     )
#   ){
#     !is.na(print.meta.results(
#       outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list, return.data = "regression.results.linear",
#       basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
#       print.regplot = F, filter.regression.linear.list = F
#     )[1])
#   } else {FALSE}
  
#   k.4 <- nrow(meta.df.list[[1]]) >= 4  # check if 4 or more studies are included
  
#   fluidRow.list <- list()
#   for (moderator in moderator.vec){
# # Headings
#     fluidRow.list <- append(fluidRow.list, fluidRow(
#       box(
#         width = 12,
#         class = "well",
#         h3(moderator)
#       )
#     ))
    
#     for (without.mean.r in c(FALSE, TRUE)){
#       if (without.mean.r){
#         fluidRow.list <- append(fluidRow.list, fluidRow(
#           box(
#             width = 12,
#             class = "well",
#             h4("Without mean ranges")
#           )
#         ))
#       } else {
#         fluidRow.list <- append(fluidRow.list, fluidRow(
#           box(
#             width = 12,
#             class = "well",
#             h4("With mean ranges")
#           )
#         ))
#       }
#       for (filter in c(TRUE, FALSE)){
#         if (filter){
#           fluidRow.list <- append(fluidRow.list, fluidRow(
#             box(
#               width = 12,
#               class = "well",
#               h5("Without outliers")
#             )
#           ))
#         } else {
#           fluidRow.list <- append(fluidRow.list, fluidRow(
#             box(
#               width = 12,
#               class = "well",
#               h5("With outliers")
#             )
#           ))
#         }
        
# # Plots
#         fluidRow.list <- append(fluidRow.list, fluidRow(
#           box(
#             title = "linear",
#             width = 6,
#             background = if(nrow(meta.df.list[[1]]) >= 4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){
#               if(
#                  get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 1, filter = filter, preferred.scale = preferred.scale)$QMp <= 0.05 &
#                  get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 1, filter = filter, preferred.scale = preferred.scale)$pval[2] <= 0.05
#               ){
#                 sig.0.05.color
#               } else if (
#                  get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 1, filter = filter, preferred.scale = preferred.scale)$QMp <= 0.1 &
#                  get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 1, filter = filter, preferred.scale = preferred.scale)$pval[2] <= 0.1
#               ){
#                 sig.0.1.color
#               } else {NULL}
#             } else {NULL},
#             class = "well",
#             plotOutput(paste(
#               moderator, ".lin", outcome.alias, if(filter){".filt"}else{".unfilt"}, if(without.mean.r){".w.o.mean.r"}else{".w.mean.r"},
#               sep = ""
#             )),
#             footer = p(paste(
#               "k: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){ get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 1, filter = filter, preferred.scale = preferred.scale)$k}else{"NA"}, "; ",
#               "Test of moderators p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round( get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 1, filter = filter, preferred.scale = preferred.scale)$QMp, digits = 4)}else{"NA"}, "; ",
#               "Regression coefficient p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round( get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 1, filter = filter, preferred.scale = preferred.scale)$pval[2], digits = 4)}else{"NA"}, "; ",
#               "R^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round( get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 1, filter = filter, preferred.scale = preferred.scale)$R2, digits = 2)}else{"NA"}, "%; ",
#               "I^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round( get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 1, filter = filter, preferred.scale = preferred.scale)$I2, digits = 2)}else{"NA"}, "%",
#               sep = ""
#             ), style="color:black"),
#             align="center",
#             collapsible = TRUE
#           ),
#           box(
#             title = "squared",
#             width = 6,
#             background = if(nrow(meta.df.list[[1]]) >= 4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){
#               if(
#                  get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 2, filter = filter, preferred.scale = preferred.scale)$QMp <= 0.05 &
#                  get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 2, filter = filter, preferred.scale = preferred.scale)$pval[2] <= 0.05
#               ){
#                 sig.0.05.color
#               } else if (
#                  get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 2, filter = filter, preferred.scale = preferred.scale)$QMp <= 0.1 &
#                  get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 2, filter = filter, preferred.scale = preferred.scale)$pval[2] <= 0.1
#               ){
#                 sig.0.1.color
#               } else {NULL}
#             } else {NULL},
#             class = "well",
#             plotOutput(paste(
#               moderator, ".sq", outcome.alias, if(filter){".filt"}else{".unfilt"}, if(without.mean.r){".w.o.mean.r"}else{".w.mean.r"},
#               sep = ""
#             )),
#             footer = p(paste(
#               "k: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){ get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 2, filter = filter, preferred.scale = preferred.scale)$k}else{"NA"}, "; ",
#               "Test of moderators p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round( get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 2, filter = filter, preferred.scale = preferred.scale)$QMp, digits = 4)}else{"NA"}, "; ",
#               "Regression coefficient p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round( get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 2, filter = filter, preferred.scale = preferred.scale)$pval[2], digits = 4)}else{"NA"}, "; ",
#               "R^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round( get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 2, filter = filter, preferred.scale = preferred.scale)$R2, digits = 2)}else{"NA"}, "%; ",
#               "I^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round( get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 2, filter = filter, preferred.scale = preferred.scale)$I2, digits = 2)}else{"NA"}, "%",
#               sep = ""
#             ), style="color:black"),
#             align="center",
#             collapsible = TRUE
#           )
#         ))
#       }
#     }
#   }
#   return(fluidRow.list)
# }
