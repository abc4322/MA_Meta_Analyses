# This script includes the second main utility function for meta-analyses, which performs regression analyses based on results from
# the first main utility function meta.analyze().

### Print all Results
# calculating regression results was outsourced from meta.analyze() to this function as it occured an unknown error
# does not support network-meta-analysis or choice of the comparison groups
print.meta.results <- function(
  outcome, preferred.scale = F, print.meta.results = T, meta.df.list = F, return.data = F, results.metafor.fixed = F,
  
  # arguments for forest and funnel plot
  basic = T, 
  print.forest = T, forest.add.fix.eff.mod = T, forest.add.power = F, forest.layout = "RevMan5", print.funnel = T, funnel.label = T, funnel.label.out.only = T,
  print.influence = F, influence.ids = "name", print.baujat = T,
  split.subgroups = T, print.forest.sub.single = F, subgroup.method = "random",
  filter.forest..funnel.vec = F, exclude.high.rob.vec = F,
    # basic is to decide if forest and funnel plot shell be printed
    # set forest.layout = gs("layout") to use custom layout
  
  # arguments for regression
  ## (singlie regression)
  regression = T, 
  moderator.vec = c("sessions.duration", "sessions.frequency", "programs.duration", "meditation.total", "follow.up.period", "delivery.mode", "meditation.type", "female.percent"),
  regression.degree.1 = T, regression.degree.2 = T, filter.regression.vec = F, print.qq.norm = F, print.regression.df = F,
  filter.regression.linear.list = F, filter.regression.poly.list = F,
    # regression = F will disable only single not multiple regression
    # filter.regression.vec filters regression.df that comes from meta.analyze
    # filter.regression.linear/poly.list (filters only linear regression) --> list with filtering vectors in order of moderator.vec ("" means no filtering)
  
  ## (additive arguments for multiple regression)
  regression.multiple = F,
  moderator.multiple.list = F, filter.multiple.regression.linear.list = F, filter.multiple.regression.poly.list = F,
  regression.multiple.degree.2 = T, non.interaction = T, interaction = F,
  
  ## for both single and multiple regression
  print.regression.results = T,  print.regplot = T, print.baujat.regression = T, print.gosh.regression = F, regression.label = F,
  without.mean.r = F
){
  
  meditation.type.var <- meditation.type.all
  
  cat("\n#", outcome, "----------------------------------\n")
  
  return.data.var <- NULL
  
# Forest and Funnel Plot
  if (basic){
    return.data.var <- meta.analyze(
      outcome, meditation.types = meditation.type.var, m.data.list = m.data.list, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
      print.forest = print.forest, forest.add.fix.eff.mod = forest.add.fix.eff.mod, forest.add.power = forest.add.power, forest.layout = forest.layout, print.baujat = print.baujat, print.funnel = print.funnel,
      funnel.label = funnel.label, funnel.label.out.only = funnel.label.out.only, print.meta.results = print.meta.results, print.influence = print.influence, influence.ids = influence.ids,
      return.data = return.data, results.metafor.fixed = results.metafor.fixed,
      filter.forest..funnel.vec = filter.forest..funnel.vec, split.subgroups = split.subgroups, print.forest.sub.single = print.forest.sub.single, subgroup.method = subgroup.method,
      exclude.high.rob.vec = exclude.high.rob.vec, sort.by = "hedges.g", without.mean.r = without.mean.r  # print.descriptive = T
    )
  }
  
# Single Regression (one moderator)
  if (regression){
    mod.no <- 1
    
    # create regression df
    for (moderator in moderator.vec){
      if (length(moderator.vec) > 1){
        print(moderator)
      }
      regression.df <- meta.analyze(
        outcome, meditation.type.var, m.data.list, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        moderator = moderator, filter.regression.vec = filter.regression.vec,
        split.subgroups = F, print.influence = F, without.mean.r = without.mean.r
      )
      
      if (nrow(regression.df) >= 4){
        if (F %in% (regression.df[1,moderator] == regression.df[,moderator])){
          # ^ checks if there is more than one equal value present in regression.df[,moderator]
          
  ## Linear Regression
          if (regression.degree.1){
            # filter for linear regression
            if (
              is.list(filter.regression.linear.list) &
              if(is.list(filter.regression.linear.list)){filter.regression.linear.list[[mod.no]][1] != ""}else{F}
            ){
              regression.df.filtered <- regression.df[filter.regression.linear.list[[mod.no]],]
            } else {
              regression.df.filtered <- regression.df
            }

            # get results for linear regression
            results.regression.linear <- get.results.metafor(
              regression.df.filtered, moderator = moderator, results.metafor.fixed = results.metafor.fixed
            )
            
            if (return.data == "regression.results.linear"){
              return.data.var <- results.regression.linear
            }
            
            for (modertor.label in colnames(regression.labels.df)){
              if (moderator == modertor.label){
                x.label <- regression.labels.df[1,moderator]
              }
            }
            
            # regression/bubble plot
            if (moderator == "delivery.mode" & print.regplot){
              # with metafor | categorical moderators are not implemented in regplot.ggplot()
              regplot(
                results.regression.linear,
                refline=0,
                pi = T,
                grid = T,
                label = regression.label,
                legend = F,
                xlab = x.label
                # xlim = c(0,60), predlim = c(0,60)  # adjusting display area of graph
              )
            } else if (moderator == "meditation.type" & print.regplot){
              print("error in print.meta.results(): ploting regression plots is not implemented for categorical moderators with more than 2 levels like 'meditation.type'")
            } else if (print.regplot){
              # with ggplot
              regplot.ggplot(regression.df.filtered, moderator, regression.label)
            }
            
            if (print.qq.norm){
              qqnorm(results.regression.linear, label = regression.label)
            }

            # baujat plot
            if (print.baujat.regression){
              # outlier analysis
              # par(mar = c(5, 6, 4, 2))  # to set plot parameters?
              # plot(influence(results.regression.linear), cex = 0.8, las = 1)
              if (nrow(regression.df.filtered) > 4){
                # get study ids
                stud.id <- results.regression.linear$data$study.id

                ## get study ids that are present more than once
                stud.id.tab <- table(stud.id)
                stud.id.multi <- names(stud.id.tab[stud.id.tab > 1])

                ## add number to study ids that are present more than once
                if (length(stud.id.multi) >= 1){
                  for (id.multi in stud.id.multi){
                    id.m.i <- 1
                    id.i <- 1
                    for (id in stud.id){
                      if (id %in% id.multi){
                        stud.id[id.i] <- paste(stud.id[id.i], ", ", id.m.i, sep = "")
                        id.m.i <- id.m.i + 1
                      }
                    id.i <- id.i + 1
                    }
                  }
                }
                results.regression.linear$slab <- stud.id
                
                # print baujat plot
                baujat(results.regression.linear, xlab = paste("Squared Pearson Residual (for ", regression.labels.df[1, moderator], "; linear)", sep = ""), symbol = "slab")
              } else {
                print("no baujat plot due to less than 5 stuides")
              }
            } 

            # gosh plot
            if (print.gosh.regression){
              if (
                is.list(filter.regression.linear.list) &
                if(is.list(filter.regression.linear.list)){filter.regression.linear.list[[mod.no]][1] != ""}else{F}
              ){
                for (outlier in -filter.regression.linear.list[[mod.no]]){

                  results.regression.linear.unfiltered <- get.results.metafor(
                    regression.df, moderator = moderator, results.metafor.fixed = results.metafor.fixed
                  )

                  cat("\n Gosh plot for", moderator, "(outlier number:", outlier, "linear)")
                  sav <- gosh(results.regression.linear.unfiltered, subsets = 15000)
                  plot(sav, out = outlier)
                }
              } else {
                cat("\nno Gosh plot printed for", moderator, "(linear)\n")
              }
            }
            
            if (print.influence){
              plot.influnece(results.regression.linear, influence.ids)
            }
          }
          
  ## Polynomial Regression
          if (regression.degree.2 & moderator %in% moderator.vec[1:4]){
              # only for continuous moderators
            
            # filter polynomial regression data
            if (
              is.list(filter.regression.poly.list) &
              if(is.list(filter.regression.poly.list)){filter.regression.poly.list[[mod.no]][1] != ""}else{F}
            ){
              regression.df.filtered <- regression.df[filter.regression.poly.list[[mod.no]],]
            } else {
              regression.df.filtered <- regression.df
            }
            
            for (modertor.label in colnames(regression.labels.df)){
              if (moderator == modertor.label){
                x.label <- regression.labels.df[1,moderator]
              }
            }
            
            degree <- 2
            
            # get polynomial regression results
            results.regression.polynomial <- get.results.metafor(
              regression.df.filtered, moderator = moderator, degree = degree, results.metafor.fixed = results.metafor.fixed
            )
            
            if (return.data == "regression.results.poly"){
              return.data.var <- results.regression.polynomial
            }
            
            # bubble/regression plot
            if (!moderator %in% c("delivery.mode", "meditation.type") & print.regplot){
              if (nrow(regression.df.filtered) > 4){
                # not figured out how to plot regplot with ggplot yet
                
                # with metafor
                xvals <- seq(-1, max(regression.df.filtered[,moderator]) + 5, length=500)
                sav <- predict(results.regression.polynomial, newmods= unname(poly(xvals, degree=degree, raw=T)))

                regplot(
                  results.regression.polynomial, mod=2, pred=sav, xvals=xvals, refline=0, las=1, digits=1, bty="l",
                  label=regression.label, xlab=x.label,
                  pi = T, grid = T, legend = F
                )
              } else {
                print("polynomial bubble plot was not printed because number of incluided studies is <= 4")
              }
            }
            
            # baujat plot
            if (print.baujat.regression){
              # outlier analysis
              # par(mar = c(5, 6, 4, 2))  # to set plot parameters?
              # plot(influence(results.regression.polynomial), cex = 0.8, las = 1)
              if (nrow(regression.df.filtered) > 4){
                # get study ids
                stud.id <- results.regression.polynomial$data$study.id

                ## get study ids that are present more than once
                stud.id.tab <- table(stud.id)
                stud.id.multi <- names(stud.id.tab[stud.id.tab > 1])

                ## add number to study ids that are present more than once
                if (length(stud.id.multi) >= 1){
                  for (id.multi in stud.id.multi){
                    id.m.i <- 1
                    id.i <- 1
                    for (id in stud.id){
                      if (id %in% id.multi){
                        stud.id[id.i] <- paste(stud.id[id.i], ", ", id.m.i, sep = "")
                        id.m.i <- id.m.i + 1
                      }
                    id.i <- id.i + 1
                    }
                  }
                }
                results.regression.polynomial$slab <- stud.id
                
                # plot baujat plot
                baujat(results.regression.polynomial, xlab = paste("Squared Pearson Residual (for ", regression.labels.df[1, moderator], "; squared)", sep = ""), symbol = "slab")
              } else{
                print("no baujat plot due to less than 5 stuides")
              }
            }
            
            # gosh plot
            if (print.gosh.regression){
              if (
                is.list(filter.regression.poly.list) &
                if(is.list(filter.regression.poly.list)){filter.regression.poly.list[[mod.no]][1] != ""}else{F}
              ){
                for (outlier in -filter.regression.poly.list[[mod.no]]){

                  results.regression.polynomial.unfiltered <- get.results.metafor(
                    regression.df, moderator = moderator, results.metafor.fixed = results.metafor.fixed
                  )

                  cat("\n Gosh plot for", moderator, "(outlier number:", outlier, "polynomial)")
                  sav <- gosh(results.regression.polynomial.unfiltered, subsets = 15000)
                  plot(sav, out = outlier)
                }
              } else {
                cat("\nno Gosh plot printed for", moderator, "(polynomial)\n")
              }
            }
            
            if (print.influence){
              plot.influnece(results.regression.polynomial, influence.ids)
            }
          }
          
          # print regression results
          if (print.regression.results){
            print(moderator)
            print("Linear")
            print(results.regression.linear)
            if (regression.degree.2){
              cat("\n")
              print("Polynomial")
              print(results.regression.polynomial)
            }
          }
        } else {
          print(sprintf('Only equal moderator values present for regression of outcome:"%s" and moderator: "%s"', outcome, moderator))
          cat("\n")
        }
      } else {
        print(sprintf('Number of included studies (%d) in regression is below 4 for outcome "%s" and moderator "%s"', nrow(regression.df), outcome, moderator))
        cat("\n")
      }
      
      # print regression data frame
      if(print.regression.df){
        print(moderator)
        print(regression.df)
      }
      
      mod.no <- mod.no + 1
    }    
  }
  
  if (regression.multiple){
    do.multiple.regressions(  # fixed effects model not implemented
      outcome = outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
      moderator.multiple.list = moderator.multiple.list,
      filter.multiple.regression.linear.list = filter.multiple.regression.linear.list,
      filter.multiple.regression.poly.list = filter.multiple.regression.poly.list,
      print.regplot = print.regplot, print.baujat.regression = print.baujat.regression,
      print.gosh.regression = print.gosh.regression, print.regression.df = print.regression.df,
      regression.label = regression.label, degree.2 = regression.multiple.degree.2,
      non.interaction = non.interaction, interaction = interaction
    )
  }
  
  if (return.data == "correlation.matrix"){
    regression.all.df <- meta.analyze(
      outcome, meditation.type.all, m.data.list, preferred.scale = preferred.scale,  # meta.df.list = meta.df.list,
      split.subgroups = FALSE, return.data = "regression.all", results.metafor.fixed = results.metafor.fixed  #, without.mean.r = without.mean.r
    )
    return.data.var <- get.cor.matrix(regression.all.df)
  }
  
  if (
    !return.data %in% c("meta.df.list", "funnel.asym.p.egger", "funnel.asym.p.rank", "descriptive", "regression.all", "results.meta", "results.metafor", "hedges.g", "influence.df", "regression.results.linear", "regression.results.poly", "correlation.matrix", "power.subgroup") &
    return.data != F
  ){
    print('error in print.meta.results(): set return.data to "meta.df.list", "funnel.asym.p.egger", "funnel.asym.p.rank", "descriptive", "regression.all", "results.meta", "results.metafor", "hedges.g", "influence.df", "regression.results.linear", "regression.results.poly", "correlation.matrix", or "power.subgroup"')
  }
  
  if (return.data != F){
    return(return.data.var)
  }
}
