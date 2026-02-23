# This script defines a utility function that generates Shiny server outputs for meta-analysis outcome visualizations,
# including forest plots, funnel plots, subgroup analyses, outlier comparisons, regression plots with various
# moderators (programs.duration, sessions.duration, sessions.frequency, follow.up.period), and network meta-analysis plots.

### Set outcome outputs for server
# without loop for moderators & incl regression comparison
moderator.vec <- c("programs.duration", "sessions.duration", "sessions.frequency", "follow.up.period")

return.outcome.output <- function(output, outcome.vec, preferred.scale = FALSE){
  moderator.vec <- c("programs.duration", "sessions.duration", "sessions.frequency", "follow.up.period")
  
  picture.size <- 400
  
  for (outcome in outcome.vec){
    # this for regreloop was inteded to set outputs for all outcomes, but the outpunts of all outcomes got set with the plots of the last outcome
    # passing only one outcome as outcome.vec works
    outliers <- outlier.list[[outcome]]
    
    meta.df.list <- meta.df.lists[[outcome]]
    
    meta.df.list.w.o.mean.r <- meta.df.lists.w.o.mean.r[[outcome]]
    
    outcome.alias <- gsub("([()])", "", gsub(" ", "_", outcome))  # because spaces and brackets could cause trouble later on
    
  # Main Results
    output[[paste("forest.", outcome.alias, sep = "")]] <- renderPlot(
      {
        print.meta.results(
          outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
          regression = F, print.forest = T, forest.add.fix.eff.mod = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F  # ,
          # filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
        )
      }
    )
    
    output[[paste("subgroup.delivery.", outcome.alias, sep = "")]] <- renderPlot(
      {
        print.meta.results(
          outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
          regression = F, print.forest = F, forest.add.fix.eff.mod = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "delivery.mode", print.meta.results = F  # ,
          # filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
        )
      },
      height = 600  # adjust height of plot (independently of from the ui frame)
    )

    output[[paste("subgroup.type.", outcome.alias, sep = "")]] <- renderPlot(
      {
        print.meta.results(
          outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
          regression = F, print.forest = F, forest.add.fix.eff.mod = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "meditation.type", print.meta.results = F  # ,
          # filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
        )
      },
      height = 600
    )

    output[[paste("funnel.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        regression = F, print.forest = F, print.funnel = T, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
        funnel.label = F  # ,
        # filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
      )
    })


  # Main Results comparison with/without outliers
    output[[paste("baujat.", outcome.alias, ".forest.comp", sep = "")]] <- renderPlot({  # n.o. = no outliers (included)
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = T, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F
      )
    })
    
    output[[paste("influence.", outcome.alias, ".forest", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        regression = F, print.forest = F, print.funnel = F, print.influence = T, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F
      )
    })
    
    output[[paste("influence.df.", outcome.alias, ".forest", sep = "")]] <- renderTable({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
        return.data = "influence.df"
      )
    })

    output[[paste("forest.o.", outcome.alias, sep = "")]] <- renderPlot({  # o. = (with) outliers
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        regression = F, print.forest = T, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F
      )
    })

    output[[paste("forest.n.o.comp", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        regression = F, print.forest = T, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
        filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
      )
    })
    
    output[[paste("subgroup.delivery.", outcome.alias, ".o.", sep = "")]] <- renderPlot(
      {
        print.meta.results(
          outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
          regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "delivery.mode", print.meta.results = F
        )
      },
      height = 600  # adjust height of plot (independently of from the ui frame)
    )

    output[[paste("subgroup.delivery.", outcome.alias, ".n.o.comp", sep = "")]] <- renderPlot(
      {
        print.meta.results(
          outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
          regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "delivery.mode", print.meta.results = F,
          filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
        )
      },
      height = 600  # adjust height of plot (independently of from the ui frame)
    )
    
    output[[paste("subgroup.type.", outcome.alias, ".o.", sep = "")]] <- renderPlot(
      {
        print.meta.results(
          outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
          regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "meditation.type", print.meta.results = F
        )
      },
      height = 600  # adjust height of plot (independently of from the ui frame)
    )

    output[[paste("subgroup.type.", outcome.alias, ".n.o.comp", sep = "")]] <- renderPlot(
      {
        print.meta.results(
          outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
          regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "meditation.type", print.meta.results = F,
          filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
        )
      },
      height = 600  # adjust height of plot (independently of from the ui frame)
    )

    output[[paste("funnel.o.", outcome.alias, sep = "")]] <- renderPlot({  # o. = (with) outliers
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        regression = F, print.forest = F, print.funnel = T, funnel.label.out.only = T, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F
      )
    })

    output[[paste("funnel.n.o.comp", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        regression = F, print.forest = F, print.funnel = T, funnel.label.out.only = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
        filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
      )
    })

    output[[paste("gosh.1.", outcome.alias, ".forest.comp", sep = "")]] <- renderImage({
      list(
        src = paste(gsub("/", "\\\\", getwd()), "\\", "Gosh Plots", "\\", "Gosh.Plot.", outcome, ".outlier.1", ".png", sep = ""),
        contentType = 'image/png',
        width = picture.size,
        height = picture.size
      )
    }, deleteFile = F)

    output[[paste("gosh.2.", outcome.alias, ".forest.comp", sep = "")]] <- renderImage({
      list(
        src = paste(gsub("/", "\\\\", getwd()), "\\", "Gosh Plots", "\\", "Gosh.Plot.", outcome, ".outlier.2", ".png", sep = ""),
        contentType = 'image/png',
        width = picture.size,
        height = picture.size
      )
    }, deleteFile = F)

    output[[paste("gosh.3.", outcome.alias, ".forest.comp", sep = "")]] <- renderImage({
      list(
        src = paste(gsub("/", "\\\\", getwd()), "\\", "Gosh Plots", "\\", "Gosh.Plot.", outcome, ".outlier.3", ".png", sep = ""),
        contentType = 'image/png',
        width = picture.size,
        height = picture.size
      )
    }, deleteFile = F)
    
    output[[paste("sens.df.rand.fix.", outcome.alias, sep = "")]] <- renderTable(
      get.sens.anal.df(outcome, "rand.fix", ifelse(outcome == "Stress", "DASS", F))
    )
    
# Regression
## with mean ranges
## =============================================================================================================  
    
    output[[paste("cor.matrix.", outcome.alias, sep = "")]] <- renderTable({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, regression = F, return.data = "correlation.matrix"
      )
    })
    
    ## "programs.duration"
    ## =============================================================================================================
    output[[paste("programs.duration", ".baujat", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("programs.duration", ".influence", outcome.alias, sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, return.data = "regression.results.linear"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("programs.duration", ".baujat.sq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("programs.duration", ".influence.sq", outcome.alias, sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T, return.data = "regression.results.poly"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("programs.duration", ".qq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, print.qq.norm = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("programs.duration", ".lin", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = F  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("programs.duration", ".sq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = F  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("programs.duration", ".lin.w.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("programs.duration", ".sq.w.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("programs.duration", ".lin.n.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T,
        filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("programs.duration", ".sq.n.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T,
        filter.regression.poly.list = if(length(outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]])}
      )
    })
    
    ## "sessions.duration"
    ## =============================================================================================================
    output[[paste("sessions.duration", ".baujat", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.duration", ".influence", outcome.alias, sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, return.data = "regression.results.linear"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("sessions.duration", ".baujat.sq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.duration", ".influence.sq", outcome.alias, sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T, return.data = "regression.results.poly"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("sessions.duration", ".qq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, print.qq.norm = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.duration", ".lin", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = F  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.duration", ".sq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = F  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.duration", ".lin.w.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.duration", ".sq.w.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.duration", ".lin.n.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T,
        filter.regression.linear.list = if(length(outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.duration", ".sq.n.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T,
        filter.regression.poly.list = if(length(outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]])}
      )
    })
    
    ## "sessions.frequency"
    ## =============================================================================================================
    output[[paste("sessions.frequency", ".baujat", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.frequency", ".influence", outcome.alias, sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, return.data = "regression.results.linear"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("sessions.frequency", ".baujat.sq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.frequency", ".influence.sq", outcome.alias, sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T, return.data = "regression.results.poly"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("sessions.frequency", ".qq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, print.qq.norm = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.frequency", ".lin", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = F  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.frequency", ".sq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = F  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.frequency", ".lin.w.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.frequency", ".sq.w.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.frequency", ".lin.n.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T,
        filter.regression.linear.list = if(length(outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.frequency", ".sq.n.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T,
        filter.regression.poly.list = if(length(outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]])}
      )
    })
    
    ## "follow.up.period"
    ## =============================================================================================================
    output[[paste("follow.up.period", ".baujat", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("follow.up.period", ".influence", outcome.alias, sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, return.data = "regression.results.linear"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("follow.up.period", ".baujat.sq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("follow.up.period", ".influence.sq", outcome.alias, sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T, return.data = "regression.results.poly"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("follow.up.period", ".qq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, print.qq.norm = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("follow.up.period", ".lin", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = F  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("follow.up.period", ".sq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = F  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("follow.up.period", ".lin.w.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("follow.up.period", ".sq.w.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("follow.up.period", ".lin.n.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T,
        filter.regression.linear.list = if(length(outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("follow.up.period", ".sq.n.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T,
        filter.regression.poly.list = if(length(outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]])}
      )
    })
    
## without mean ranges
## =============================================================================================================  
    
    output[[paste("cor.matrix.", outcome.alias, "w.o.m.r", sep = "")]] <- renderTable({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, regression = F, return.data = "correlation.matrix"
      )
    })
    
    ## "programs.duration"
    ## =============================================================================================================
    output[[paste("programs.duration", ".baujat", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("programs.duration"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("programs.duration", ".influence", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("programs.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, return.data = "regression.results.linear"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("programs.duration", ".baujat.sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("programs.duration"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("programs.duration", ".influence.sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("programs.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T, return.data = "regression.results.poly"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("programs.duration", ".qq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, print.qq.norm = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("programs.duration", ".lin", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("programs.duration", ".sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("programs.duration", ".lin.w.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("programs.duration", ".sq.w.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("programs.duration", ".lin.n.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T,
        filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("programs.duration", ".sq.n.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T,
        filter.regression.poly.list = if(length(outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]])}
      )
    })
    
    ## "sessions.duration"
    ## =============================================================================================================
    output[[paste("sessions.duration", ".baujat", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.duration"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.duration", ".influence", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, return.data = "regression.results.linear"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("sessions.duration", ".baujat.sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.duration"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.duration", ".influence.sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T, return.data = "regression.results.poly"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("sessions.duration", ".qq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, print.qq.norm = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.duration", ".lin", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.duration", ".sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.duration", ".lin.w.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.duration", ".sq.w.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.duration", ".lin.n.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T,
        filter.regression.linear.list = if(length(outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.duration", ".sq.n.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T,
        filter.regression.poly.list = if(length(outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]])}
      )
    })
    
    ## "sessions.frequency"
    ## =============================================================================================================
    output[[paste("sessions.frequency", ".baujat", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.frequency"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.frequency", ".influence", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.frequency"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, return.data = "regression.results.linear"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("sessions.frequency", ".baujat.sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.frequency"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.frequency", ".influence.sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.frequency"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T, return.data = "regression.results.poly"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("sessions.frequency", ".qq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, print.qq.norm = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.frequency", ".lin", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.frequency", ".sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.frequency", ".lin.w.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.frequency", ".sq.w.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.frequency", ".lin.n.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T,
        filter.regression.linear.list = if(length(outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.frequency", ".sq.n.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T,
        filter.regression.poly.list = if(length(outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]])}
      )
    })
    
    ## "follow.up.period"
    ## =============================================================================================================
    output[[paste("follow.up.period", ".baujat", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("follow.up.period"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("follow.up.period", ".influence", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("follow.up.period"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, return.data = "regression.results.linear"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("follow.up.period", ".baujat.sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("follow.up.period"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("follow.up.period", ".influence.sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("follow.up.period"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T, return.data = "regression.results.poly"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("follow.up.period", ".qq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, print.qq.norm = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("follow.up.period", ".lin", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("follow.up.period", ".sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("follow.up.period", ".lin.w.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("follow.up.period", ".sq.w.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("follow.up.period", ".lin.n.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T,
        filter.regression.linear.list = if(length(outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("follow.up.period", ".sq.n.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T,
        filter.regression.poly.list = if(length(outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]])}
      )
    })
    
# Network Meta-Analysis
    output[[paste("network.", outcome.alias, sep = "")]] <- renderPlot({
      net.meta.analyze(
        outcome, preferred.scale = preferred.scale, net.df = F, net.res = net.ress[[outcome]],
        plot.netgraph = T, plot.forest = F, plot.direct.evidence = F, plot.netheat = F,
        return.data = F, reference.group = "passive control", random = T
      )
    })
    
    output[[paste("net.forest.", outcome.alias, sep = "")]] <- renderPlot({
      net.meta.analyze(
        outcome, preferred.scale = preferred.scale, net.df = F, net.res = net.ress[[outcome]],
        plot.netgraph = F, plot.forest = T, plot.direct.evidence = F, plot.netheat = F,
        return.data = F, reference.group = "passive control", random = T
      )
    })
    
    output[[paste("net.dir.evidence.", outcome.alias, sep = "")]] <- renderPlot({
      net.meta.analyze(
        outcome, preferred.scale = preferred.scale, net.df = F, net.res = net.ress[[outcome]],
        plot.netgraph = F, plot.forest = F, plot.direct.evidence = T, plot.netheat = F,
        return.data = F, reference.group = "passive control", random = T
      )
    })
    
    output[[paste("net.heat.", outcome.alias, sep = "")]] <- renderPlot({
      net.meta.analyze(
        outcome, preferred.scale = preferred.scale, net.df = F, net.res = net.ress[[outcome]],
        plot.netgraph = F, plot.forest = F, plot.direct.evidence = F, plot.netheat = T,
        return.data = F, reference.group = "passive control", random = T
      )
    })
    
    try( 
      output[[paste("net.p.df.", outcome.alias, sep = "")]] <- renderTable(
        data.frame(
          net.meta.analyze(
            outcome, preferred.scale = preferred.scale, net.df = F, net.res = net.ress[[outcome]],
            plot.netgraph = F, plot.forest = F, plot.direct.evidence = F, plot.netheat = F,
            return.data = "net.res", reference.group = "passive control", random = T
          )$pval.random
        ),
        digits = 4
      ),
      silent = T
    )
    
  }
  return(output)
}


# # with loop for moderators (error: shows only follow.up period plot for every moderator yet)
# moderator.vec <- c("programs.duration", "sessions.duration", "sessions.frequency", "follow.up.period")

# return.outcome.output <- function(output, outcome.vec, preferred.scale = FALSE){
#   moderator.vec <- c("programs.duration", "sessions.duration", "sessions.frequency", "follow.up.period")
  
#   picture.size <- 400
  
#   for (outcome in outcome.vec){
#     # this for loop was inteded to set outputs for all outcomes, but the outpunts of all outcomes got set with the plots of the last outcome
#     # passing only one outcome as outcome.vec works
#     outliers <- outlier.list[[outcome]]
    
#     meta.df.list <- print.meta.results(
#       outcome, preferred.scale = preferred.scale,
#       regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
#       return.data = "meta.df.list"
#     )
    
#     outcome.alias <- gsub("([()])", "", gsub(" ", "_", outcome))  # because spaces and brackets could cause trouble later on
    
#   # Main Results
#     output[[paste("forest.", outcome.alias, sep = "")]] <- renderPlot(
#       {
#         print.meta.results(
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           regression = F, print.forest = T, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F  # ,
#           # filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
#         )
#       }
#     )
    
#     output[[paste("subgroup.delivery.", outcome.alias, sep = "")]] <- renderPlot(
#       {
#         print.meta.results(
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "delivery.mode", print.meta.results = F  # ,
#           # filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
#         )
#       },
#       height = 600  # adjust height of plot (independently of from the ui frame)
#     )

#     output[[paste("subgroup.type.", outcome.alias, sep = "")]] <- renderPlot(
#       {
#         print.meta.results(
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "meditation.type", print.meta.results = F  # ,
#           # filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
#         )
#       },
#       height = 600
#     )

#     output[[paste("funnel.", outcome.alias, sep = "")]] <- renderPlot({
#       print.meta.results(
#         outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#         regression = F, print.forest = F, print.funnel = T, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
#         funnel.label = F  # ,
#         # filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
#       )
#     })


#   # Main Results comparison with/without outliers
#     output[[paste("baujat.", outcome.alias, ".forest.comp", sep = "")]] <- renderPlot({  # n.o. = no outliers (included)
#       print.meta.results(
#         outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#         regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = T, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F
#       )
#     })
    
#     output[[paste("influence.", outcome.alias, ".forest", sep = "")]] <- renderPlot({
#       print.meta.results(
#         outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#         regression = F, print.forest = F, print.funnel = F, print.influence = T, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F
#       )
#     })
    
#     output[[paste("influence.df.", outcome.alias, ".forest", sep = "")]] <- renderTable({
#       print.meta.results(
#         outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#         regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
#         return.data = "influence.df"
#       )
#     })

#     output[[paste("forest.o.", outcome.alias, sep = "")]] <- renderPlot({  # o. = (with) outliers
#       print.meta.results(
#         outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#         regression = F, print.forest = T, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F
#       )
#     })

#     output[[paste("forest.n.o.comp", outcome.alias, sep = "")]] <- renderPlot({
#       print.meta.results(
#         outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#         regression = F, print.forest = T, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
#         filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
#       )
#     })
    
#     output[[paste("subgroup.delivery.", outcome.alias, ".o.", sep = "")]] <- renderPlot(
#       {
#         print.meta.results(
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "delivery.mode", print.meta.results = F
#         )
#       },
#       height = 600  # adjust height of plot (independently of from the ui frame)
#     )

#     output[[paste("subgroup.delivery.", outcome.alias, ".n.o.comp", sep = "")]] <- renderPlot(
#       {
#         print.meta.results(
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "delivery.mode", print.meta.results = F,
#           filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
#         )
#       },
#       height = 600  # adjust height of plot (independently of from the ui frame)
#     )
    
#     output[[paste("subgroup.type.", outcome.alias, ".o.", sep = "")]] <- renderPlot(
#       {
#         print.meta.results(
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "meditation.type", print.meta.results = F
#         )
#       },
#       height = 600  # adjust height of plot (independently of from the ui frame)
#     )

#     output[[paste("subgroup.type.", outcome.alias, ".n.o.comp", sep = "")]] <- renderPlot(
#       {
#         print.meta.results(
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "meditation.type", print.meta.results = F,
#           filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
#         )
#       },
#       height = 600  # adjust height of plot (independently of from the ui frame)
#     )

#     output[[paste("funnel.o.", outcome.alias, sep = "")]] <- renderPlot({  # o. = (with) outliers
#       print.meta.results(
#         outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#         regression = F, print.forest = F, print.funnel = T, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F
#       )
#     })

#     output[[paste("funnel.n.o.comp", outcome.alias, sep = "")]] <- renderPlot({
#       print.meta.results(
#         outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#         regression = F, print.forest = F, print.funnel = T, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
#         filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
#       )
#     })

#     output[[paste("gosh.1.", outcome.alias, ".forest.comp", sep = "")]] <- renderImage({
#       list(
#         src = paste(gsub("/", "\\\\", getwd()), "\\", "Gosh Plots", "\\", "Gosh.Plot.", outcome, ".outlier.1", ".png", sep = ""),
#         contentType = 'image/png',
#         width = picture.size,
#         height = picture.size
#       )
#     }, deleteFile = F)

#     output[[paste("gosh.2.", outcome.alias, ".forest.comp", sep = "")]] <- renderImage({
#       list(
#         src = paste(gsub("/", "\\\\", getwd()), "\\", "Gosh Plots", "\\", "Gosh.Plot.", outcome, ".outlier.2", ".png", sep = ""),
#         contentType = 'image/png',
#         width = picture.size,
#         height = picture.size
#       )
#     }, deleteFile = F)

#     output[[paste("gosh.3.", outcome.alias, ".forest.comp", sep = "")]] <- renderImage({
#       list(
#         src = paste(gsub("/", "\\\\", getwd()), "\\", "Gosh Plots", "\\", "Gosh.Plot.", outcome, ".outlier.3", ".png", sep = ""),
#         contentType = 'image/png',
#         width = picture.size,
#         height = picture.size
#       )
#     }, deleteFile = F)
    
# # Regression
#     for (moderator in moderator.vec){
#       output[[paste(moderator, ".lin", outcome.alias, sep = "")]] <- renderPlot({
#         print.meta.results(
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           basic = F, moderator.vec = c(moderator), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
#           regression.label = T  # ,
#           # filter.regression.linear.list = if(length(outlier.list[[paste(moderator, ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste(moderator, ".lin", sep = "")]][[outcome]])}
#         )
#       })
      
#       output[[paste(moderator, ".sq", outcome.alias, sep = "")]] <- renderPlot({
#         print.meta.results( 
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           basic = F, moderator.vec = c(moderator), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
#           regression.label = T  # ,
#           # filter.regression.poly.list = if(length(outlier.list[[paste(moderator, ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste(moderator, ".sq", sep = "")]][[outcome]])}
#         )
#       })
      
#       output[[paste(moderator, ".lin.w.o.", outcome.alias, sep = "")]] <- renderPlot({
#         print.meta.results(
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           basic = F, moderator.vec = c(moderator), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
#           regression.label = T  # ,
#           # filter.regression.linear.list = if(length(outlier.list[[paste(moderator, ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste(moderator, ".lin", sep = "")]][[outcome]])}
#         )
#       })
      
#       output[[paste(moderator, ".sq.w.o.", outcome.alias, sep = "")]] <- renderPlot({
#         print.meta.results( 
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           basic = F, moderator.vec = c(moderator), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
#           regression.label = T  # ,
#           # filter.regression.poly.list = if(length(outlier.list[[paste(moderator, ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste(moderator, ".sq", sep = "")]][[outcome]])}
#         )
#       })
      
#       output[[paste(moderator, ".lin.n.o.", outcome.alias, sep = "")]] <- renderPlot({
#         print.meta.results(
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           basic = F, moderator.vec = c(moderator), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
#           regression.label = T,
#           filter.regression.linear.list = if(length(outlier.list[[paste(moderator, ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste(moderator, ".lin", sep = "")]][[outcome]])}
#         )
#       })
      
#       output[[paste(moderator, ".sq.n.o.", outcome.alias, sep = "")]] <- renderPlot({
#         print.meta.results( 
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           basic = F, moderator.vec = c(moderator), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
#           regression.label = T,
#           filter.regression.poly.list = if(length(outlier.list[[paste(moderator, ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste(moderator, ".sq", sep = "")]][[outcome]])}
#         )
#       })
#     }
#   }
#   return(output)
# }
