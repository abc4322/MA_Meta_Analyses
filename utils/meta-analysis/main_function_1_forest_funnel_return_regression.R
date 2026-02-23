# This script includes the first main utility function for meta.analyze() for meta-analyses, which collects data from m.data.list for a desired comparison,
# prints forest and funnel plots for the desired comparison, and returns a filtered single regression data frame (meta-regression was outsourced to print.meta.results function),
# single plots, and other data.

### Plot Forest & Funnel Plots, Return regression data frame
meta.analyze <- function(
# includes...
  # ...collecting data from m.data.list for desired comparison
  # ...printing forest and funnel plots for desired comparison
  # ...returning...
    # ...filtered single regression data frame (meta-regression was outsourced to print.meta.results function)
    # ...single plots
    # ...other data
  
  # basic parameters
  outcome, meditation.types, m.data.list, preferred.scale = F, meta.df.list = F, comparison.list = list(meditation.type.all, cont.passive),
  
  # print plots or data
  print.forest = F, forest.add.fix.eff.mod = T, forest.add.power = F, forest.layout = "RevMan5", print.funnel = F, funnel.label = T, funnel.label.out.only = T, print.meta.results = F,
  print.descriptive = F, print.influence = F, influence.ids = "name", print.baujat = F, report = F,
  
  # regression and subgroup analysis 
  moderator = F, split.subgroups = T, subgroups = c("delivery.mode", "meditation.type"), print.forest.sub.single = F, subgroup.method = "random",
  without.mean.r = F,
  
  # filter and sorting
  filter.forest..funnel.vec = F, filter.regression.vec = F, sort.by = "hedges.g",
  exclude.high.rob.vec = F, comparisons.skip.list = F,
  
  # return
  return.data = F, results.metafor.fixed = F, silent = F
    # meta.df.list is used to increase performace if data already was extracted, otherwise --> F
    # meditation.types = vector of meditation types (not used anymore, but not removed in case of causing any bug)
    # preferred.scale = preferred scale in case of 2 scales for one outcome (Stress)
    # moderator returns regression df instead of printing because latter does not work for some reason
    # filter.forest..funnel.vec (filters basic data frame) and filter.regression.vec (filters regression results) should not be combined
      # filter.forest..funnel.vec works with numbers and study labels (filter out study numbers with e.g. -c(1, 2, 3) and study labels with c("stud1", "stud2", "stud3"))
    # meta.df.list can be inserted from earlier calculations with the same outcome to skip the data collection process
    # print.forest.sub.single = insert categorical moderator to get single results of it (works only for {meta} and with split.subgroups = T)
    # comparisons.skip.list = a list of lists of intervention comparison vectors (e.g. list(list(int.vec.1, int.vec.2), list(int.vec.3, int.vec.3)))
      # ... if just one intervention should be cutted set this intervention to both elements of list (e.g. list(list(int.vec.1, int.vec.1)))
    # set forest.layout = gs("layout") to use custom layout
){
  
  if (length(comparison.list) != 2){
    cat("\nerror in meta.analyze(): parameter comparison.list should have length 2 but has length", length(comparison.list), "instead.\n")
  }
  
  if (if(is.logical(meta.df.list)){meta.df.list == F}else{F}){
    study.id <- c()

  # Set Variables
  ## Descriptive Results
    n.int <- c()
    n.control <- c()
    mean.int <- c()
    mean.control <- c()
    sd.int <- c()
    sd.control <- c()

  ## Meta-Regression Factors
    # for active control groups with regression data, create vectors also for control group
    # and insert them at the function's control group part
    # for this script there are only passive controls that do not have regression data
    sessions.duration <- c()
    sessions.frequency <- c()
    programs.duration <- c()
    delivery.mode <- c()
    follow.up.period <- c()
    meditation.type <- c()
    female.percent <- c()

  ## create list of all meta data vectors to pass it to fill.meta.df()
    # arrays or data frames are not used to be able to append values to vectors separately 
    descr..reg.data.list.t1 <- list(
      study.id, n.int, n.control, mean.int, mean.control, sd.int, sd.control,
      sessions.duration, sessions.frequency, programs.duration, delivery.mode,
      follow.up.period, meditation.type, female.percent
    )

    names(descr..reg.data.list.t1) <- c(
      "study.id", "n.int", "n.control", "mean.int", "mean.control", "sd.int", "sd.control",
      "sessions.duration", "sessions.frequency", "programs.duration", "delivery.mode",
      "follow.up.period", "meditation.type", "female.percent"
    )

    descr..reg.data.list.t2 <- descr..reg.data.list.t1

    descr..reg.data.list.t3 <- descr..reg.data.list.t1

    descr..reg.data.list <- list(descr..reg.data.list.t1, descr..reg.data.list.t2, descr..reg.data.list.t3)

    names(descr..reg.data.list) <- c("t1", "t2", "t3")

    results.descriptive.array <- m.data.list[["results.descriptive.array"]]
    
  #___For other than Stress, Self-compassion, and Positive affect (Outcomes with one scale per outcome per study)___#
    if (preferred.scale == F){
      for (study in 1:study.no){
        # skip study if none of both of the comparison groups are present
        interventions <- meditation.techniques.df[study,]
        if (!(
            T %in% (interventions %in% comparison.list[[1]]) &
            T %in% (interventions %in% comparison.list[[2]])
        )){next}
        
        # skip study if comparison of comparisons.skip.list is present
        if(!is.logical(comparisons.skip.list)){
          comp.skip.present <- F
          for (comparison in 1:length(comparisons.skip.list)){
            if (length(comparisons.skip.list[[comparison]]) != 2){
              cat(
                "\n\n error in meta.analyze(): set parameter comparisons.skip.list to a list of lists of 2 intervention comparison vectors (e.g. list(list(int.vec.1, int.vec.2), list(int.vec.3, int.vec.3))).\n",
                "got ", length(comparisons.skip.list[[comparison]]), " elements in entry ", comparison, " of comparisons.skip.list.", sep = ""
              )
            }
            int.1 <- comparisons.skip.list[[comparison]][[1]]
            int.2 <- comparisons.skip.list[[comparison]][[2]]
            if (
              T %in% (int.1 %in% interventions) &
              T %in% (int.2 %in% interventions)
            ){comp.skip.present <- T}
          }
          if (comp.skip.present) next
        }

        #  find out which are the respective intervention numbers
        intervention.no.vec <- which(interventions %in% comparison.list[[1]])
        control.no.vec <- which(interventions %in% comparison.list[[2]])
        
        for (time.point in 2:4){  # in this case 2 is post-test
          for (outcome.no in 1:7){            
            # iterate overall intervention vs. control combinations
            for (intervention.no in intervention.no.vec){
              for (control.no in control.no.vec){
                # Define boolean variables for checking if 1) wanted outcome is present, 2) intervention name is present, and 3) data is complete
                outcome_name <- m.data.list[["outcome.names.df"]][study, sprintf("Name.of.Outcome.%d", outcome.no)]
                outcome_name_present <- if (is.na(outcome_name)) {F} else {outcome_name == outcome}
                intervention_name_present <- !(
                  is.na(m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"]) |
                  m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] == "NA" |
                  m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] ==
                    nm.placeholder |
                  m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] ==
                    as.character(nm.placeholder)
                )
                data_complete <- !(
                  NA %in% results.descriptive.array[intervention.no,,time.point, outcome.no, "Scale.1", study] |
                  nm.placeholder %in% results.descriptive.array[intervention.no,,time.point, outcome.no, "Scale.1", study] |
                  NA %in% results.descriptive.array[control.no,,time.point, outcome.no, "Scale.1", study] |
                  nm.placeholder %in% results.descriptive.array[control.no,,time.point, outcome.no, "Scale.1", study]
                )
                # Fill descr..reg.data.list if all conditions are met 
                if (
                  outcome_name_present &
                  intervention_name_present &
                  data_complete
                ){
                  descr..reg.data.list <- fill.meta.df(
                    "Scale.1", time.point - 1, intervention.no, control.no, outcome.no, study,
                    m.data.list, nm.placeholder, descr..reg.data.list, without.mean.r
                    # time.point - 1 because descr..reg.data.list starts with pos-test
                  )
                }
              }
            }
          }
        }
      }

  #___For Stress, Self-compassion, and Positive affect (Sometimes 2 scales per study per outcome)___#
    } else if (preferred.scale != F){
      for (study in 1:study.no){
        
        # skip study if none of both of the comparison groups are present
        interventions <- meditation.techniques.df[study,]
        if (!(
            T %in% (interventions %in% comparison.list[[1]]) &
            T %in% (interventions %in% comparison.list[[2]])
        )){next}
        
        # skip study if comparison of comparisons.skip.list is present
        if(!is.logical(comparisons.skip.list)){
          comp.skip.present <- F
          for (comparison in 1:length(comparisons.skip.list)){
            if (length(comparisons.skip.list[[comparison]]) != 2){
              cat(
                "\n\n error in meta.analyze(): set parameter comparisons.skip.list to a list of lists of 2 intervention comparison vectors (e.g. list(list(int.vec.1, int.vec.2), list(int.vec.3, int.vec.3))).\n",
                "got ", length(comparisons.skip.list[[comparison]]), " elements in entry ", comparison, " of comparisons.skip.list.", sep = ""
              )
            }
            int.1 <- comparisons.skip.list[[comparison]][[1]]
            int.2 <- comparisons.skip.list[[comparison]][[2]]
            if (
              T %in% (int.1 %in% interventions) &
              T %in% (int.2 %in% interventions)
            ){comp.skip.present <- T}
          }
          if (comp.skip.present) next
        }

        #  find out which are the respective intervention numbers
        intervention.no.vec <- which(interventions %in% comparison.list[[1]])
        control.no.vec <- which(interventions %in% comparison.list[[2]])
        
        for (time.point in 2:4){  # in this case 2 is post-test
          for (scale in 1:2){
            for (outcome.no in 1:7){
              
  # skip iteration if 2 scales per outcome are present and current scale is not the preferred one
              scale.name <- m.data.list[["outcome.measures.df.list"]][[study]][
                sprintf("Outcome.%d", outcome.no), "Measures.Name"
              ]
              if (!is.na.or.nm(scale.name)){
                if (multiple.commas.present(scale.name)){
                  cat("multiple commas in scale name, find solution:", scale.name, "\n\n")
                  next
                } else if (grepl(",", scale.name)){  # comma = 2 scales per outcome

                  if (scale == 1){
                    scale.name.substring <- sub(",.*", "", scale.name)
                      # extracts substring before comma
                    if (!grepl(gsub("([()])","\\\\\\1", preferred.scale), scale.name.substring)){
                      next
                    }
                  } else if (scale == 2){
                    scale.name.substring <- sub(".*,", "", scale.name)
                      # extracts substring after comma
                    if (!grepl(gsub("([()])","\\\\\\1", preferred.scale), scale.name.substring)){
                      next
                    }
                  }
                }
              }

              # iterate overall intervention vs. control combinations
              for (intervention.no in intervention.no.vec){
                for (control.no in control.no.vec){
                  # Define boolean variables for checking if 1) wanted outcome is present, 2) intervention name is present, and 3) data is complete
                  outcome_name <- m.data.list[["outcome.names.df"]][study, sprintf("Name.of.Outcome.%d", outcome.no)]
                  outcome_name_present <- if (is.na(outcome_name)) {F} else {outcome_name == outcome}
                  intervention_name_present <- !(
                    is.na(m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"]) |
                    m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] == "NA" |
                    m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] ==
                      nm.placeholder |
                    m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] ==
                      as.character(nm.placeholder)
                  )
                  data_complete <- !(
                    NA %in% results.descriptive.array[intervention.no,,time.point, outcome.no, scale, study] |
                    nm.placeholder %in% results.descriptive.array[intervention.no,,time.point, outcome.no, scale, study] |
                    NA %in% results.descriptive.array[control.no,,time.point, outcome.no, scale, study] |
                    nm.placeholder %in% results.descriptive.array[control.no,,time.point, outcome.no, scale, study]
                  )
                  # Fill descr..reg.data.list if all conditions are met
                  if (
                    outcome_name_present &
                    intervention_name_present &
                    data_complete
                  ){
                    descr..reg.data.list <- fill.meta.df(
                      scale, time.point - 1, intervention.no, control.no, outcome.no, study,
                      m.data.list, nm.placeholder, descr..reg.data.list, without.mean.r
                      # time.point - 1 because descr..reg.data.list starts with pos-test
                    )
                  }
                }
              }
            }
          }
        }
      }
    }

  # Create data frame list from descr..reg.data.list
    meta.df.list.unfiltered <- list()

    for (time.point in 1:3){  # here 1 is post-test

      n.control <- descr..reg.data.list[[time.point]][["n.control"]]
      mean.control <- descr..reg.data.list[[time.point]][["mean.control"]]
      sd.control <- descr..reg.data.list[[time.point]][["sd.control"]]
      n.int <- descr..reg.data.list[[time.point]][["n.int"]]
      mean.int <- descr..reg.data.list[[time.point]][["mean.int"]]
      sd.int <- descr..reg.data.list[[time.point]][["sd.int"]]

      sessions.duration <- descr..reg.data.list[[time.point]][["sessions.duration"]]
      sessions.frequency <- descr..reg.data.list[[time.point]][["sessions.frequency"]]
      programs.duration <- descr..reg.data.list[[time.point]][["programs.duration"]]

      delivery.mode <- descr..reg.data.list[[time.point]][["delivery.mode"]]

      follow.up.period <- descr..reg.data.list[[time.point]][["follow.up.period"]]

      meditation.type <- descr..reg.data.list[[time.point]][["meditation.type"]]
      
      female.percent <- descr..reg.data.list[[time.point]][["female.percent"]]

      pooled.sd <- get.pooled.sd(n.e = n.int, sd.e = sd.int, n.c = n.control, sd.c = sd.control)
      mean.diff <- mean.int - mean.control
      hedges.g <- mean.diff / pooled.sd 
      weights <- 1/sqrt(pooled.sd)  # this weight and hedges.g will not be used for caluclations
        # ... weights and SMD calculated by {metafor} and {meta} will be used instead 
      meta.df.list.unfiltered <- append(
        meta.df.list.unfiltered,
        list(data.frame(  # data frame as to be in a list to append it to a list
          study.id = descr..reg.data.list[[time.point]][["study.id"]],
          n.control = n.control,
          mean.control = mean.control,
          sd.control = sd.control,
          n.int = n.int,
          mean.int = mean.int,
          sd.int = sd.int,
          pooled.sd = pooled.sd,
          mean.diff = mean.diff,
          hedges.g = hedges.g,
          weights = weights,
          sessions.duration = sessions.duration,
          sessions.frequency = sessions.frequency,
          programs.duration = programs.duration,
          delivery.mode = delivery.mode,
          follow.up.period = follow.up.period,
          meditation.type = meditation.type,
          meditation.total = if (
            c(sessions.duration, sessions.frequency, programs.duration) %>%
              sapply(is.numeric) %>%
              all()
          ) {
            sessions.duration * sessions.frequency * (programs.duration / 7)
          } else {
            NA_real_
          },
          female.percent = female.percent
        ))
      )

      # sort studies
      if (sort.by == "hedges.g"){
        meta.df.list.unfiltered[[time.point]] <- arrange(meta.df.list.unfiltered[[time.point]], hedges.g)
      } else if (sort.by == "name"){
        meta.df.list.unfiltered[[time.point]] <- arrange(meta.df.list.unfiltered[[time.point]], study.id)
      } else if (sort.by != F){
        cat('\n\nsort by "hedges.g" or "name"\n\n')
      }
    }
    names(meta.df.list.unfiltered) <- c("t1", "t2", "t3")
  } else {
    meta.df.list.unfiltered <- meta.df.list
  }

  # filter study results from posttest
  if (!F %in% exclude.high.rob.vec){
    studies.high.rob.included <- which(meta.df.list.unfiltered[[1]]$study.id %in% studies.high.rob)
    if (length(studies.high.rob.included) > 0){
      meta.df.list[[1]] <- meta.df.list.unfiltered[[1]][
        -studies.high.rob.included,
      ]
    }
  } else {
    meta.df.list <- meta.df.list.unfiltered
  }

  if (!F %in% filter.forest..funnel.vec){
    if (!F %in% exclude.high.rob.vec){
      if (is.character(filter.forest..funnel.vec)){
        meta.df.list[[1]] <- meta.df.list[[1]] |> filter(!study.id %in% filter.forest..funnel.vec)
      } else {
        meta.df.list[[1]] <- meta.df.list[[1]][filter.forest..funnel.vec,]
      }
    } else {
      if (is.character(filter.forest..funnel.vec)){
        meta.df.list[[1]] <- meta.df.list.unfiltered[[1]] |> filter(!study.id %in% filter.forest..funnel.vec)
      } else {
        meta.df.list[[1]] <- meta.df.list.unfiltered[[1]][filter.forest..funnel.vec,]
      }
    }
  } else {
    meta.df.list <- meta.df.list.unfiltered
  }
  
  if (nrow(meta.df.list[[1]]) >= 1) {
    
# get results for forest plot {meta}
    results.meta <- metacont(
      n.e = n.int, mean.e = mean.int, sd.e = sd.int,
      n.c = n.control, mean.c = mean.control, sd.c = sd.control,
      common = forest.add.fix.eff.mod, random = T, studlab = study.id,
      data = meta.df.list[[1]], sm = "SMD"
    )
    
# calculate power
## a post-hoc power analysis seems to be a not usefull as it is a function of the p-value (HOENIG, J. M. & HEISEY, D. M. (2001). The Abuse of Power)
## a way to use it however could be to calculate the post-hoc power and its convidence interval (CI) by using the of the effect size and its CI (Wang, L. L. (2010). Retrospective Statistical Power)
## the latter approach was not implemented so the post-hoc power was not reported within the results
    if (forest.add.power){
      results.meta.cohens.d <- metacont(
      n.e = n.int, mean.e = mean.int, sd.e = sd.int,
      n.c = n.control, mean.c = mean.control, sd.c = sd.control,
        common = T, random = T, studlab = study.id,
        data = meta.df.list[[1]], sm = "SMD", method.smd = "Cohen"
      )

      if (results.meta.cohens.d$TE.random > 0){
        te.random <- results.meta.cohens.d$TE.random
      } else {
        te.random <- -results.meta.cohens.d$TE.random
      }
      I2 <- results.meta.cohens.d$I2
      I2 <- ifelse(is.na(I2), 1, I2)

      power.obj <- power.analysis(
        d = te.random,  # over all effect size (must be positive)
        k = results.meta.cohens.d$k,  # number of included studies
        n1 = mean(results.meta.cohens.d$n.c), n2 = mean(results.meta.cohens.d$n.e),  # mean numbers of control and experimental goup
        heterogeneity = if(I2 <= 0.3){"low"}else if(I2 <= 0.6){"moderate"}else{"high"}
      )

      results.meta$power <- round(100 * power.obj$Power, digits = 2)
    }
    
# Print Forest plot or descriptive or meta-results
    if (print.descriptive){
      print(meta.df.list[[1]])
    }

    if (print.meta.results){
      print(results.meta)
    }

    # set right and left label for scale of SMD
    if (
      outcome.direction.df[
        which(outcome.direction.df[,"Outcome"] == outcome),
        "High.or.low.means.resilient"
      ] == "v"
    ){
      label.left <- "may improve resilience   "
      label.right <- "   may reduce resilience"  
    } else {
      label.left <- "may reduce resilience   "
      label.right <- "   may improve resilience"
    }
    
    if (print.forest){
            
      # create label to show outcome in case of multiple plots in wrong order
      label.add <- ""
      if (preferred.scale != F){
        if (grepl("PSS", preferred.scale)){
          label.add <- " (PSS) "
        } else if (grepl("DASS", preferred.scale)){
          label.add <- " (DASS) "
        } 
      }
      
      if (forest.layout != "RevMan5"){
        forest(
          results.meta,  # leftcols = c('studlab'), <-- would hide descriptive statistics in plot
          layout = forest.layout,
          prediction = T, print.Q = T,
          label.left = label.left, label.right = label.right,
          plotwidth = "5cm",
          smlab = "Standardized Mean\nDifference",  # using Standardized... instad of Standardised.. as heading above plot of SMDs
          # smlab = paste("SMD for\n", outcome, label.add),  # label to show outcome
          hetlab = ifelse(
            forest.add.power,
            paste("Power = ", as.character(results.meta$power), "%, ", sep = ""),
            gs("hetlab")  # default value of hetlab
          ),
          addrows.below.overall = 4,  # rows below SMD scale
          digits.pval.Q = 4,
          digits.I2 = 2,
          digits.sd = 2  # ,  
          # colgap.forest.left = "1cm"  # left space from graph 
        )
      } else {
        forest(
          results.meta,
          common = F,
          layout = forest.layout,
          prediction = T,
          label.left = label.left, label.right = label.right,
        )
      }
    }
    
    # Subgroup Analysis
    if (split.subgroups){
      if (results.meta$k >= 4){
        i <- 1
        for (subgroup in subgroups){
          if (print.forest.sub.single == F){
            cat("\n")
            cat("# Subgroup Analysis:", subgroup, "\n")
          }
          
          # get results for test of subgroup differences with desired method 
          results.meta.sub <- metacont(
            n.e = n.int, mean.e = mean.int, sd.e = sd.int,
            n.c = n.control, mean.c = mean.control, sd.c = sd.control,
            common = T, random = T, studlab = study.id,
            data = meta.df.list[[1]] %>%
              filter(!(
                is.na(!!sym(subgroup)) |
                !!sym(subgroup) %in% c( "NA", nm.placeholder)
              )),
            sm = "SMD"
          )
          if (subgroup.method == "fixed"){  # corresponds to Schwarzer et al. (2015). Meta-Analysis with R. doi: 10.1007/978-3-319-21416-0, pp. 41 - 45 & 89 - 91
            results.meta.sub <- update.meta(
              results.meta.sub, subgroup = eval(parse(text = subgroup)), random = F
                # eval(parse()) makes it possible to pass string as variable
            )
          } else if (subgroup.method == "random"){  # this delivers same results as "random.separate.tau2" but without confidence interval of tau2 (within-group)
            results.meta.sub <- update.meta(
              results.meta.sub, subgroup = eval(parse(text = subgroup)), common = F
            )
          } else if (subgroup.method == "rand.fix"){
            results.meta.sub <- update.meta(
              results.meta.sub, subgroup = eval(parse(text = subgroup))
            )
          } else if (subgroup.method == "random.separate.tau2"){  # corresponds to Schwarzer et al. (2015) pp. 91 - 94
            # get separete results for all levels of subgroups (random effect model)
            sub.lvls <- unique(results.meta$data[, subgroup])
            results.meta.subs <- list()
            TE.del <- c()
            seTE.del <- c()
            i <- 1
            for (sub.lvl in sub.lvls){
              # generate results object for subset ob subgroup level
              sub.res <- metacont(
                n.e = n.int, mean.e = mean.int, sd.e = sd.int,
                n.c = n.control, mean.c = mean.control, sd.c = sd.control,
                common = F, random = T, studlab = study.id,
                data = meta.df.list[[1]], sm = "SMD",
                subset = eval(parse(text = subgroup)) == sub.lvl
              )
              
              results.meta.subs[[i]] <- sub.res
              
              # subgroup treatment effects 
              TE.del[i] <- sub.res$TE.random

              # corresponding standard error
              seTE.del[i] <- sub.res$seTE.random
              
              i <- i + 1
            }

            # meta-analysis of subgroup estimates (fixed effect model)
            results.meta.sub <- metagen(
              TE.del, seTE.del, sm="SMD",
              studlab = sub.lvls,
              random = F, common = T
            )
            
            # add results of subgroups to results object
            results.meta.sub$TE.random.w <- TE.del
            results.meta.sub$seTE.random.w <- seTE.del
            
            results.meta.sub$tau2.w <- c()
            results.meta.sub$lower.tau2.w <- c()
            results.meta.sub$upper.tau2.w <- c()
            results.meta.sub$I2.w <- c()
            results.meta.sub$lower.I2.w <- c()
            results.meta.sub$upper.I2.w <- c()
            results.meta.sub$Q.w <- c()
            results.meta.sub$pval.Q.w <- c()

            results.meta.sub$lower.random.w <- c()
            results.meta.sub$upper.random.w <- c()
            results.meta.sub$zval.random.w <-  c()

            results.meta.sub$k <- 0
            results.meta.sub$k.w <- c()
            results.meta.sub$n.c <- 0
            results.meta.sub$n.e <- 0
            
            for (i in 1:length(sub.lvls)){
              results.meta.sub$tau2.w[i] <- results.meta.subs[[i]]$tau2
              results.meta.sub$lower.tau2.w[i] <- results.meta.subs[[i]]$lower.tau2
              results.meta.sub$upper.tau2.w[i] <- results.meta.subs[[i]]$upper.tau2
              results.meta.sub$I2.w[i] <- results.meta.subs[[i]]$I2
              results.meta.sub$lower.I2.w[i] <- results.meta.subs[[i]]$lower.I2
              results.meta.sub$upper.I2.w[i] <-results.meta.subs[[i]]$upper.I2
              results.meta.sub$Q.w[i] <- results.meta.subs[[i]]$Q
              results.meta.sub$pval.Q.w[i] <- results.meta.subs[[i]]$pval.Q
              
              results.meta.sub$lower.random.w[i] <- results.meta.subs[[i]]$lower.random
              results.meta.sub$upper.random.w[i] <- results.meta.subs[[i]]$upper.random
              results.meta.sub$zval.random.w[i] <-  results.meta.subs[[i]]$zval.random
              
              results.meta.sub$k <- results.meta.sub$k + results.meta.subs[[i]]$k
              results.meta.sub$k.w[i] <- results.meta.subs[[i]]$k
              results.meta.sub$n.c <- results.meta.sub$n.c + sum(results.meta.subs[[i]]$n.c)
              results.meta.sub$n.e <- results.meta.sub$n.e + sum(results.meta.subs[[i]]$n.e)
            }
            
          } else if (subgroup.method == "random.common.tau2"){  # corresponds to Schwarzer et al. (2015) pp. 94 - 97
            results.meta.sub <- update.meta(results.meta, subgroup = eval(parse(text = subgroup)), tau.common = T, common = F, random = T)
          } else {
            print("error in meta.analyze(): set parameter 'subgroup.method' to 'fixed', 'random.separate.tau2' or 'random.common.tau2'")
          }
          
          if (print.forest.sub.single == F){
            print(results.meta.sub)
          }
          
          if (if(print.forest.sub.single != F){subgroup == print.forest.sub.single}else{T}){
            
            # calculate power for delivery.mode (level 1 vs. level 2)
            if (subgroup == "delivery.mode" & (forest.add.power | return.data == "power.subgroup")){
              TE1 <- results.meta.sub$TE.random.w[[1]]
              seTE1 <- results.meta.sub$seTE.random.w[[1]]
              TE2 <- results.meta.sub$TE.random.w[[2]]
              seTE2 <- results.meta.sub$seTE.random.w[[2]]

              power.sub <- 100 * round(
                power.analysis.subgroup(
                  TE1 = TE1, TE2 = TE2, seTE1 = seTE1, seTE2 = seTE2
                )$Power,
                digits = 4
              )
              power.sub.str <- paste("= ", as.character(power.sub), "%", sep = "")
              
              if (return.data == "results.meta"){
                return(results.meta.sub)
              }
              
            # calculate all possible powers of meditation.type (all combinations of level 1:4)
            } else if (subgroup == "meditation.type" & (forest.add.power | return.data == "power.subgroup")){
              # get total effects and standard errors
              TE.vec <- c()
              seTE.vec <- c()
              for (i in 1:4){
                tryCatch(
                  {
                    TE.vec[length(TE.vec) + 1] <- results.meta.sub$TE.random.w[[i]]
                    seTE.vec[length(seTE.vec) + 1] <- results.meta.sub$seTE.random.w[[i]]
                  },
                  error=NULL,
                  warning=NULL
                )
              }
              power.sub <- c()
              
              # calculate power of all combinations
              for (j in 1:4){
                for (k in 1:4){
                  if (k <= j){
                    next
                  }
                  tryCatch(
                    {
                      power.sub[length(power.sub) + 1] <- 100 * round(
                        power.analysis.subgroup(
                          TE1 = TE.vec[j], TE2 = TE.vec[k], seTE1 = seTE.vec[j], seTE2 = seTE.vec[k]
                        )$Power,
                        digits = 4
                      )
                    },
                    error=NULL,
                    warning=NULL
                  )
                }
                
                # show only highest power in plot
                power.sub.str <- paste(
                  ifelse(length(power.sub) > 1, "<= ", "= "),
                  as.character(max(power.sub)), "%", sep = ""
                )
              }
            }
            
            if (return.data == "power.subgroup"){
              return(power.sub)
            } else if (return.data == "results.meta"){
              return(results.meta.sub)
            }
            
            if (forest.layout != "RevMan5"){
              forest(
                results.meta.sub,  # leftcols = c('studlab'),
                layout = forest.layout,
                prediction = T, print.Q = T,
                label.left = label.left, label.right = label.right,
                hetlab = ifelse(
                  forest.add.power,
                  paste(
                    "Power (total, subgroup) = ", as.character(results.meta$power), "%, ", power.sub.str, ", ",
                    sep = ""
                  ),
                  gs("hetlab")  # default value of hetlab
                ),
                smlab = "Standardized Mean\nDifference",
                plotwidth = "6cm", addrows.below.overall = 4,
                # colgap.forest.left = ifelse(forest.add.power, "9cm", "4cm"),
                # squaresize = 1.05,
                digits.pval.Q = 4,
                digits.I2 = 2,
                digits.sd = 2
              )
            } else {
              forest(
                results.meta.sub,
                common = F,
                layout = forest.layout,
                prediction = T,
                label.left = label.left, label.right = label.right,
              )
            }
          }
          i <- i + 1
        }
        
        if (print.forest.sub.single != F & !print.forest.sub.single %in% c("delivery.mode", "meditation.type")){
          print("error in meta.analyze(): set parameter 'print.forest.sub.single' to 'delivery.mode' or 'meditation.type'")
        }
        
      } else {
        cat("\nk < 4 => no Subgroup Analysis\n\n")
      }
    }
    
    # Outlier Analysis (influential outliers present?)
    if(results.meta$k >= 3){
      results.influence.metafor <- get.results.metafor(meta.df.list[[1]], results.metafor.fixed = results.metafor.fixed)
      
      if (print.influence){
        # par(mar = c(5, 6, 4, 2))  # to set plot parameters?
        # plot(influence(results.influence.metafor), cex = 0.8, las = 1)  # using metafor
        plot.influnece(results.influence.metafor, influence.ids)  # self-gernerated (based on {dmetar})
      }
      
      if (print.baujat){
        # results.influence.meta <- InfluenceAnalysis(results.meta, random = T)
        # plot(results.influence.meta, "baujat")  # plot by dmetar
        
        results.influence.metafor$slab <- results.influence.metafor$data$study.id  # set author name and year as study label (instead of numbers)
        baujat(results.influence.metafor, symbol = "slab")  # plot by metafor
      }
      
    } else if ((print.influence | print.baujat) & results.meta$k < 3){
      print("number of included studies is below 3 --> now influence or baujat plot")
    }
    
    if(report){
      results.report <- get.results.metafor(meta.df.list[[1]], results.metafor.fixed = results.metafor.fixed)
      reporter(results.report)
    }
  } else if (!silent) {
    cat("no studies included for outcome:", outcome, "\n\n")
  }

  if (print.funnel & nrow(meta.df.list[[1]]) >= 3){
    
# Funnel plot
    # get results for funnel and meta-regression plot
    results.funnel <- get.results.metafor(meta.df.list[[1]], results.metafor.fixed = results.metafor.fixed)
    
      # method argument is set default to:
      # method="REML" = restricted maximum likelihood estimator (Viechtbauer, 2005; Raudenbush, 2009)
    
    # # with {meta}
    # funnel(results.meta)
    
    # with {metafor}
    # test for relationship between effect sizes and standard error (which implies funnel plot asynnetry) with the Egger's regression test (mixed-effects meta-regression version)
    if(return.data %in% c("funnel.asym.p.egger", "funnel.asym.p.rank")){
      print(regtest(results.funnel))
      print(ranktest(results.funnel))
    }
    
    # use trim and fill metheod to insert potential missing studies due to publication bias
    trim.and.fill <- trimfill(results.funnel)
    
    # create custom labels
    ## set study labels to numbers
    if (funnel.label.out.only){
      if (!is.null(outlier.list[[outcome]]) & T %in% (filter.forest..funnel.vec == F)){
        # set study labels to author and year and only show labels of outlaying studies
        trim.and.fill$slab[1:length(results.funnel$data$study.id)] <- results.funnel$data$study.id
        trim.and.fill$slab[-outlier.list[[outcome]]] <- ""
        funnel(  # with paramter slab adjusted
          trim.and.fill, legend = T,  yaxis="seinv", label = ifelse(funnel.label, T, F),
          level=c(90, 95, 99), shade=c("white", "gray55", "gray75"),
          refline=0, lty = 0, refline2 = results.meta$TE.random, lty2 = 3
            # results.meta$TE.random = overall effect size of the random effects model
        )
      } else {
        # no outliers --> no labels
        funnel(  # without paramter slab adjusted, label set to F
          trim.and.fill, legend = T,  yaxis="seinv", label = F,
          level=c(90, 95, 99), shade=c("white", "gray55", "gray75"),
          refline=0, lty = 0, refline2 = results.meta$TE.random, lty2 = 3
        )
      }
    } else {
      funnel(  # without paramter slab adjusted
        trim.and.fill, legend = T,  yaxis="seinv", label = ifelse(funnel.label, T, F),  # slab = as.character(1:trim.and.fill$k),
          # slab cant be set as for some reason trim.and.fill$k cant be found (only of trimfill fills in studies?)
        level=c(90, 95, 99), shade=c("white", "gray55", "gray75"),
        refline=0, lty = 0, refline2 = results.meta$TE.random, lty2 = 3
      )
    }
    
  } else if (print.funnel & nrow(meta.df.list[[1]]) < 3){
    print(sprintf("Number of included studies (%d) for the funnel plot is below 3 for outcome: %s", nrow(meta.df.list[[1]]), outcome))
    cat("\n")
  }

# Meta-Regression {metafor}
  # only preparing data frames for regression analysis in print.meta.results().
  # (an unknown error occured while doing this in the current function)
  if (moderator %in% c(
    "sessions.duration", "sessions.frequency", "programs.duration", "meditation.total", "delivery.mode",
    "follow.up.period", "meditation.type", "female.percent"
    )
  ){
  # follow.up.period
    if (moderator == "follow.up.period"){
      regression.df.list <- list()
      for (t in 1:3){
        if (nrow(meta.df.list[[t]]) < 1){
          regression.df <- meta.df.list[[t]]
        } else {
          regression.df <- meta.df.list[[t]] %>%
            filter(!(
              is.na(!!sym(moderator)) |
              !!sym(moderator) %in% c("NA", nm.placeholder, as.character(nm.placeholder))
            ))
            # filters out rows in which the moderator's column is NA
        }
        regression.df.list <- append(
          regression.df.list,
          list(regression.df)
        )
      }
      regression.df <- rbind(regression.df.list[[1]], regression.df.list[[2]], regression.df.list[[3]])
        # rbind() appends rows of data frames together
      
  # other moderators    
    } else {
      regression.df <- meta.df.list[[1]] %>%
        filter(!(
          is.na(!!sym(moderator)) |
          !!sym(moderator) %in% c("NA", nm.placeholder, as.character(nm.placeholder))
        ))
    }
    
  # filter
    if (!F %in% filter.regression.vec){
      regression.df <- regression.df[filter.regression.vec,]
    }
    
    if(moderator != F){
      return(regression.df)
    }

    
  } else if (moderator != F) {
    print('set moderator to "follow.up.period", "sessions.duration", "sessions.frequency", "programs.duration", "meditation.total", "delivery.mode", or "female.percent"')
  }

# Return Data
  if (moderator == F & return.data != F){
    if (return.data == "meta.df.list"){
      return(meta.df.list)
    } else if (return.data == "funnel.asym.p.egger"){
      if (nrow(meta.df.list[[1]]) >= 3){
        results.funnel <- get.results.metafor(meta.df.list[[1]], results.metafor.fixed = results.metafor.fixed)
        return(regtest(results.funnel)$pval)
      }
    } else if (return.data == "funnel.asym.p.rank"){
      if (nrow(meta.df.list[[1]]) >= 3){
        results.funnel <- get.results.metafor(meta.df.list[[1]], results.metafor.fixed = results.metafor.fixed)
        return(ranktest(results.funnel)$pval)
      }
    } else if (return.data == "descriptive"){
      return(meta.df.list)
    } else if (return.data == "regression.all"){
      return(rbind(meta.df.list[[1]], meta.df.list[[2]], meta.df.list[[3]]))
    } else if (return.data == "results.meta" & print.forest.sub.single == F){
      if (nrow(meta.df.list[[1]]) >= 1){
        results.meta <- metacont(
          n.e = n.int, mean.e = mean.int, sd.e = sd.int,
          n.c = n.control, mean.c = mean.control, sd.c = sd.control,
          common = T, random = T, studlab = study.id,
          data = meta.df.list[[1]], sm = "SMD"
        )

        if (results.meta$TE.random > 0){
          te.random <- results.meta$TE.random
        } else {
          te.random <- -results.meta$TE.random
        }
        power.obj <- power.analysis(
          d = te.random,  # over all effect size (must be positive)
          k = results.meta$k,  # number of included studies
          n1 = mean(results.meta$n.c), n2 = mean(results.meta$n.e)  # mean numbers of control and experimental goup
        )
        results.meta$power <- round(100 * power.obj$Power, digits = 2)

        return(results.meta)
      } else {
        return(
          data.frame(k = c(0))
        )
      }
    } else if (return.data == "results.meta" & print.forest.sub.single != F){
      # results.meta for print.forest.sub.single != T are returned above
    } else if (return.data == "results.metafor"){
      if (nrow(meta.df.list[[1]]) >= 1){
        results.metafor <- get.results.metafor(meta.df.list[[1]], results.metafor.fixed = results.metafor.fixed)
      } else {
        results.metafor <- NULL
      }
      return(results.metafor)
    } else if (return.data == "hedges.g"){
      return(meta.df.list[[1]][, "hedges.g"])
    } else if (return.data == "influence.df"){
      if (nrow(meta.df.list[[1]]) >= 3){
        results.influence.metafor <- get.results.metafor(meta.df.list[[1]], results.metafor.fixed = results.metafor.fixed)
        return(get.influence.df(results.influence.metafor))
      }
    } else if (return.data == "power.subgroup"){
      # data returned above
    } else if (return.data %in% c("regression.results.linear", "regression.results.poly")){
      # data return will happen in print.meta.results()
    } else {
      print('error in meta.analyze(): set return.data to "meta.df.list", "funnel.asym.p.egger", "funnel.asym.p.rank", "descriptive", "regression.all", "results.meta", "results.metafor", "hedges.g", "influence.df", "regression.results.linear", "regression.results.poly", or "power.subgroup"')
    }
  } else if (moderator != F & return.data != F){
    print("Argument moderator and return.data are != F. Only regression data was returned.")
  }
}
