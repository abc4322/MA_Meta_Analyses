# This script includes main utility functions for meta-analyses, which include overall and network meta-analysis methods (see plot.summary.forest() and net.meta.analyze())

### Get global data frame of all important data to calculate overall SMD from all outcomes
get.global.df <- function(exclude.outcome.vec = c(), comparison.list = list(meditation.type.all, cont.passive), filter.forest..funnel.vec = F){
  
  # gernerate empty df with columns of meta.df.list[[1]] + an outcome column
  global.df <- data.frame(matrix(ncol = 20, nrow = 0))
  colnames(global.df) <- c(
    "study.id", "n.control", "mean.control", "sd.control", "n.int", "mean.int", "sd.int", "pooled.sd", "mean.diff", "hedges.g",
    "weights", "sessions.duration", "sessions.frequency", "programs.duration", "delivery.mode", "follow.up.period", "meditation.type",
    "meditation.total", "female.percent", "outcome"
  )
  
  for (outcome in present.outcomes.sorted){
    if (outcome %in% exclude.outcome.vec){
      next
    }
    # get data of post-test per outcome
    outcome.df <- meta.analyze(
      outcome, meditation.type.all, m.data.list, preferred.scale = get.1st.preferred.scale(outcome),
      comparison.list = comparison.list, filter.forest..funnel.vec = filter.forest..funnel.vec,
      split.subgroups = F, return.data = "meta.df.list" # ,
      # filter.forest..funnel.vec = - outlier.list[["Stress"]]
    )[[1]]
    
    if (nrow(outcome.df) > 0){
      # add column of outcome
      outcome.df$outcome <- outcome

      # append to global.df
      global.df <- rbind(global.df, outcome.df)
    }
  }
  global.df
}


### Get overall results
get.overall.res.metafor <- function(
  subset = NULL, exclude.outcome.vec = c(), return.data = "results.metafor", mods = F, comparison.list = list(meditation.type.all, cont.passive),
  filter.forest..funnel.vec = F
){
  # get df of all studies' data
  global.df <- get.global.df(exclude.outcome.vec, comparison.list = comparison.list, filter.forest..funnel.vec = filter.forest..funnel.vec)

  # append SMDs and sampling variances
  if (nrow(global.df) > 0){

    res.escalc <- escalc(
      data = global.df, measure = "SMD",
      m1i = mean.int, sd1i = sd.int, n1i = n.int,
      m2i = mean.control, sd2i = sd.control, n2i = n.control
    )

    # add id
    res.escalc <- res.escalc |> mutate(id = 1:nrow(res.escalc))

    # adjust polarity
    outcomes.neg.pol.df <- outcome.direction.df |> filter(High.or.low.means.resilient == "v")
    outcomes.neg.pol.vec <- outcomes.neg.pol.df$Outcome
    outcomes.neg.pol.vec
    pol.correct <- res.escalc |>
      filter(outcome %in% outcomes.neg.pol.vec) |>
      mutate(yi = yi * -1)

    res.escalc[res.escalc$id %in% pol.correct$id,] <- pol.correct

    # arrarnge by SMD
    res.escalc <- res.escalc |> arrange(yi)

    # reset id
    res.escalc <- res.escalc |> mutate(id = 1:nrow(res.escalc))

    if (!is.null(subset)){
      t.stud.ids <- 1:nrow(res.escalc)
      t.stud.ids <- t.stud.ids[subset]
      res.escalc <- res.escalc |>  filter(id %in% t.stud.ids)
    }
  
    if (return.data == "results.metafor"){
      if (mods == F){
        # get results (multi-variate if more than one study is onluded otherwise univariate)
        if (nrow(global.df) > 1){
          return(
            rma.mv(yi, vi, random = ~ 1 | outcome/study.id, data=res.escalc, test = "t", slab = outcome)
          )
        } else {
          return(rma.uni(yi, vi, measure="SMD", data = res.escalc))
        }
      } else {
        return(
          rma.mv(yi, vi, random = ~ 1 | outcome/study.id, data=res.escalc, test = "t", slab = outcome, mods = formula(paste("~", mods, "-1")))
        )
      }
    } else if (return.data == "results.escalc"){
      return(res.escalc)
    } else if (return.data != F){
      print("error in get.overall.res.metafor(): set parameter return.data to 'results.metafor' or 'results.escalc'")
    }
  } else {
    if (return.data == "results.metafor"){
      return(data.frame(k = 0))
    } else if (return.data == "results.escalc"){
      return(NULL)
    } else if (return.data != F){
      print("error in get.overall.res.metafor(): set parameter return.data to 'results.metafor' or 'results.escalc'")
    }
  }
}


### Plot Summary Forest Plot
# using random effect models
# fonts adjusted for plot size  fonts adjusted for plot size options(repr.plot.width = 25, repr.plot.height = 9, repr.plot.res = 350)
plot.summary.forest <- function(
  net.res.all = F,
  with.outliers = T,
  overall.method = "rma.mv",
  overall.measure = "net",
  outcome_vec = present.outcomes,
  title = "Summary Forest Plot",
  no.participants.df.n.total.imputed_ = no.participants.df.n.total.imputed,
  study.names.suff.data_ = study.names.suff.data
) {
  # parameter with.outliers is not implemented for the overall results of the network meta-analysis model 
  # get total effect sizes and confidence intervals of all outcomes
  outcomes <- c()
  k <- c()
  o <- c()  # total observations
  o.i <- c()  # observations in intervention
  o.c <- c()  # observations in control
  te <- c()
  ci.l <- c()
  ci.u <- c()
  I2 <- c()
  pow <- c()
  mean.int <- c()
  mean.control <- c()
  seTE.random <- c()
  
  present.outcomes.ordered.te <- outcome_vec
  
  for (outcome in present.outcomes.ordered.te){
    results.meta <- meta.analyze(
      outcome = outcome, meditation.types = meditation.type.all, m.data.list = m.data.list,
      return.data = "results.meta", preferred.scale = get.1st.preferred.scale(outcome), split.subgroups = FALSE,
      filter.forest..funnel.vec = if(length(outlier.list[[outcome]]) > 0 & !with.outliers){-outlier.list[[outcome]]}else{FALSE}
    )
    if (results.meta$k > 0){
      outcomes <- append(outcomes, outcome)
      k <- append(k, results.meta$k)
      o.i <- append(o.i, sum(results.meta$data[, "n.int"]))
      o.c <- append(o.c, sum(results.meta$data[, "n.control"]))
      o <- append(
        o, sum(results.meta$data[, "n.int"]) + sum(results.meta$data[, "n.control"])
      )
      te  <- append(te, results.meta$TE.random)
      ci.l  <- append(ci.l, results.meta$lower.random)
      ci.u <- append(ci.u, results.meta$upper.random)
      I2 <- append(I2, results.meta$I2)
      pow <- append(pow, results.meta$power)
      seTE.random <- append(seTE.random, results.meta$seTE.random)
    }
  }

  df.sum <- data.frame(
    outcomes = outcomes,
    k = k,
    o.i = o.i,
    o.c = o.c,
    o = o,
    te = te,
    ci.l = ci.l,
    ci.u = ci.u,
    I2 = I2,
    pow = pow,
    seTE.random = seTE.random
  )

  # correct scale  directions
  for (outcome in df.sum[,"outcomes"]){
    if (outcome.direction.df[outcome.direction.df[,"Outcome"] == outcome, "High.or.low.means.resilient"] == "v"){
      df.sum[df.sum[,"outcomes"] == outcome, c("te", "ci.l", "ci.u")] <- -1 * df.sum[df.sum[,"outcomes"] == outcome, c("te", "ci.u", "ci.l")]
    }
  }
  
  
  
  # insert overall metrics
  if (overall.measure == "net"){
    
    overall.name <- "Overall (network meta-analysis model)"

    # Get study names of studies including the outcomes
    study.names.of.outcomes <- outcome.names.df %>%
      filter(if_any(everything(), ~ . %in% outcome_vec)) %>%
      row.names()

    # Define number of participants
    n_participants_overall <- no.participants.df.n.total.imputed_ %>%
      # Select only columns of studies with sufficient data
      select(all_of(study.names.suff.data_)) %>%
      # Select only rows of studies including the outcomes
      select(any_of(study.names.of.outcomes)) %>%
      # Filter for sum row
      filter(row.names(.) == "Sum") %>% 
      sum()

    # sum(no.participants.df.n.total.imputed_["Sum", study.names.suff.data])
    
    if (is.logical(net.res.all)){
      print("error in plot.summary.forest(): set net.res.all to netmeta results object of network meta-analysis while overall.measure = 'net'")
    }
    
    df.overall <- data.frame(
      outcomes = overall.name,
      k = c(sum(net.res.all$k)),
      o.i = c(NA),
      o.c = c(NA),
      o = c(n_participants_overall),
      te = c(net.res.all$TE.random["meditation (exclusive)", "passive control"]),
      ci.l = c(net.res.all$lower.random["meditation (exclusive)", "passive control"]),
      ci.u = c(net.res.all$upper.random["meditation (exclusive)", "passive control"]),
      I2 = c(net.res.all$I2),
      pow = c(NA),
      seTE.random = c(net.res.all$seTE.random["meditation (exclusive)", "passive control"])
    )
  } else if (overall.measure == "pairwise"){
    if (overall.method == "rma.mv"){
      
      overall.name <- "Overall (multi-variate model)"
      
      # get results
      res.overall.mv <- get.overall.res.metafor()

      # generate one row df with metrics of interest
      df.overall <- data.frame(
        outcomes = overall.name,
        k = c(sum(df.sum$k)),
        o.i = c(sum(df.sum$o.i)),
        o.c = c(sum(df.sum$o.c)),
        o = c(sum(df.sum$o)),
        te = c(res.overall.mv$b[1, 1]),
        ci.l = c(res.overall.mv$ci.lb),
        ci.u = c(res.overall.mv$ci.ub),
        I2 = c(max(df.sum$I2, na.rm = T)),  # to make it red in plot (will be set to "NA" in table) 
        pow = c(NA),
        seTE.random = c(NA)
      )

    } else if (overall.method == "metamean"){
      # bad approach | better: with metafor::rma.mv(); see https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/multilevel-ma.html#multilevel-R
      res.overall.meta <- metamean(n = df.sum$o, mean = df.sum$te, sd = df.sum$seTE.random * sqrt(df.sum$o), studlab = df.sum$outcomes)
      df.overall <- data.frame(
        outcomes = overall.name,
        k = c(sum(df.sum$k)),
        o.i = c(sum(df.sum$o.i)),
        o.c = c(sum(df.sum$o.c)),
        o = c(sum(df.sum$o)),
        te = c(res.overall.meta$TE.random),
        ci.l = c(res.overall.meta$lower.random),
        ci.u = c(res.overall.meta$upper.random),
        I2 = c(res.overall.meta$I2),
        pow = c(NA),
        seTE.random = c(NA)
      )
    } else {
      print("error in plot.summary.forest(): set overall.method to 'rma.mv' or 'metamean'")
    }
  # Don't add an overall measure if overall.measure is set to 'none'
  } else if (tolower(overall.measure) == "none") {
    df.overall <- data.frame(
      outcomes = character(),
      k = numeric(),
      o.i = numeric(),
      o.c = numeric(),
      o = numeric(),
      te = numeric(),
      ci.l = numeric(),
      ci.u = numeric(),
      I2 = numeric(),
      pow = numeric(),
      seTE.random = numeric()
    )
  } else {
    print("error in plot.summary.forest(): set overall.measure to 'net', 'pairwise', or 'none'")
  }

  df.sum <- rbind(
    df.sum,
    df.overall
  )
  
  # create col with I^2 is 0 instead of NA
  df.sum$I2.trans <- df.sum$I2*100

  df.sum[is.na(df.sum$I2.trans), "I2.trans"] <- 0
  
  # addd column of te that does not get rounded (for correct order in plot/table later)
  df.sum$te.prec <- df.sum$te
  if (tolower(overall.measure) != "none") {
    df.sum[df.sum$outcomes == overall.name, "te.prec"] <- -100  # to set it at the bottom line
  }
  # roud values
  df.sum[,c("o", "te", "ci.l", "ci.u", "I2", "I2.trans", "pow")] <- round(df.sum[,c("o", "te", "ci.l", "ci.u", "I2", "I2.trans", "pow")], 2)
  df.sum[,c("I2", "I2.trans")] <- round(df.sum[,c("I2", "I2.trans")], 0)

  # create col of strings with te an ci vaulues
  df.sum$te.ci <- paste(
    as.character(df.sum$te), " (",
    as.character(df.sum$ci.l), ", ",
    as.character(df.sum$ci.u), ")",
    sep = ""
  )

  # create col of strings with observation and no. of interventions
  df.sum$o.k <- paste(
    as.character(df.sum$o), " (",
    as.character(df.sum$k), ")",
    sep = ""
  )
  
  if (!is.logical(net.res.all) && tolower(overall.measure) != "none") {
    df.sum[df.sum$outcomes == overall.name, "o.k"] <- paste(
      as.character(df.sum[df.sum$outcomes == overall.name, "o"]), " (",
      as.character(net.res.all$m), ")",
      sep = ""
    )
  } else if (tolower(overall.measure) != "none") {
    df.sum[df.sum$outcomes == overall.name, "o.k"] <- as.character(df.sum[df.sum$outcomes == overall.name, "o"])
  }
  
  # set band colours for table
  df.sum <- arrange(df.sum, te.prec)  # sort df.sum according to te
  if (nrow(df.sum) %% 2 == 1){
    df.sum$colour <- c("gray90", rep(c("white", "gray95"), nrow(df.sum) / 2))
  } else {
    df.sum$colour <- rep(c("white", "gray95"), nrow(df.sum) / 2)
  }
  
  
  # set color range for coloring heterogeneity
  fun_color_range <- colorRampPalette(c("chartreuse4", "gold", "red", "darkred"))  # Create color generating function
  my_colors <- fun_color_range(20)
    
  
  # drop column of power
  df.sum <- df.sum %>% select(!pow)
  # forest plot"
  p <- ggplot(df.sum, aes(y = te, x = reorder(outcomes, te.prec))) + 
    geom_vline(xintercept = df.sum$outcomes, colour = df.sum$colour, size = 19) +
    geom_pointrange(
      aes(
        ymin = ci.l, ymax = ci.u, colour = I2.trans, size = o
      ),
      alpha = 0.5, shape = 15
    ) +
    coord_flip() +
    guides(colour = guide_legend(override.aes = list(size=1)), reverse=TRUE) +
    scale_colour_gradientn(
      colors = my_colors, name=expression(paste("I"^{2}*" [%]")),
      limits = c(0,100), breaks = c(0, 25, 50, 75, 100)
    ) + 
    scale_size_continuous(
      name = "Data Points", range = c(0.5, 3),
      breaks = c(
        950, 450, 200, 50
      )
    ) +
    geom_hline(aes(yintercept=0), colour = 'red', lty=2) +
    geom_hline(aes(yintercept=0.2), colour = 'darkgray', lty=3) +
    geom_hline(aes(yintercept=0.5), colour = 'darkgray', lty=3) +
    geom_hline(aes(yintercept=0.8), colour = 'darkgray', lty=3) +
    geom_hline(aes(yintercept=-0.2), colour = 'darkgray', lty=3) +
    geom_hline(aes(yintercept=-0.5), colour = 'darkgray', lty=3) +
    geom_hline(aes(yintercept=-0.8), colour = 'darkgray', lty=3) +
    theme_bw() +
    theme(
      legend.key=element_rect(fill='cornsilk2'),
      axis.text.y = element_blank(), axis.title.y = element_blank(),
      axis.text=element_text(size=18), #change font size of axis text
      axis.title=element_text(size=18), #change font size of axis titles
      plot.title=element_text(size=18, hjust = 0.5),
      legend.title = element_text(size=19),
      legend.text = element_text(size=16),
    ) +
    ggtitle("\nSt. Mean Difference") + ylab("may reduce resilience                                 may improve resilience")
  # table
  font.size <- 7
  
  # Set I2 to "NA" where not applicable
  if (overall.measure != "net" && tolower(overall.measure) != "none") {
    df.sum[df.sum$outcomes == overall.name, c("I2", "I2.trans")] <- "NA"
  }
  df.sum[df.sum$k <= 1, c("I2", "I2.trans")] <- "NA"
  
  
  data_table <- ggplot(data = df.sum, aes(y = reorder(outcomes, te.prec))) +
    geom_hline(aes(yintercept = outcomes, colour = arrange(df.sum, te.prec)$colour), size = 19) +

    geom_text(aes(x = 0, label = outcomes), hjust = 0, size = font.size) +
    geom_text(aes(x = 1, label = o.k), nudge_x = 0.1, hjust = 0, size = font.size) +
    geom_text(aes(x = 2, label = te.ci), hjust = 0, nudge_x = - 0.1, size = font.size) +
    geom_text(aes(x = 3, label = I2.trans), hjust = 0, nudge_x = - 0.1, size = font.size) +
    # geom_text(aes(x = 4, label = pow), hjust = 0, nudge_x = - 0.5, size = font.size) +

    scale_colour_identity() +
    theme_void() +
    ggtitle(expression(paste("       Outcome                                   Data Points (Comparisons)   SMD (CI) [Hedge's g]               I"^{2}*" [%]"))) +
    theme(
      plot.margin = margin(6, 0, 48, 0),
      plot.title=element_text(size=23, face="bold"),
    )
  
  # combine forest plot and table with additional title
  combined_plot <- grid.arrange(
    data_table, p, ncol = 2,
    widths = c(4/7, 3/7),
    top = textGrob(title, gp = gpar(fontsize = 30, fontface = "bold"))
  )
  combined_plot
}


### Network meta-analysis
net.meta.analyze <- function(
  # parameters for data collection
  outcome.vec = NULL, preferred.scale = F, net.df = F, net.res = F, filter.forest..funnel.vec = F, comparisons.skip.list = F,
  
  # for netmeta()
  details.chkmultiarm = T, tol.multiarm = 10,  ####### <-- do sensitivity analyis in which studies with higher multi-arm inconsitency than default value in netmeta() are cut out
  
  # plots
  plot.netgraph = F, plot.forest = F, plot.direct.evidence = F, plot.netheat = F,
  
  # return
  silent = F, return.data = F,
  
  # additional
  reference.group = "passive control", random = T
  
  
  # set df or results object to net.df or net.res to skip the respective caluclation (directly plot the repective plots with given df or results object)
){
  
  if (is.logical(net.df) & is.logical(net.res)){
    net.df <- data.frame(
      studlab = c(),
      outcome = c(),
      TE = c(),
      seTE = c(),
      treat1 = c(),
      treat2 = c()
    )

    # Renomve all follow-up time points from m.data.list
    # (otherwise time points after the post-test would flow into the model as post-test data)
    m.data.list.no.T2.T3 <- m.data.list
    m.data.list.no.T2.T3$results.descriptive.array[,,c("T2", "T3"),,,] <- NA_real_
    
    for (outcome in outcome.vec){
      for (i in 1:length(comparions.all)){
        for (j in 1:length(comparions.all)){
          # skip if same intervention/control is compared to it self or if comparison was done already
          if (j <= i){
            next
          }

          if (preferred.scale == F){
            preferred.scale <- get.1st.preferred.scale(outcome)
          }

          if (!silent){
            cat("\n", "# ", names(comparions.all)[i], " vs. ", names(comparions.all)[j], "\n", sep = "")
          }

          results.meta <- meta.analyze(
            outcome, meditation.type.all, m.data.list.no.T2.T3, preferred.scale = preferred.scale, meta.df.list = F, comparison.list = list(comparions.all[[i]], comparions.all[[j]]),
            split.subgroups = F, return.data = "results.meta", silent = silent, filter.forest..funnel.vec = filter.forest..funnel.vec, comparisons.skip.list = comparisons.skip.list
          )

          if (results.meta$k > 0){
            # correct polarities of scales so high means more resilient
            pol <- outcome.direction.df[outcome.direction.df[, "Outcome"] == outcome, "High.or.low.means.resilient"]
            sign. <- ifelse(pol == "v", -1, 1)

            # append SMD data to net.df
            new.df <- data.frame(
              studlab = results.meta$studlab,
              outcome = outcome,
              TE = sign. * results.meta$TE,
              seTE = results.meta$seTE,
              treat1 = names(comparions.all[i]),
              treat2 = names(comparions.all[j])
            )
            net.df <- rbind(net.df, new.df)
          }
        }
      }
    }
    
    # add number to label if same intervention comparision is present more than one time in a study
    # in other words, pretend this intervention comparision is from another study
      # this approach leads to a lack of standard error adjustment that would have been done for multi-arm studies:
      # "Multi-Arm Studies
      # Usually, we have a number of multi-arm studies (i.e. studies with more than two
      # treatment groups) to include in our network meta-analysis. We can do this most
      # easily by including each multi-arm study in the dataset as a series of two-arm
      # comparisons. However, the standard error of each two-arm comparison from a
      # multi-arm study needs to be adjusted to reflect the fact that comparisons within
      # multi-arm studies are correlated." (Schwarzer, G. et al. (2015) Meta-Analysis with R. p. 192)
    
    if (nrow(net.df) > 1){
      dupls.i <- which(duplicated(net.df[,c("studlab", "treat1", "treat2")]))
      stud.labs.dupl <- unique(net.df[,"studlab"])

      for (stud.lab.dupl in stud.labs.dupl){
        i <- 1
        for (dupl.i in dupls.i){
          if (net.df[dupl.i, "studlab"] == stud.lab.dupl){
            net.df[dupl.i, "studlab"] <- paste(stud.lab.dupl, " #", i, "#", sep = "")
            i <- i + 1
          }
        }
      }
    }
  } else if (!is.data.frame(net.df) & is.logical(net.res)){
    cat("\nerror in net.meta.analyze(): set parameter net.df to a data frame of the format required for netmeta(), got class", class(net.df), "instead\n")
  }
  
  if (is.logical(net.res)){
    if (nrow(net.df) > 1){
      net.res <- netmeta(
        TE = TE,
        seTE = seTE,
        treat1 = treat1,
        treat2 = treat2,
        studlab = studlab,
        data = net.df,
        sm = "SMD",
        common = !random,
        random = random,
        reference.group = ifelse(
          reference.group %in% net.df$treat1 | reference.group %in% net.df$treat2,
          reference.group,
          "meditation (exclusive)"
        ),
        details.chkmultiarm = details.chkmultiarm,
        sep.trts = " vs. ",
        tol.multiarm = tol.multiarm
      )
      
      if (!(reference.group %in% net.df$treat1 | reference.group %in% net.df$treat2)){
        cat(
          "warning in net.meta.analyze() for calculation of net.res: parameter reference.group (", reference.group, ") was set to 'meditation (exclusive)' as ",
          reference.group, " was not present\n", sep = ""
        )
      }
      
    } else {
      print("error in net.meta.analyze(): calculation of net.res not possible as nrow(net.df) is not above 1 --> only 1 or 0 treatment comparisons are present")
    }
  } else if (class(net.res) != "netmeta"){
    cat("\nerror in net.meta.analyze(): set parameter net.res to an variable of class netmeta, got class", class(net.res), "instead\n")
  }
  
  if (plot.netgraph == T){
    if (is.logical(net.res)){
      print("error in net.meta.analyze(): plot of netgraph not possible as nrow(net.df) is not above 1 --> only 1 or 0 treatment comparisons are present")
    } else {
      netgraph(
        net.res,
        iterate=F, # start="prcomp", 
        col="darkgray", cex=1.5, multiarm=F,
        points=F, col.points="blue", cex.points=3,
        number.of.studies = T, pos.number.of.studies = 0.42
      )
    }
  }
  
  if (plot.direct.evidence == T){
    if (is.logical(net.res)){
      print("error in net.meta.analyze(): plot of direct.evidence.plot not possible as nrow(net.df) is not above 1 --> only 1 or 0 treatment comparisons are present")
    } else {
      d.evidence <- direct.evidence.plot(net.res, random = T)
      plot(d.evidence)
    }
  }
  
  if (plot.forest == T){
    if (is.logical(net.res)){
      print("error in net.meta.analyze(): plot of network forest plot not possible as nrow(net.df) is not above 1 --> only 1 or 0 treatment comparisons are present")
    } else {
      forest(
        net.res,
        reference.group = ifelse(
          reference.group %in% net.res$treat1 | reference.group %in% net.res$treat2,
          reference.group,
          "meditation (exclusive)"
        ),
        sortvar = TE,
        drop.reference.group = TRUE,
        label.left = "may reduce resilience",
        label.right = "may improve resilience"
  #       smlab = paste(  # headline of forest plot in the center
  #         "Therapy Formats vs. Care As Usual \n",
  #         "(Depressive Symptoms)"),
      #        xlim = c(-1.3, 0.5),
      #        labels = long.labels
      )
      
      if (!(reference.group %in% net.res$treat1 | reference.group %in% net.res$treat2)){
        cat(
          "warning in net.meta.analyze() for forest plot: parameter reference.group (", reference.group, ") was set to 'meditation (exclusive)' as ",
          reference.group, " was not present\n", sep = ""
        )
      }
    }
  }
  
  if (plot.netheat){
    if (is.logical(net.res)){
      print("error in net.meta.analyze(): plot of netheat plot not possible as nrow(net.df) is not above 1 --> only 1 or 0 treatment comparisons are present")
    } else {
      netheat(net.res, nchar.trts = 3)
    }
  }
  
  if (return.data != F){
    if (return.data == "net.df"){
      return(net.df)
    } else if (return.data == "net.res"){
      return(net.res)
    } else {
      print("error in net.meta.analyze(): set parameter return.data to 'net.df' or 'net.res'")
    }
  }
}
