# This script includes utility functions for meta-analysis, e.g., multiple regressions, regression plots, and correlation matrices.
# Some of these functions are used as helper functions in meta.analyze() of main_function_1_forest_funnel_return_regression.R

### Multiple Regressions
do.multiple.regressions <- function(  # fixed effects model not implemented
  outcome, preferred.scale = F, meta.df.list = F, moderator.multiple.list,
  filter.multiple.regression.linear.list = F, filter.multiple.regression.poly.list = F,
  print.regplot = T, print.baujat.regression = T, print.gosh.regression = F,
  print.regression.results = T, print.regression.df = F, regression.label = F,
  degree.1 = T, degree.2 = F, non.interaction = T, interaction = F, without.mean.r = F
  # moderator.multiple.list --> list of string vectors specifying what moderators has to be combined
  # filter.regression.linear/poly.list (filters only linear regression) --> list with filtering vectors in order of moderator.vec ("" means no filtering)
){
  
  meditation.type.var <- meditation.type.all
  
  mod.comb.no <- 1
  
  for (moderator.combination in moderator.multiple.list){
    
    regression.df <- meta.analyze(
      outcome, meditation.type.var, m.data.list, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
      split.subgroups = F, return.data = "regression.all", without.mean.r = without.mean.r
    )
    
    # cut out data after post-test (that have follow.up.period > 0) if follow.up.period is not in moderator.combination
    if (!"follow.up.period" %in% moderator.combination){
      regression.df <- regression.df[
        which(regression.df[,"follow.up.period"] == 0),
      ]
    }
    
    # eliminate NAs
    if (length(moderator.combination) == 2){
      regression.df <- regression.df %>%
        filter(!(
          is.na(!!sym(moderator.combination[1])) |
          !!sym(moderator.combination[1]) %in% c("NA", nm.placeholder, as.character(nm.placeholder)) |
          is.na(!!sym(moderator.combination[2])) |
          !!sym(moderator.combination[2]) %in% c("NA", nm.placeholder, as.character(nm.placeholder))
        ))
    } else if (length(moderator.combination) == 3){
      regression.df <- regression.df %>%
        filter(!(
          is.na(!!sym(moderator.combination[1])) |
          !!sym(moderator.combination[1]) %in% c("NA", nm.placeholder, as.character(nm.placeholder)) |
          is.na(!!sym(moderator.combination[2])) |
          !!sym(moderator.combination[2]) %in% c("NA", nm.placeholder, as.character(nm.placeholder)) |
          is.na(!!sym(moderator.combination[3])) |
          !!sym(moderator.combination[3]) %in% c("NA", nm.placeholder, as.character(nm.placeholder))
        ))
    } else {
      print("filtering multiple regression data frame from NAs is only implemted for length(moderator.combination) == 2 or 3")
        # could be implemented with for loop
    }
    
# Linear
    if (degree.1){
      
      # filter for linear regression
      if (
        is.list(filter.multiple.regression.linear.list) &
        if(is.list(filter.multiple.regression.linear.list)){filter.multiple.regression.linear.list[[mod.comb.no]][1] != ""}else{F}
      ){
        regression.df.filtered <- regression.df[filter.multiple.regression.linear.list[[mod.comb.no]],]
      } else {
        regression.df.filtered <- regression.df
      }
      
      if (print.regression.df){
        cat("regression data frame for multiple linear regression of", moderator.combination, "\n")
        print(regression.df.filtered)
      }
      
      if (non.interaction){
        results.regression.linear <- get.results.metafor(
          regression.df.filtered, moderator = moderator.combination
        )
      }
      
      if (interaction){
        results.regression.linear.interact <- get.results.metafor(
          regression.df.filtered, moderator = moderator.combination, interaction = T
        )
      }
      
      # bubble/regression plot      
      if (print.regplot & length(moderator.combination) == 2){
        
        # get model results for 500 values between -1 and maximum of moderator 1 + 5 for 3 fixed values of moderatore 2
        if (non.interaction){
          model.res.list <- list(min = "", mean = "", max = "", data = "")
        }
        
        if (interaction){
          model.res.list.interact <- list(min = "", mean = "", max = "", data = "")
        }

        for (i in 1:3){
          if (i == 1){
            mod.fix.vals <- rep(min(regression.df.filtered[, moderator.combination[2]], na.rm = T), 500)
          } else if (i == 2){
            mod.fix.vals <- rep(
              (max(regression.df.filtered[, moderator.combination[2]], na.rm = T) + min(regression.df.filtered[, moderator.combination[2]], na.rm = T)) / 2,
              500
            )
          } else if (i == 3){
            mod.fix.vals <- rep(max(regression.df.filtered[, moderator.combination[2]], na.rm = T), 500)
          }   
          
          mod.pred.xvals <- seq(-1, max(regression.df.filtered[, moderator.combination[1]], na.rm = T) + 5, length=500)
          
          if (non.interaction){
            res.obj <- predict(results.regression.linear, newmods = unname(cbind(mod.pred.xvals, mod.fix.vals)), addx=T)
                
            res.df <- data.frame(
              pred = res.obj$pred,
              ci.lb = res.obj$ci.lb,
              ci.ub = res.obj$ci.ub,
              X.mod.1 = res.obj$X[,moderator.combination[1]]
            )
            model.res.list[[i]] <- res.df
          }

          if (interaction){
            res.obj.interact <- predict(results.regression.linear.interact, newmods = unname(cbind(mod.pred.xvals, mod.fix.vals, mod.pred.xvals * mod.fix.vals)), addx=T)
            
            res.df.interact <- data.frame(
              pred = res.obj.interact$pred,
              ci.lb = res.obj.interact$ci.lb,
              ci.ub = res.obj.interact$ci.ub,
              X.mod.1 = res.obj.interact$X[,moderator.combination[1]]
            )
            model.res.list.interact[[i]] <- res.df.interact
          }
        }
        
        # combine data from escalc() and regression.df.filtered to data frame and filter out NA for the multiple regression / bubble plot
        res.escalc <- escalc(
          data = regression.df.filtered, measure = "SMD",
          m1i = regression.df.filtered[,"mean.int"], sd1i = regression.df.filtered[,"sd.int"], n1i = regression.df.filtered[,"n.int"],
          m2i = regression.df.filtered[,"mean.control"], sd2i = regression.df.filtered[,"sd.control"], n2i = regression.df.filtered[,"n.control"]
        )
        
        res.escalc.df <- data.frame(res.escalc[
          which(!(
            is.na(res.escalc[,moderator.combination[1]]) |
            is.na(res.escalc[,moderator.combination[2]]) 
          ))
          ,
        ])
        
        dat <- regression.df.filtered[
          which(!(
            is.na(regression.df.filtered[,moderator.combination[1]]) |
            is.na(regression.df.filtered[,moderator.combination[2]]) 
          ))
          ,
        ]
        
        if (non.interaction){
          model.res.list[[4]] <- dat[,c("study.id", "hedges.g", "weights", moderator.combination[1], moderator.combination[2])]

          # set correct SMD and weights that are used by metafor
          model.res.list[[4]]$hedges.g <- res.escalc.df$yi
          model.res.list[[4]]$weights <- 1 / res.escalc.df$vi
        }
        
        if (interaction){
          model.res.list.interact[[4]] <- dat[,c("study.id", "hedges.g", "weights", moderator.combination[1], moderator.combination[2])]

          # set correct SMD and weights that are used by metafor
          model.res.list.interact[[4]]$hedges.g <- res.escalc.df$yi
          model.res.list.interact[[4]]$weights <- 1 / res.escalc.df$vi
        }
        
        # get fixed values of moderator 2 that was used for prediction as string to label graphs
        mod.fix.val.min <- as.character(round(min(regression.df.filtered[, moderator.combination[2]], na.rm = T), digits = 2))
        mod.fix.val.mean <- as.character(round(
          (max(regression.df.filtered[, moderator.combination[2]], na.rm = T) + min(regression.df.filtered[, moderator.combination[2]], na.rm = T)) / 2,
          digits = 2
        ))
        mod.fix.val.max <- as.character(round(max(regression.df.filtered[, moderator.combination[2]], na.rm = T), digits = 2)) 
        
        # plot
        scale.color.values <- c("red", "blue", "chartreuse3")
        names(scale.color.values) <- c(mod.fix.val.min, mod.fix.val.mean, mod.fix.val.max)
        
        if (non.interaction){
          print(
            ggplot(model.res.list[["min"]], aes(x = X.mod.1, y = pred, group = 1)) + 
              # 3 regression lines (for low, medium, and high moderator 2 values)
              geom_line(aes(col = mod.fix.val.min), size = 1, data = model.res.list[["min"]],) + 
              geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.05, fill = "red", data = model.res.list[["min"]]) +
              geom_line(aes(col = mod.fix.val.mean), size = 1, data = model.res.list[["mean"]]) + 
              geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.05, fill = "blue", model.res.list[["mean"]]) +
              geom_line(aes(col = mod.fix.val.max), size = 1, data = model.res.list[["max"]]) + 
              geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.05, fill = "chartreuse3", model.res.list[["max"]]) +

              # bubbles (SMDs x Moderator 1 x weights)
              geom_point(aes(x=!!sym(moderator.combination[1]), y = hedges.g, size = weights), color="darkslategray", alpha = 0.5, data = model.res.list[["data"]]) +

              # reference line
              geom_hline(yintercept = 0, linetype='dotted') +

              # labels
              ylab("Standard Mean Difference") +
              xlab(moderator.combination[1]) +
              labs(title = paste("Multiple Linear Regression Plot of", moderator.combination[1], "and", moderator.combination[2])) +
              scale_color_manual(
                name = moderator.combination[2], values = scale.color.values,
                breaks = c(mod.fix.val.min, mod.fix.val.mean, mod.fix.val.max)
              ) +
              scale_size_continuous(guide = "none")  # +
              # coord_cartesian(ylim = c(NA, 0))  # to change display range
          )
        }
        
        if (interaction){
          print(
            ggplot(model.res.list.interact[["min"]], aes(x = X.mod.1, y = pred, group = 1)) + 
              # 3 regression lines (for low, medium, and high moderator 2 values)
              geom_line(aes(col = mod.fix.val.min), size = 1, data = model.res.list.interact[["min"]],) + 
              geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.05, fill = "red", data = model.res.list.interact[["min"]]) +
              geom_line(aes(col = mod.fix.val.mean), size = 1, data = model.res.list.interact[["mean"]]) + 
              geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.05, fill = "blue", model.res.list.interact[["mean"]]) +
              geom_line(aes(col = mod.fix.val.max), size = 1, data = model.res.list.interact[["max"]]) + 
              geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.05, fill = "chartreuse3", model.res.list.interact[["max"]]) +

              # bubbles (SMDs x Moderator 1 x weights)
              geom_point(aes(x=!!sym(moderator.combination[1]), y = hedges.g, size = weights), color="darkslategray", alpha = 0.5, data = model.res.list.interact[["data"]]) +

              # reference line
              geom_hline(yintercept = 0, linetype='dotted') +

              # labels
              ylab("Standard Mean Difference") +
              xlab(moderator.combination[1]) +
              labs(title = paste(
                "Multiple Linear Regression Plot of", moderator.combination[1], "and", moderator.combination[2],
                "(with interaction)"
              )) +
              scale_color_manual(
                name = moderator.combination[2], values = scale.color.values,
                breaks = c(mod.fix.val.min, mod.fix.val.mean, mod.fix.val.max)
              ) +
              scale_size_continuous(guide = "none")  # +
              # coord_cartesian(ylim = c(NA, 0))  # to change display range
          )
        }
        
        
#         ############# TEST to plot multiple regression plot for factors ###############
#         results.regression.linear.cat <- get.results.metafor(
#           regression.df.filtered, moderator = moderator.combination, categorical.mod.trans = T
#         )
        
#         y.lim.min <- min(results.regression.linear.cat$data$hedges.g)
#         y.lim.max <- max(results.regression.linear.cat$data$hedges.g)
        
#         plot(
#           coef(results.regression.linear.cat)[1:3], type="o", pch=19,
#           xlim=c(0.8,3.2), ylim=c(y.lim.min,y.lim.max),
#           xlab=moderator.combination[1],
#           ylab="Standardized Mean Difference", xaxt="n", bty="l"
#         )
        
#         axis(side=1, at=1:3, labels=c("min","mean","max"))
        
#         lines(
#           coef(results.regression.linear.cat)[1:3] +
#           coef(results.regression.linear.cat)[4], type="o", pch=15, lty="dotted"
#         )
        
#         legend(
#           "topright", legend=c(paste("min", moderator.combination[2]), paste("max", moderator.combination[2])),
#           lty=c("solid", "dotted"), pch=c(19,15), inset=0.01
#         )
        
#         title("Estimated Average Effects based on the Additive Model")

#       } else if (print.regplot){
#         print("linear multiple regression plot currently works only with 2 moderators")
        #################################
      } else if (print.regplot) {
        print("printing the bubble plot for multiple linear regressions is only implemented for 2 moderators at once")
      }
                                            
      # baujat plot
      if (print.baujat.regression){
        # par(mar = c(5, 6, 4, 2))  # to set plot parameters?
        # plot(influence(results.regression.linear), cex = 0.8, las = 1)
        
        if (non.interaction){
          baujat(results.regression.linear, xlab = paste("Squared Pearson Residual (for ", paste(moderator.combination, collapse = ", "), "; linear)", sep = ""))
        }
        
        if (interaction){
          if (length(moderator.combination) < 3){
            baujat(results.regression.linear.interact, xlab = paste("Squared Pearson Residual (for ", paste(moderator.combination, collapse = ", "), "; linear; with interaction)", sep = ""))
          } else {
            # for length(moderator.combination) == 3 an error occured with baujat plot, so the hat value and the weights of the influnce() will be used to search for outliers instead
            cat("\ninfluence plots of:", moderator.combination)
            par(mar = c(5, 6, 4, 2))
            plot(influence(results.regression.linear.interact), cex = 0.8, las = 1)
          }
        }
      }

      # gosh plot
      if (print.gosh.regression){
        if (
          is.list(filter.multiple.regression.linear.list) &
          if(is.list(filter.multiple.regression.linear.list)){filter.multiple.regression.linear.list[[mod.comb.no]][1] != ""}else{F}
        ){
          
          if (non.interaction){
            for (outlier in -filter.multiple.regression.linear.list[[mod.comb.no]]){

              results.regression.linear.unfiltered <- get.results.metafor(
                regression.df, moderator = moderator.combination
              )

              cat("\n Gosh plot for", paste(moderator.combination, sep = ", "), "(outlier number:", outlier, "linear)")
              sav <- gosh(results.regression.linear.unfiltered, subsets = 15000)
              plot(sav, out = outlier)
            }
          }
          
          if (interaction){
            for (outlier in -filter.multiple.regression.linear.list[[mod.comb.no]]){

              results.regression.linear.interact.unfiltered <- get.results.metafor(
                regression.df, moderator = moderator.combination, interaction = T
              )

              cat("\n Gosh plot for", paste(moderator.combination, sep = ", "), "(outlier number:", outlier, "linear)")
              sav <- gosh(results.regression.linear.interact.unfiltered, subsets = 15000)
              plot(sav, out = outlier)
            }
          }
          
        } else {
          cat("\nno Gosh plot printed for", moderator.combination, "(linear)\n")
        }
      }
    }
    
# Polynomial
    if (degree.2){
      if (interaction){
        print("using multiple regression models with interaction ist not implemented yet")
      }
      
      degree <- 2
  # filter for polynomial regression
      if (
        is.list(filter.multiple.regression.poly.list) &
        if(is.list(filter.multiple.regression.poly.list)){filter.multiple.regression.poly.list[[mod.comb.no]][1] != ""}else{F}
      ){
        regression.df.filtered <- regression.df[filter.multiple.regression.poly.list[[mod.comb.no]],]
      } else {
        regression.df.filtered <- regression.df
      }
      
      # eliminate NAs
      regression.df.filtered <- regression.df.filtered %>%
        filter(!(
          is.na(!!sym(moderator.combination[1])) |
          !!sym(moderator.combination[1]) %in% c("NA", nm.placeholder, as.character(nm.placeholder)) |
          is.na(!!sym(moderator.combination[2])) |
          !!sym(moderator.combination[2]) %in% c("NA", nm.placeholder, as.character(nm.placeholder))
        ))
      
      if (print.regression.df){
        cat("regression data frame for multiple polynomial regression of", moderator.combination, "\n")
        print(regression.df.filtered)
      }
      
      results.regression.poly <- get.results.metafor(
        regression.df.filtered, moderator = moderator.combination, degree = degree
      )
      
      # bubble plot
      if (print.regplot){
        print("bubble plot for multiple polynomial regressions is not implemented yet")
        
#         ############### TEST for multiple polynomial regression plots ##############
#         for (mod.no in 1:length(moderator.combination)){
          
#           xvals <- seq(
#             -1,
#             max(regression.df.filtered[
#               which(!is.na(regression.df.filtered[,moderator.combination[mod.no]])),
#               moderator.combination[mod.no]
#             ]) + 5,
#             length=500
#           )
          
#           sav <- predict(
#             results.regression.poly,
#             newmods = unname(poly(xvals, degree=degree, raw=T))
#           )
          
#           regplot(
#             results.regression.poly,
#             mod = (mod.no - 1) * degree + 2,
#             refline=0, pred=sav, xvals=xvals, las=1, digits=1, bty="l",
#             xlab = paste(moderator.combination[mod.no], " with follwing fixed: ", moderator.combination[-mod.no], "; polynomial", sep = ""),
#             label=regression.label
#             # xlim = c(0,60), predlim = c(0,60)  # adjusting display area of graph
#           )
#         }
#        ##################################
        
       }

      # baujat plot
      if (print.baujat.regression){
        # par(mar = c(5, 6, 4, 2))  # to set plot parameters?
        # plot(influence(results.regression.poly), cex = 0.8, las = 1)

        baujat(results.regression.poly, xlab = paste("Squared Pearson Residual (for ", paste(moderator.combination, collapse = ", "), "; Degree: ", degree, ")", sep = ""))
      }

      # gosh plot
      if (print.gosh.regression){
        if (
          is.list(filter.multiple.regression.poly.list) &
          if(is.list(filter.multiple.regression.poly.list)){filter.multiple.regression.poly.list[[mod.comb.no]][1] != ""}else{F}
        ){
          for (outlier in -filter.multiple.regression.poly.list[[mod.comb.no]]){
            results.regression.poly.unfiltered <- get.results.metafor(
              regression.df, moderator = moderator.combination
            )

            cat("\n Gosh plot for", paste(moderator.combination, sep = ", "), "(outlier number:", outlier, "; Degree:",degree, ")")
            sav <- gosh(results.regression.poly.unfiltered, subsets = 15000)
            plot(sav, out = outlier)
          }
        } else {
          cat("\nno Gosh plot printed for", moderator.combination, "(linear)\n")
        }
      }
    }
    
    if (print.regression.results & degree.1){
      cat("\nMultiple Regression Results for", moderator.combination, "\n")
      if (non.interaction){
        print("Multiple Linear (no interaction)")
        print(results.regression.linear)
      }
      if (interaction){
        print("Multiple Linear (with interaction)")
        print(results.regression.linear.interact)
      }

      if (degree.2){
        cat("\n")
        print("Multiple Polynomial")
        print(results.regression.poly)
      }
    }
    
    mod.comb.no <- 1 + 1
  }
}

# implement also interaction of moderators?
### Plot Regression plots with ggplot
# only for linear regression
regplot.ggplot <- function(reg.df, moderator, regression.label = T){    
  res.reg.lin.metafor <- get.results.metafor(
    reg.df, moderator = moderator
  )

  res.escalc <- escalc(
    data = reg.df, measure = "SMD",
    m1i = reg.df[,"mean.int"], sd1i = reg.df[,"sd.int"], n1i = reg.df[,"n.int"],
    m2i = reg.df[,"mean.control"], sd2i = reg.df[,"sd.control"], n2i = reg.df[,"n.control"]
  ) %>% mutate(labels = 1:length(rownames(.))) %>% relocate(labels)

  pred <- as.data.frame(predict(res.reg.lin.metafor, addx=TRUE))

  w <- weights(res.reg.lin.metafor)
  
  font.size <- 21
  
  if (regression.label){
    p <- ggplot(pred, aes(x = !!sym(paste("X.", moderator, sep = "")), y = pred, group = 1)) +
      geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.05, fill = "blue", col = "blue", linetype=3, data = pred) +
      geom_ribbon(aes(ymin = pi.lb, ymax = pi.ub), alpha = 0.05, fill = "red", col = "red", linetype=3, data = pred) +
      geom_line(col = "blue", data = pred,) + 
      geom_point(aes(x=!!sym(moderator), y = yi, size = w), color="darkslategray", alpha = 0.4, data = res.escalc) +
      geom_text(data = res.escalc, aes(x=!!sym(moderator),y=yi, label = labels), vjust = 1.8) +

      # reference line
      geom_hline(yintercept = 0, linetype='dotted') +

      # labels
      ylab("Standard Mean Difference") +
      xlab(regression.labels.df[,moderator]) +
      labs(title = paste(regression.labels.df[,moderator])) +
      scale_size_continuous(guide = "none") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
      theme(text=element_text(size=font.size))
  } else {
    p <- ggplot(pred, aes(x = !!sym(paste("X.", moderator, sep = "")), y = pred, group = 1)) +
      geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.05, fill = "blue", col = "blue", linetype=3, data = pred) +
      geom_ribbon(aes(ymin = pi.lb, ymax = pi.ub), alpha = 0.05, fill = "red", col = "red", linetype=3, data = pred) +
      geom_line(col = "blue", data = pred,) + 
      geom_point(aes(x=!!sym(moderator), y = yi, size = w), color="darkslategray", alpha = 0.4, data = res.escalc) +

      # reference line
      geom_hline(yintercept = 0, linetype='dotted') +

      # labels
      ylab("Standard Mean Difference") +
      xlab(regression.labels.df[,moderator]) +
      labs(title = paste(regression.labels.df[,moderator])) +
      scale_size_continuous(guide = "none") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
      theme(text=element_text(size=font.size))
  }
  plot(p)
}
### Get correlation matrix
get.cor.matrix <- function(
  regression.df,  # regression.df should be df with all regression data (all time points)
  moderator.cor.vec = c("sessions.duration", "sessions.frequency", "programs.duration", "follow.up.period", "female.percent")
){
  
  # generate correlation matrix
  mod.len <- length(moderator.cor.vec)
  cor.df <- data.frame(matrix(rep(NA, mod.len**2), nrow = mod.len, ncol = mod.len))
  colnames(cor.df) <- moderator.cor.vec
  rownames(cor.df) <- moderator.cor.vec

  i <- 0
  for(moderator.cor.1 in moderator.cor.vec){
    i <- i + 1
    j <- 0
    for (moderator.cor.2 in moderator.cor.vec){
      j <- j + 1
      if (j >= i){
        cor.df[i, j] <- NA
      } else {
        mod.vec.1 <- regression.df[, moderator.cor.1]
        mod.vec.2 <- regression.df[, moderator.cor.2]
        mod.df <- data.frame(
          mod.1 = mod.vec.1,
          mod.2 = mod.vec.2
        )
        r.test <- cor.test(~ mod.1 + mod.2, data = mod.df)
        r.est <- round(r.test$estimate, digits = 2)  # pearson's rho
        r.p <- r.test$p.value  # p-value
        print(r.p)
        r.p.symbol <- if(T %in% (r.p <= 0.001)){"***"}else if(T %in% (r.p <= 0.01)){"**"}else if(T %in% (r.p <= 0.05)){"*"}else{""}
        cor.df[i, j] <- paste(r.est, r.p.symbol, sep = "")
      }
    }
  }
  
  # add column with moderator names (for display in dashboard)
  cor.df <- cor.df %>%
    mutate(moderators = moderator.cor.vec) %>%
    relocate(moderators)
  
  as.data.frame(cor.df)
}
