# This script contains utility functions for a Shiny dashboard displaying meta-analysis results.
# It includes: set.outcome.page() to create individual outcome UI tabs with forest plots, funnel plots, subgroup analyses, and regression outputs;
# set.summary.page() to create summary domain tabs with network meta-analysis visualizations

### Create outcome page for the ui
set.outcome.page <- function(outcome, preferred.scale = FALSE){
  
# Collect data of outcome 
  meta.df.list <- meta.df.lists[[outcome]]
  
# Get p-value of Egger's regression test for funnel plot asymmetry
  funnel.asym.p.egger <- print.meta.results(
    outcome, preferred.scale = preferred.scale,  meta.df.list = meta.df.list,
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "funnel.asym.p.egger"
  )
  if(length(funnel.asym.p.egger) == 0){
    funnel.asym.p.egger <- 1
  }
  
  funnel.asym.p.egger.n.o. <- print.meta.results(
    outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "funnel.asym.p.egger", filter.forest..funnel.vec = if(length(outlier.list[[outcome]]) != 0){-outlier.list[[outcome]]}else{FALSE}
  )
  if(length(funnel.asym.p.egger.n.o.) == 0){
    funnel.asym.p.egger.n.o. <- 1
  }
  
# Get p-value of rank correlation test for funnel plot asymmetry
  funnel.asym.p.rank <- print.meta.results(
    outcome, preferred.scale = preferred.scale,  meta.df.list = meta.df.list,
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "funnel.asym.p.rank"
  )
  if(length(funnel.asym.p.rank) == 0){
    funnel.asym.p.rank <- 1
  }
  
  funnel.asym.p.rank.n.o. <- print.meta.results(
    outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "funnel.asym.p.rank", filter.forest..funnel.vec = if(length(outlier.list[[outcome]]) != 0){-outlier.list[[outcome]]}else{FALSE}
  )
  if(length(funnel.asym.p.rank.n.o.) == 0){
    funnel.asym.p.rank.n.o. <- 1
  }
  
  # get number of control and experiment groups' total observations
  results.meta <-  print.meta.results(
    outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "results.meta"  # , filter.forest..funnel.vec = if(length(outlier.list[[outcome]]) != 0){-outlier.list[[outcome]]}else{FALSE}
  )
  o.exp <- results.meta$n.e.pooled
  o.con <- results.meta$n.c.pooled
  o.total <- o.exp + o.con
  k <- results.meta$k
  pval.random <- round(as.double(results.meta$pval.random), digits = 4)
  lower.I2 <- round(as.double(results.meta$lower.I2) * 100, digits = 2)
  upper.I2 <- round(as.double(results.meta$upper.I2) * 100, digits = 2)
  
  # get results for network meta-analysis
  net.res <- net.ress[[outcome]]
  
# Adjust outcome name because spaces and brackets could cause trouble later on
  outcome.alias <- gsub("([()])", "", gsub(" ", "_", outcome))
  
  
  tabItem(
    tabName = paste(outcome.alias, "_page", sep = ""),
    tabsetPanel(
      type = "tabs",
      
# Main Results (with outliers)
      tabPanel(
        forest.tab.name,
        fluidRow(
          box(
            title = "Forest Plot",
            footer = paste(
              "Exp. Observations: ", o.exp, "; Con. Observations: ", o.con, "; Total: ", o.total, "; k: ", k,
              "; I2 CI: (", lower.I2, ", ", upper.I2, "); p (SMD diff. from 0): ", pval.random, sep = ""
            ),
            width = 8,
            class = "well",
            plotOutput(paste("forest.", outcome.alias, sep = "")),
            align="center",
            collapsible = TRUE
          ),
          box(
            title = "Funnel Plot (with Trim and Fill Method)",
            background = if(funnel.asym.p.egger <= 0.05){sig.0.05.color}else{NULL},
            footer = paste(
              "P-value of Egger's regression and rank correlation tests for funnel plot asymmetry: ",
              round(funnel.asym.p.egger, digits = 4), ", ", round(funnel.asym.p.rank, digits = 4), sep = ""
            ),
            width = 4,
            class = "well",
            plotOutput(paste("funnel.", outcome.alias, sep = "")),
            collapsible = TRUE
          )
        ),
        
        fluidRow(
          box(
            title = "Forest Plot (devided by Delivery Mode)",
            width = 6,
            class = "well",
            plotOutput(paste("subgroup.delivery.", outcome.alias, sep = "")),
            align="center",
            style = forest.plot.box.height.Stress,
              # adjust height of grey box around the plot, as the high number of studies hinders shiny to recognize the actual plot size
            collapsible = TRUE
          ),
          box(
            title = "Forest Plot (devided by Meditation Type)",
            width = 6,
            plotOutput(paste("subgroup.type.", outcome.alias, sep = "")),
            class = "well",
            align = "center",
            style = forest.plot.box.height.Stress,
            collapsible = TRUE
          )
        )
      ),
      
# Main Results comparison with/without outliers
      tabPanel(
        forest.no.outliers.tab.name,
        fluidRow(
          box(
            title = "Influence Data",
            width = 7,
            class = "well",
            tableOutput(paste("influence.df.", outcome.alias, ".forest", sep = "")),
            align="right",
            collapsible = TRUE,
            collapsed = TRUE
          ),
          box(
            title = "Influence Plots",
            width = 5,
            class = "well",
            plotOutput(paste("influence.", outcome.alias, ".forest", sep = "")),
            align="center",
            collapsible = TRUE,
            collapsed = TRUE
          ),
        ),
        fluidRow(
          box(
            title = "Baujat Plot",
            width = 6,
            class = "well",
            plotOutput(paste("baujat.", outcome.alias, ".forest.comp", sep = "")),
            align="center",
            collapsible = TRUE,
            collapsed = TRUE
          )
        ),
        # Gosh Plots
        fluidRow(
          box(
            title = "Gosh Plot (outlier 1)",
            width = 4,
            class = "well",
            imageOutput(paste("gosh.1.", outcome.alias, ".forest.comp", sep = "")),
            align="center",
            collapsible = TRUE,
            collapsed = TRUE
          ),
          box(
            title = "Gosh Plot (outlier 2)",
            width = 4,
            class = "well",
            imageOutput(paste("gosh.2.", outcome.alias, ".forest.comp", sep = "")),
            align="center",
            collapsible = TRUE,
            collapsed = TRUE
          ),
          box(
            title = "Gosh Plot (outlier 3)",
            width = 4,
            class = "well",
            imageOutput(paste("gosh.3.", outcome.alias, ".forest.comp", sep = "")),
            align="center",
            collapsible = TRUE,
            collapsed = TRUE
          )
        ),
        
        # Comparison Plots
        fluidRow(
          box(
            width = 6,
            class = "well",
            h4(paste(
              "Outliers:",
              paste(outlier.list[[outcome]], collapse = ", ")
            ))
          )
        ),
        fluidRow(
          box(
            title = "Forest Plot (with outliers)",
            width = 6,
            class = "well",
            plotOutput(paste("forest.o.", outcome.alias, sep = "")),
            align="right",
            collapsible = TRUE
          ),
          box(
            title = "Forest Plot (without outliers)",
            width = 6,
            class = "well",
            plotOutput(paste("forest.n.o.comp", outcome.alias, sep = "")),
            align="right",
            collapsible = TRUE
          )
        ),
        
        fluidRow(
          box(
            title = "Forest Plot (devided by Delivery Mode with outliers)",
            width = 6,
            class = "well",
            plotOutput(paste("subgroup.delivery.", outcome.alias, ".o.", sep = "")),
            align="center",
            style = forest.plot.box.height.Stress,
            collapsible = TRUE
          ),
          box(
            title = "Forest Plot (devided by Delivery Mode without outliers)",
            width = 6,
            class = "well",
            plotOutput(paste("subgroup.delivery.", outcome.alias, ".n.o.comp", sep = "")),
            align="center",
            style = forest.plot.box.height.Stress,
            collapsible = TRUE
          )
        ),
        
        fluidRow(
          box(
            title = "Forest Plot (devided by Meditation Type with outliers)",
            width = 6,
            class = "well",
            plotOutput(paste("subgroup.type.", outcome.alias, ".o.", sep = "")),
            align="center",
            style = forest.plot.box.height.Stress,
            collapsible = TRUE
          ),
          box(
            title = "Forest Plot (devided by Meditation Type without outliers)",
            width = 6,
            class = "well",
            plotOutput(paste("subgroup.type.", outcome.alias, ".n.o.comp", sep = "")),
            align="center",
            style = forest.plot.box.height.Stress,
            collapsible = TRUE
          )
        ),
        
        fluidRow(
          box(
            title = "Funnel Plot (with outliers)",
            width = 6,
            class = "well",
            plotOutput(paste("funnel.o.", outcome.alias, sep = "")),
            footer = paste(
              "P-value of Egger's regression and rank correlation tests for funnel plot asymmetry: ",
              round(funnel.asym.p.egger, digits = 4), ", ", round(funnel.asym.p.rank, digits = 4), sep = ""
            ),
            align="right",
            collapsible = TRUE
          ),
          box(
            title = "Funnel Plot (without outliers)",
            width = 6,
            class = "well",
            plotOutput(paste("funnel.n.o.comp", outcome.alias, sep = "")),
            footer = paste(
              "P-value of Egger's regression and rank correlation tests for funnel plot asymmetry: ",
              round(funnel.asym.p.egger.n.o., digits = 4), ", ", round(funnel.asym.p.rank.n.o., digits = 4), sep = ""
            ),
            align="right",
            collapsible = TRUE
          )
        ),
        
        fluidRow(
          box(
            title = "Sensitivity Dataframe",
            width = 12,
            class = "well",
            tableOutput(paste("sens.df.rand.fix.", outcome.alias, sep = "")),
            align="right",
            collapsible = TRUE
          )
        )
      ),
      
# Single Regressions
  # (without outliers)
      tabPanel(
        regression.single.with.outliers.tab.name,
        set.moderator.rows.w.o.(meta.df.list, outcome, preferred.scale, outcome.alias, sig.0.05.color, sig.0.1.color)
      ),
      
      tabPanel(
        regression.single.comp.tab.name,
        set.moderator.rows.comp(meta.df.list, outcome, preferred.scale, outcome.alias, sig.0.05.color, sig.0.1.color)
      ),
      
      tabPanel(
        regression.single.comp.tab.name.no.mean.r,
        set.moderator.rows.comp(meta.df.list, outcome, preferred.scale, outcome.alias, sig.0.05.color, sig.0.1.color, without.mean.r = T)
      ),
      
# Network Analysis
      tabPanel(
        net.meta.tab.name,
        fluidRow(
          box(
            title = "Network Plot",
            footer = if(is.logical(net.res)){""}else{paste(
              "Studies = ", net.res$k, "; Pairwise comparisons = ", net.res$m, "; Treatments = ", net.res$n, "; Designs", net.res$d,
              "; tau2 = ", round(net.res$tau2, digit = 4), "; I2 = ", round(net.res$I2, digit = 2), " [", round(net.res$lower.I2, digit = 2), ", ", round(net.res$upper.I2, digit = 2), "]",
              "; Total Q = ", round(net.res$Q, digit = 2), "; df = ", round(net.res$df.Q, digit = 2), "; paval = ", round(net.res$pval.Q, digit = 4),
              "; Hetero Q = ", round(net.res$Q.heterogeneity, digit = 2), "; df = ", round(net.res$df.Q.heterogeneity, digit = 2), "; paval = ", round(net.res$pval.Q.heterogeneity, digit = 4),
              "; Incons Q = ", round(net.res$Q.inconsistency, digit = 2), "; df = ", round(net.res$df.Q.inconsistency, digit = 2), "; paval = ", round(net.res$pval.Q.inconsistency, digit = 4),
              sep = ""
            )},
            width = 6,
            class = "well",
            plotOutput(paste("network.", outcome.alias, sep = "")),
            align="center",
            collapsible = TRUE
          ),
          box(
            title = "Network Forest Plot",
            width = 6,
            class = "well",
            plotOutput(paste("net.forest.", outcome.alias, sep = "")),
            collapsible = TRUE
          )
        ),
        
        fluidRow(
          box(
            title = "Direct Evidence Plot",
            width = 8,
            class = "well",
            plotOutput(paste("net.dir.evidence.", outcome.alias, sep = "")),
            align="center",
            collapsible = TRUE
          ),
          box(
            title = "Netheat Plot",
            width = 4,
            plotOutput(paste("net.heat.", outcome.alias, sep = "")),
            class = "well",
            align = "center",
            collapsible = TRUE
          )
        )
      ),
      fluidRow(
        box(
          title = "P-val table",
          width = 12,
          class = "well",
          tableOutput(paste("net.p.df.", outcome.alias, sep = "")),
          align="right",
          collapsible = TRUE,
          collapsed = TRUE
        )
      )
    )
  )
}
### Create summary pages for the ui
set.summary.page <- function(net.res.object, domain_name, tabName) {

  # Check if net.res.object is a netmeta object
    if (!inherits(net.res.object, "netmeta")) {
      stop("Error: net.res.object must be a netmeta object")
    }

  # Define number of included studies
  included_studies_per_model <- net.res.object$studlab %>%
    # In case, there is one study with the same treatment comparison occuring multiple times in the model
    # (e.g., because of multiple outcomes for this treatment comparison), there was added a label such as " #1#"
    # we remove these labels to count unique studies only
    str_remove_all(" #\\d+#") %>%
    unique()
  n_included_studies_per_model <- length(included_studies_per_model)

  # Return the tabItem with the tabsetPanels and tabPanels
  tabItem(
    tabName = tabName,
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Summary without outliers",
        fluidRow(
          box(
            title = "Summary Forest Plot",
            width = 12,
            class = "well",
            plotOutput(paste0("forest.summary.", domain_name)),
            align="center",
            collapsible = TRUE
          )
        )
      ),
      tabPanel(
        "Comparison with/without outliers",
        fluidRow(
          box(
            title = "Summary Forest Plot (with outliers)",
            width = 12,
            class = "well",
            plotOutput(paste0("forest.summary.o..", domain_name)),
            align="center",
            collapsible = TRUE
          )
        ),
        fluidRow(
          box(
            title = "Summary Forest Plot (without outliers)",
            width = 12,
            class = "well",
            plotOutput(paste0("forest.summary.n.o..", domain_name)),
            align="center",
            collapsible = TRUE
          )
        )
      ),
      tabPanel(
        "Network Analysis",
        fluidRow(
          box(
            title = "Network Plot",
            footer = paste(
              "N included unique studies = ", n_included_studies_per_model,
              "; Inflated study count (mixture of number of outcomes, actual studies, and split multi-arm designs) = ", net.res.object$k,
              "; Pairwise comparisons = ", net.res.object$m,
              "; Treatments = ", net.res.object$n, "; Designs = ", net.res.object$d,
              "; tau2 = ", round(net.res.object$tau2, digit = 4), "; I2 = ", round(net.res.object$I2, digit = 2), " [", round(net.res.object$lower.I2, digit = 2), ", ", round(net.res.object$upper.I2, digit = 2), "]",
              "; Total Q = ", round(net.res.object$Q, digit = 2), "; df = ", round(net.res.object$df.Q, digit = 2), "; paval = ", round(net.res.object$pval.Q, digit = 4),
              "; Hetero Q = ", round(net.res.object$Q.heterogeneity, digit = 2), "; df = ", round(net.res.object$df.Q.heterogeneity, digit = 2), "; paval = ", round(net.res.object$pval.Q.heterogeneity, digit = 4),
              "; Incons Q = ", round(net.res.object$Q.inconsistency, digit = 2), "; df = ", round(net.res.object$df.Q.inconsistency, digit = 2), "; paval = ", round(net.res.object$pval.Q.inconsistency, digit = 4),
              "; \nlist of included studies: \n'", paste(included_studies_per_model, collapse = "', '"), "'",
              sep = ""
            ),
            width = 6,
            class = "well",
            plotOutput(paste0("network.all.", domain_name)),
            align="center",
            collapsible = TRUE
          ),
          box(
            title = "Network Forest Plot",
            # background = if(funnel.asym.p.egger <= 0.05){sig.0.05.color}else{NULL},
            # footer = "",
            width = 6,
            class = "well",
            plotOutput(paste0("net.forest.all.", domain_name)),
            collapsible = TRUE
          )
        ),

        fluidRow(
          box(
            title = "Direct Evidence Plot",
            width = 8,
            class = "well",
            plotOutput(paste0("net.dir.evidence.all.", domain_name)),
            align="center",
            # style = forest.plot.box.height.Stress,
              # adjust height of grey box around the plot, as the high number of studies hinders shiny to recognize the actual plot size
            collapsible = TRUE
          ),
          box(
            title = "Netheat Plot",
            width = 4,
            plotOutput(paste0("net.heat.all.", domain_name)),
            class = "well",
            align = "center",
            # style = forest.plot.box.height.Stress,
            collapsible = TRUE
          )
        ),
        fluidRow(
          box(
            title = "P-val table",
            width = 12,
            class = "well",
            tableOutput(paste0("net.p.df.all.", domain_name)),
            align="right",
            collapsible = TRUE,
            collapsed = TRUE
          )
        )
      ),
      tabPanel(
        "Node Splitting Forest Plot",
        fluidRow(
          box(
            title = "Node Splitting Forest Plot",
            width = 12,
            class = "well",
            plotOutput(paste0("netsplit.summary.", domain_name), height = "1700px"),
            align="center",
            collapsible = TRUE
          )
        )
      )
    )
  )
}
