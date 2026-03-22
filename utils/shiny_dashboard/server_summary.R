# The script contains a utility fucntion for a shiny dashboard that generates summary server outputs for a domain, including forest plots,
# network analysis plots (netgraph, forest, direct evidence, heat maps), p-value tables, and node-splitting analyses.


### Set summary domain outputs for server
return.summary.output <- function(output, net.res.object, domain_name, outcome_vec) {
  # output for summary section (all outcomes included)
  output[[paste0("forest.summary.", domain_name)]] <- renderPlot(plot.summary.forest(net.res.object, outcome_vec = outcome_vec))
  output[[paste0("forest.summary.o..", domain_name)]] <- renderPlot(plot.summary.forest(net.res.object, outcome_vec = outcome_vec))
  output[[paste0("forest.summary.n.o..", domain_name)]] <- renderPlot(plot.summary.forest(net.res.object, outcome_vec = outcome_vec, with.outliers = F))
  
  output[[paste0("network.all.", domain_name)]] <- renderPlot({
    net.meta.analyze(
      outcome_vec, preferred.scale = F, net.df = F, net.res = net.res.object,
      plot.netgraph = T, plot.forest = F, plot.direct.evidence = F, plot.netheat = F,
      return.data = F, reference.group = "passive control", random = T
    )
  })
  output[[paste0("net.forest.all.", domain_name)]] <- renderPlot({
    net.meta.analyze(
      outcome_vec, preferred.scale = F, net.df = F, net.res = net.res.object,
      plot.netgraph = F, plot.forest = T, plot.direct.evidence = F, plot.netheat = F,
      return.data = F, reference.group = "passive control", random = T
    )
  })
  output[[paste0("net.dir.evidence.all.", domain_name)]] <- renderPlot({
    net.meta.analyze(
      outcome_vec, preferred.scale = F, net.df = F, net.res = net.res.object,
      plot.netgraph = F, plot.forest = F, plot.direct.evidence = T, plot.netheat = F,
      return.data = F, reference.group = "passive control", random = T
    )
  })
  output[[paste0("net.heat.all.", domain_name)]] <- renderPlot({
    net.meta.analyze(
      outcome_vec, preferred.scale = F, net.df = F, net.res = net.res.object,
      plot.netgraph = F, plot.forest = F, plot.direct.evidence = F, plot.netheat = T,
      return.data = F, reference.group = "passive control", random = T
    )
  })
  
  output[[paste0("net.p.df.all.", domain_name)]] <- renderTable(data.frame(net.res.object$pval.random), digits = 4)

  net.res.split <- netsplit(net.res.object)
  output[[paste0("netsplit.summary.", domain_name)]] <- renderPlot({plot(net.res.split)})
  
  return(output)
}
