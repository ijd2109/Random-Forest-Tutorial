vimPlot = function(model) {
  impFrame = as.data.frame(randomForest::importance(model))
  names(impFrame)[1] = "importance"
  # the first column is MSE increase (for reg; analogous for classification)
  impFrame = impFrame %>%
    mutate(variable = rownames(.),
           impSD = model$importanceSD) %>%
    # by default importances are "scaled" by their SD, so return them to units reflecting % change in MSE
    mutate(importance = importance * impSD) %>%
    arrange(desc(importance))
  # set the variable labels for plotting aesthetics
  impFrame$variable = factor(impFrame$variable, levels = rev(impFrame$variable))
  
  plt = ggplot(impFrame) + 
    geom_bar(stat='identity', aes(x = variable, y = importance),fill="lightgreen") +
    geom_errorbar(aes(x = variable, y = importance,
                      ymin = importance - 2*impSD, ymax = importance + 2*impSD)) +
    coord_flip() +
    ggtitle("Variable Importances within 95% Bootsrap confidence intervals")

  print(plt)
}