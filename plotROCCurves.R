ggplot(learnersResults$rocResults,
    aes(x = 1 - specificity, y = sensitivity)) +
    geom_path(aes(color = ML, linetype = predSet)) +
    geom_abline(color = 'black') +
    coord_equal() +
    theme_bw() +
    ggtitle('ROC Curve')
