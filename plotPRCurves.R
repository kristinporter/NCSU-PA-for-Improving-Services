ggplot(learnersResults$prResults,
    aes(x = recall, y = precision)) +
    geom_path(aes(color = ML, linetype = predSet)) +
    geom_hline(yintercept = learnerSpec$positiveProportion, color = "black") +
    geom_text(data = data.frame(x = 0.25, 
                                y = learnerSpec$positiveProportion), 
              aes(x,y), 
              label = "No skills classifier", 
              vjust = 1,
              size = 3) +
    coord_equal() +
    theme_bw() +
    ggtitle('Precision-Recall Curve')
