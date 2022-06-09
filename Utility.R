######################
## UTILITY FUNCTIONS #
######################

## Winsorize function from the DescTools package;
## it is copied here in case we do not have access to the package
Winsorize <- function(x, minval = NULL, maxval = NULL,
                      probs=c(0.05, 0.95), na.rm = FALSE, type=7) {
  
  if(is.null(minval) || is.null(maxval)){
    xq <- quantile(x=x, probs=probs, na.rm=na.rm, type=type)
    if(is.null(minval)) minval <- xq[1L]
    if(is.null(maxval)) maxval <- xq[2L]
  }
  
  x[x<minval] <- minval
  x[x>maxval] <- maxval
  
  return(x)
}


## function that computes the difference between two subsequent values
compute.difference <- function(values) {
  require(dplyr)
  abs(values - dplyr::lag(values))
}

## function that provides summary statistics about clusters
summary.stats <- function(feature.set, clusters, cl.num) {
  sum.stats <- aggregate(x = feature.set, 
                         by = list(clusters), 
                         FUN = function(x) { 
                           m <- mean(x, na.rm = T)
                           sd <- sqrt(var(x, na.rm = T))
                           paste(round(m, digits = 2), " (", 
                                 round(sd, digits = 2), ")", sep = "")
                         })
  sum.stat.df <- data.frame(cluster = sum.stats[,1], 
                            freq = as.vector(table(clusters)),
                            sum.stats[,-1])
  
  sum.stats.transpose <- t( as.matrix(sum.stat.df) )
  sum.stats.transpose <- as.data.frame(sum.stats.transpose)
  attributes <- rownames(sum.stats.transpose)
  sum.stats.transpose <- as.data.frame( cbind(attributes, sum.stats.transpose) )
  colnames(sum.stats.transpose) <- c( "attributes", rep("Mean (SD)", cl.num) )
  rownames(sum.stats.transpose) <- NULL
  sum.stats.transpose
}


## The following two functions are for creating and arranging a set of
## box plots, one plot for each attribute that was used for clustering.
## The purpose is to visually compare the distribution of the attributes
## across the clusters
# 
## The 'main' function is create_comparison_plots() that receives 2 input parameters:
## - a data frame with the attributes used for clustering
## - a factor variable with cluster assignments
## The function creates and arranges box plots for all the attributes.
# 
## The create_attr_boxplot() is a helper function for the create_comparison_plots() f.
## that creates a box plot for one attribute
create_attr_boxplot <- function(df, attribute, clust_var) {
  ggplot(data = df,
         mapping = aes(x=.data[[clust_var]], 
                       y=.data[[attribute]], 
                       fill=.data[[clust_var]])) +
    geom_boxplot() + 
    labs(y = attribute, x = "") +
    theme_classic()
}

create_comparison_plots <- function(df, clust) {
  require(dplyr)
  require(ggpubr)
  
  df_clust <- df
  df_clust[['cluster']] <- clust
  boxplots <- lapply(colnames(df), 
                     function(x) create_attr_boxplot(df_clust, x, 'cluster'))
  
  ggarrange(plotlist = boxplots,
            ncol = 3, nrow = 2,
            common.legend = TRUE, legend = "bottom",
            vjust = 1, hjust = -1, font.label = list(size=12))
  
}

