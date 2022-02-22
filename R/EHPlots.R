
#Needs Work -
EH_OutlierBoxplotIt_Horizontal <-function(dataframe, colWithOutliers, colWithCategories)
{
  q1<-ggplot(dataframe, aes(x=colWithCategories, y=colWithOutliers)) +
    geom_boxplot() +
    ggtitle("Boxplots of Employees By Industry") +
    scale_x_discrete(limits = rev) +
    coord_flip()

  dataframe_WithoutOutliers<-EH_RemoveOutliersBasedOnQuartile(dataframe,colWithOutliers)

  q2<-ggplot(dataframe_WithoutOutliers, aes(x=colWithCategories, y=colWithOutliers)) +
    geom_boxplot() +
    ggtitle("Boxplots of Employees By Industry - Outliers Removed") +
    scale_x_discrete(limits = rev) +
    coord_flip()

grid.arrange(q1, q2, ncol = 2)
}
