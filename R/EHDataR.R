library(devtools)
library(roxygen2)
library(Hmisc)
library(psych)
library(tidyverse)
library(skimr)
library(purrr)
library(tidyr)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(fastDummies)
library(data.table)
library(mltools)
library(MASS)
library(car)
library(patchwork)
library(ggthemes)
library(tinytex)
library(stats)
library(ggsci)

EHTheme <- function(){
  
  x <- theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 9), axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(), panel.grid.major.y=element_line(color="gray"), panel.background = element_rect(fill = "slategray2", color="darkslategray"))
  
  return (x)
  
}


EHPrepare_MissingValues_Imputation <- function(df, y)
{
  
  #1. Missing Completely at Random (MCAR):
  #2. Missing at Random (MAR):
  #3. Missing Not at Random (MNAR)
  
  dfImputedMean <- data.frame(
    sapply(df, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))
  
  dfImputedMedian <- data.frame(
    sapply(df, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)))
  
  dfOmit <- na.omit(df)
  
  fla <- substitute(n ~ ., list(n = as.name(y)))
  m1 <- lm(fla, dfImputedMean)
  step1 <- stepAIC(m1, trace=FALSE)
  s1 <- summary(step1)$adj.r.squared
  
  fla2 <- substitute(n ~ ., list(n = as.name(y)))
  m2 <- lm(fla2, dfImputedMedian)
  step2 <- stepAIC(m2, trace=FALSE)
  s2 <- summary(step2)$adj.r.squared
  
  fla3 <- substitute(n ~ ., list(n = as.name(y)))
  m3 <- lm(fla3, dfOmit)
  step3 <- stepAIC(m3, trace=FALSE)
  s3 <- summary(step3)$adj.r.squared
  
  l1 <- vector(mode = "list", length = 5)
  names(l1) <- c("df", "type", "r2mean", "r2median", "r2omit")
  
  l1$r2mean = s1
  l1$r2median = s2
  l1$r2omit = s3
  
  if (s1>=s2) {
    l1$type = "mean"
    l1$df=dfImputedMean
    
    print(c("type:", l1$type))
    print(c("r2mean:", round(l1$r2mean,4)))
    print(c("r2median:", round(l1$r2median,4)))
    print(c("r2omit", round(l1$r2omit,4)))
    
    return (l1$df)
  }
  else {
    l1$type = "median"
    l1$df=dfImputedMedian
    
    
    print(c("type:", l1$type))
    print(c("r2mean:", round(l1$r2mean,4)))
    print(c("r2median:", round(l1$r2median,4)))
    print(c("r2omit", round(l1$r2omit,4)))
    
    return (l1$df)
  }
}

EHExplore_Interactions_Scatterplots <- function(df, y, interaction) {
  
  df <- select_if(df, is.numeric)
  
  df[,interaction] <- as.factor(df[,interaction])
  
  plot_list <- list()
  
  for(i in 1:ncol(df)) {     
    
    p <- eval(substitute(ggplot(df, aes_string(df[ , i], y, color=interaction)) +
                           geom_point(alpha=.1) +
                           geom_smooth(method = "lm") +
                           xlab("") +
                           theme(title = element_text(size=7), axis.title.x = element_text(size = 7), axis.title.y = element_text(size = 9), axis.text.x = element_text(size = 8), panel.grid.major.x = element_line(color="gray"), panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(), panel.grid.major.y=element_line(color="gray"), panel.background = element_rect(fill = "slategray1", color="darkslategray")) +
                           scale_color_d3()+
                           scale_fill_d3()+
                           ggtitle(colnames(df)[i]), list(i=i)))
    plot_list[[i]] <- p 
    
  }
  return(plot_list)
}

EHSummarize_SingleColumn_Boxplots <- function(df, font_size=7)
{  
  df <- select_if(df, is.numeric)


plot_list2 <- list()

for(i in 1:ncol(df)) {     
  
  qp <- toString(head(sort(round(df[,i],2)),5))
  qz <- toString(tail(sort(round(df[,i],2)),5))
  qk <- str_c("L:   ", qp, "\\\n", "H:   ", qz)
  
  qk <- gsub('\\\\','', qk)
  
  p <- eval(substitute(ggplot(df, aes(df[,i])) +
                         coord_flip() +  
                         xlab(colnames(df)[i])  +
                         ylab(qk) +
                         theme(axis.title.x = element_text(size = font_size), axis.title.y = element_text(size = 9), axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(), panel.grid.major.y=element_line(color="gray"), panel.background = element_rect(fill = "slategray2", color="darkslategray")) +
                         geom_boxplot(), list(i=i)))
  
  plot_list2[[i]] <- p 
  
  
}
return (plot_list2)
}

EHExplore_OneContinuousAndOneCategoricalColumn_Boxplots <- function(df, x)
{  

  df <- select_if(df, is.numeric)
  df[,x] <- as.factor(df[,x])
  
  plot_list3 <- list()
  
  for(i in 1:ncol(df)) {
    
    p <- ggplot(df, aes_string(y=df[,i], x=x, fill=x)) +
                           xlab(x)  +
                           ylab(colnames(df)[i]) +
                           theme(axis.title.x = element_text(size = 9), axis.title.y = element_text(size = 9), panel.grid.major.x = element_blank(), panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(), panel.grid.major.y=element_line(color="gray"), panel.background = element_rect(fill = "slategray1", color="darkslategray")) +
                            scale_color_d3()+
                            scale_fill_d3()+                     
                            geom_boxplot()
    
    
    plot_list3[[i]] <- eval(substitute(p, list(i=i)))
    
    
  }
  return (plot_list3)
}

#dfTrain <- read.csv("C:\\Users\\erico\\Documents\\R\\CUNY_621\\Baseball\\moneyball-training-data.csv", header=TRUE)
#dfTrain <- dfTrain %>%
#  mutate(xq = ifelse(TEAM_PITCHING_H >1500, 1, 0))
#EHExplore_Correlations_Boxplots(dfTrain, "xq")

EHSummarize_SingleColumn_Histograms <- function(df, font_size = 7, hist_nbins = 20)
{
  
  df <- select_if(df, is.numeric)
  
  plot_list2 <- list()
  
  for(i in 1:ncol(df)) {     
    
    qp <- toString(head(sort(round(df[,i],2)),5))
    qz <- toString(tail(sort(round(df[,i],2)),5))
    qk <- str_c("L:   ", qp, "\\\n", "H:   ", qz)
    
    qk <- gsub('\\\\','', qk)
    
    p <- eval(substitute(ggplot(df, aes(df[,i])) +
                           ylab(colnames(df)[i])  +
                           xlab(qk) +
                           theme(axis.title.x = element_text(size = font_size), axis.title.y = element_text(size = 9), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.text.x = element_text(size=8),  panel.grid.major.x = element_blank(), panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), panel.background = element_rect(fill = "slategray2", color="darkslategray"))  + 
                           geom_histogram(bins=hist_nbins, fill="white", aes(y = stat(density))) +
                           geom_density(col = "red"), list(i=i)))
    plot_list2[[i]] <- p 
    
  }
  return (plot_list2)
}


EHExplore_TwoContinuousColumns_Scatterplots <- function(df, y, flip=FALSE)
{
  plot_list <- list()
  
  df <- select_if(df, is.numeric)
  
  for(i in 1:ncol(df)) {
    
    ct <- cor.test(df[,i], df[,y])
    
    xText <- str_c("Correlation: ", round(ct$estimate,2), "   p value: ", round(ct$p.value,2))
    
    x1 = df[[i]]
    y1 =y
    
    if(flip)
    {
      x1=y
      y1=df[[i]]
    }
    
    p <- ggplot(df, aes_string(x1, y1)) +
      geom_point(fill="navy", color="white") +
      geom_smooth(method = "loess", color="red", fill="lightcoral") +
      ylab(y) +
      xlab(xText) +
      theme(title = element_text(size=9), axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 9), axis.text.x = element_text(size = 8), axis.ticks.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(), panel.grid.major.y=element_line(color="gray"), panel.background = element_rect(fill = "slategray2", color="darkslategray")) +
      ggtitle(colnames(df)[i])
    
    p <- eval(substitute(p, list(i=i)))
    plot_list[[i]] <- p 
    
  }
  return(plot_list)
}


EHSummarize_StandardPlots <-function(data, y, return_list = FALSE, h_nbins = 20, print=TRUE)
{  
  
  list1 <- EHExplore_Outliers_Boxplots(data)
  list2 <- EHExplore_Distributions_Histograms(data, hist_nbins =  h_nbins)
  list3 <- EHExplore_Correlations_Scatterplots(data, y)
  
  zz2 <- list()
  
  
  
  for(i in 1:length(list1)) {
    zz2[i*3-2] <- list1[i]
    zz2[i*3-1] <- list2[i]
    zz2[i*3] <- list3[i]
  }
  
  if (print) {
    lenZ <- length(zz2)
    quotient <- lenZ %/% 9
    gap <- lenZ - quotient*9
    gaprows <- gap/3
    
    
    for(i in 1:quotient) { 
      
      start <- (i-1)*9 + 1
      finish <- start + 8
      
      grid.arrange(grobs=zz2[c(start:finish)], ncol=3)
      
    }
    
    if (gaprows>0) {
      
      start <- quotient*9 + 1
      finish <- start + gaprows*3 - 1
      
      grid.arrange(grobs=zz2[c(start:finish)], ncol=3, nrow=gaprows)
    }  
  }
  
  if (return_list) {
    return (zz2)
  }
  
}

EHExplore_Multicollinearity <-function(df, run_all=FALSE, title="Heatmap for Multicollinearity Analysis") {
  
  dfCor <- as.data.frame(cor(df))
  
  library(corrplot)
  my_matrix <- df[]
  cor_res <- cor(my_matrix, use = "na.or.complete")
  
  if (run_all) {
    pairs.panels(df)
    print(dfCor)
    corrplot(cor_res, method = 'number')
  }
  
  library(corrplot)
  my_matrix <- df[]
  cor_res <- cor(my_matrix, use = "na.or.complete")
  
  z <- corrplot(cor_res, title = title, mar=c(0,0,2,0), 
                diag=FALSE, type = "upper", order = "original", tl.col = "black", tl.srt = 45, tl.cex = 0.55)
  
  #return (z)
  
}


EHModel_Regression_StandardLM <- function(df, y) {
  
  fla <- substitute(n ~ ., list(n = as.name(y)))
  
  par(mfcol=c(2,2))
  
  mod_4 <- lm(fla, df)
  step3 <- stepAIC(mod_4, trace=FALSE)
  print(summary(step3))
  
  print("VIF Analysis")
  vif_values <- car::vif(step3)
  print(vif_values)
  
  print(plot(step3))
  
  return(step3)
  
}

EHExplore_TwoCategoricalColumns_Barcharts <- function(df, y)
  {
  
  plot_list4 <- list()
  
  df <- select_if(df, is.numeric)

  
  for(i in 1:ncol(df)) {
    
    plotdata <- df %>%
      group_by(df[, 1], y) %>%
      summarize(n = n()) %>% 
      mutate(pct = n/sum(n),
             lbl = scales::percent(pct))
    plotdata
    
    p <- ggplot(plotdata, aes_string(df[ , i], pct, fill=y)) +
      geom_bar(stat = "identity",
               position = "fill") + +
      ylab(y) +
      xlab(xText) +
      theme(title = element_text(size=9), axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 9), axis.text.x = element_text(size = 8), axis.ticks.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(), panel.grid.major.y=element_line(color="gray"), panel.background = element_rect(fill = "slategray2", color="darkslategray")) +
      ggtitle(colnames(df)[i]) + EHTheme()
    
    p <- eval(substitute(p, list(i=i)))
    plot_list[[i]] <- p 
  }
 
  return (plot_list4)
}

