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
library(scales)
library(naniar)
#library(Amelia)
library(caret)
library(pROC)

EHTheme <- function(){
  
  x <- theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 9), axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(), panel.grid.major.y=element_line(color="gray"), panel.background = element_rect(fill = "slategray2", color="darkslategray"))
  
  return (x)
  
}

EH_Theme_Histogram <- function(font_size=7, hist_nbins=30){
  
  #Example of Usage:
  # ggplot(dfErrors, aes(x=residuals)) +
  #  ggtitle("Distribution of Residuals for Decision Tree") +
  #  q$geom_histogram +
  #  q$theme_histogram +
  #  q$density_Histogram
  
  theme_histogram <- theme(axis.title.x = element_text(size = font_size), axis.title.y = element_text(size = 9), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.text.x = element_text(size=8),  panel.grid.major.x = element_blank(), panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), panel.background = element_rect(fill = "slategray2", color="darkslategray"))
    
  geom_histogram <- geom_histogram(bins=hist_nbins, fill="white", aes(y = stat(density))) 
    
  density_histogram <- geom_density(col = "red")

  newList <- list("theme_histogram" = theme_histogram, "geom_histogram" = geom_histogram, "density_Histogram" = density_histogram)
  return(newList)
  
}


EHSummarize_MissingValues <- function(df)
{

  library(naniar)
  
  #1. Missing Completely at Random (MCAR):
  #2. Missing at Random (MAR):
  #3. Missing Not at Random (MNAR)
  
  list12 = list()
  
  list12[[1]] <- gg_miss_var(df)
  list12[[2]] <- vis_miss(df)
  list12[[3]] <- gg_miss_upset(df)
  
  return(list12)

  
}


EHPrepare_MissingValues_Imputation <- function(df, y="", impute = "mean", print_all = FALSE)
{
  
  #1. Missing Completely at Random (MCAR):
  #2. Missing at Random (MAR):
  #3. Missing Not at Random (MNAR)
    
    dfImputedMean <- df

    for(i in colnames(df))
      if(is.numeric(df[,i])){
        meanv <- mean(df[,i], na.rm = TRUE)  
        dfImputedMean[,i][is.na(df[,i])] <- meanv
      }
    

  
    dfImputedMedian <- df
    
    for(i in colnames(df))
      if(is.numeric(df[,i])){
        medianv <- median(df[,i], na.rm = TRUE)  
        dfImputedMedian[,i][is.na(df[,i])] <- medianv
      }
    
    if(y==""){
      if(impute=="mean"){
        return(dfImputedMean)
      } else if (impute=="median"){
        return(dfImputedMedian)
      }
    }
    
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

  
  if (impute == "mean") {
    l1$type = "mean"
    l1$df=dfImputedMean
  }
  else if (impute == "median") {
    l1$type = "median"
    l1$df=dfImputedMedian
  }
  else if (impute == "omit") {
    l1$type = "omit"
    l1$df=dfOmit
  }
  
  
  print(c("type:", l1$type))
  print(c("r2mean:", round(l1$r2mean,4)))
  print(c("r2median:", round(l1$r2median,4)))
  print(c("r2omit", round(l1$r2omit,4)))
  
    if (print_all) {
      print(summary(step1))
      print(summary(step2))
      print(summary(step3))
    }
  
    return (l1$df)
}

EHExplore_Interactions_Scatterplots <- function(df, y, interaction) {
  
  #If you get these Errors:
  #"Error: Unknown input: tbl_df' = you probably did not pass it a proper dataframe (probably a tibble instead)
  
  library(ggsci)
  
  df <- select_if(df, is.numeric)
  
  v <- as.vector(df[,interaction])

  xtext1 = as.data.frame(aggregate(data.frame(count = v), list(value = v), length))
  df[interaction][df[interaction] == "0"] <- paste0("0 (n=", xtext1$count[1], ")")
  df[interaction][df[interaction] == "1"] <- paste0("1 (n=", xtext1$count[2], ")")
  
  
  df[,interaction] <- as.factor(df[,interaction])
  
  plot_list <- list()
  
  for(i in 1:ncol(df)) {     
    
    p <- eval(substitute(ggplot(df, aes_string(df[ , i], y, color=interaction)) +
                           geom_point(alpha=.1) +
                           geom_smooth(method = "lm") +
                           xlab(colnames(df)[i]) +
                           theme(title = element_text(size=9), axis.title.x = element_text(size = 9), axis.title.y = element_text(size = 9), axis.text.x = element_text(size = 8), panel.grid.major.x = element_line(color="gray"), panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(), panel.grid.major.y=element_line(color="gray"), panel.background = element_rect(fill = "slategray1", color="darkslategray")) +
                           scale_color_d3()+
                           scale_fill_d3()+
                           ggtitle(colnames(df)[i]), list(i=i)))
    plot_list[[i]] <- p 
    
  }
  return(plot_list)
}

EHSummarize_SingleColumn_Countplots <- function(df, font_size=7)
{  
  df <- select_if(df, is.character)
  
  
  plot_list2 <- list()
  
  for(i in 1:ncol(df)) {     
    
    p <- eval(substitute(ggplot(df, aes(df[,i])) +
                           coord_flip() +  
                           xlab(colnames(df)[i])  +
                           #ylab(qk) +
                           theme(axis.title.x = element_text(size = font_size), axis.title.y = element_text(size = 9), axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(), panel.grid.major.y=element_line(color="gray"), panel.background = element_rect(fill = "slategray2")) +
                           #geom_text(size = 3, position = position_stack(vjust = 0.5)) +
                           #geom_bar(stat = "identity", col="white", border="black"), list(i=i)))
                           geom_bar(color="black", fill="white"), list(i=i)))
    
    plot_list2[[i]] <- p 
    
    
  }
  return (plot_list2)
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


EHExplore_OneContinuousAndOneCategoricalColumn_Boxplots <- function(df, y, yCategorical=TRUE)
{
  plot_list3 <- list()
  
  df <- select_if(df, is.numeric)
  
  df$NumericY <- as.numeric(df[,y])
  
  if(yCategorical){
    
  df[,y] <- as.factor(df[,y])
  
  #v <- as.vector(df[,y])
  #xtext1 = as.data.frame(aggregate(data.frame(count = v), list(value = v), length))
  #df[y][df[y] == "0"] <- paste0("0 (n=", xtext1$count[1], ")")
  #df[y][df[y] == "1"] <- paste0("1 (n=", xtext1$count[2], ")")
  
  }
  
  for(i in 1:ncol(df)) {
    
    df$NumericX <- as.numeric(df[,i])
    
    ct <- cor.test(df$NumericX, df$NumericY)
    
    xText <- str_c("Correlation: ", round(ct$estimate,2), "   p value: ", round(ct$p.value,2))
    
    
    x1 = df[[i]]
    y1 =y
    
    if(!yCategorical)
    {
      
      x1=y
      y1=as.factor(df[[i]])
      
      #v <- as.vector(df[,i])
      #xtext1 = as.data.frame(aggregate(data.frame(count = v), list(value = v), length))
      #df[i][df[i] == "0"] <- paste0("0 (n=", xtext1$count[1], ")")
      #df[i][df[i] == "1"] <- paste0("1 (n=", xtext1$count[2], ")")
    }
    
    p <- ggplot(df, aes_string(x1, y1, fill=y1)) +
      xlab(colnames(df)[i])  +
      ylab(xText) +
      theme(axis.title.x = element_text(size = 9), axis.title.y = element_text(size = 9), panel.grid.major.x = element_blank(), panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(), panel.grid.major.y=element_line(color="gray"), panel.background = element_rect(fill = "slategray1", color="darkslategray")) +
      scale_color_d3()+
      scale_fill_d3()+                     
      geom_boxplot()+
      coord_flip() 
    
    plot_list3[[i]] <- eval(substitute(p, list(i=i)))
    
  }
  return(plot_list3)
}


EHSummarize_StandardPlots <-function(data, y, return_list = FALSE, h_nbins = 20, print=TRUE, type="scatter")
{  
  
  list1 <- EHSummarize_SingleColumn_Boxplots(data)
  list2 <- EHSummarize_SingleColumn_Histograms(data, hist_nbins =  h_nbins)
  
  if(type=="scatter"){
    list3 <- EHExplore_TwoContinuousColumns_Scatterplots(data, y)
  } else if (type=="box"){
    list3 <- EHExplore_OneContinuousAndOneCategoricalColumn_Boxplots(data, y)
  }
  
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
    
    if (lenZ>=9) {
    for(i in 1:quotient) { 
      
      start <- (i-1)*9 + 1
      finish <- start + 8
      
      grid.arrange(grobs=zz2[c(start:finish)], ncol=3)
      
    }
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

EHExplore_Multicollinearity <-function(df, printCorrs=FALSE, printHeatMap = TRUE, printHighest=FALSE, threshold=.85,  title="Heatmap for Multicollinearity Analysis") {
  
  #To print out only what you want, set the function to a variable, i.e. x <- EHExplore_Multicollinearity
  
  dfCor <- as.data.frame(cor(df))
  
  library(corrplot)
  my_matrix <- df[]
  cor_res <- cor(my_matrix, use = "na.or.complete")
  
  if (printCorrs) {
    print(dfCor)
  }
  
  if (printHeatMap) {
  my_matrix <- df[]
  cor_res <- cor(my_matrix, use = "na.or.complete")
  
  z <- corrplot(cor_res, title = title, mar=c(0,0,2,0), 
                diag=FALSE, type = "upper", order = "original", tl.col = "black", tl.srt = 45, tl.cex = 0.55)
  }
  
  dfmm <- data.frame(col1=character(),
                     col2=character(),
                     correlation=double())
  
  mult2 <- as.data.frame(dfCor)
  
  for(i in 1:ncol(mult2)) {       # for-loop over columns
    for(j in 1:nrow(mult2)) {
      
      if((mult2[i,j] >threshold | mult2[i,j] < -1*threshold) & mult2[i,j] != 1){
        v <- c(colnames(mult2[i]), colnames(mult2[j]), mult2[i,j])
        dfmm <- rbind(dfmm, data.frame(col1 =colnames(mult2[i]), col2 = colnames(mult2[j]), correlation= 
        mult2[i,j], stringsAsFactors = FALSE))
        }
    }
  }

if (nrow(dfmm)>0){
  
    nrow1 <- nrow(dfmm)/2
    
  for (j in 1:nrow1){
    cl1 <- dfmm[j,1]
    cl2 <- dfmm[j,2]
    
    dfmm <- subset(dfmm, dfmm[,1]!=cl2 | dfmm[,2]!=cl1)
  }    
} else {
  dfmm[nrow(df) + 1,] = c("No Values", 0, 0)
}
  
    if (printHighest){
    print(dfmm)  
  }

 rlist <- list(dfCor, dfmm)
  return (rlist)
  
}


EHModel_Regression_StandardLM <- function(df, y, splitRatio=.8, xseed = 0, vif=TRUE, tests = TRUE, avplots = FALSE, xstepAIC=TRUE, returnLM=FALSE) {
  
  library(caTools)
  library(Metrics)
  
  
  if(xseed>0) {
    set.seed(xseed)
  }
  
  par(mfcol=c(2,2))
  fla <- substitute(n ~ ., list(n = as.name(y)))
  
  if(splitRatio==1) {
    mod_4 <- lm(fla, df)
  } else {
    split <- sample.split(df, SplitRatio = splitRatio)
    split
    
    train_reg <- subset(df, split == "TRUE")
    test_reg <- subset(df, split == "FALSE")
    mod_4 <- lm(fla, train_reg)
  }
  
  if(xstepAIC){
  step3 <- stepAIC(mod_4, trace=FALSE)
  } else {
    step3 <- mod_4
  }
  
  step3_summary <- summary(step3)
  print(step3_summary)
  
  if (vif){
  print("VIF Analysis")
  vif_values <- car::vif(step3)
  print(vif_values)
  }
  
  print(plot(step3))
  
  if (tests) {
  library(lmtest)
  print(bptest(step3))
  
  print(shapiro.test(step3$residuals))
  }
  
  if (avplots) {
    avPlots(step3)
  }
  
  print(paste("AIC: ", AIC(step3)))
  
  if (splitRatio==1){
    
    list_data <- c(step3, 0, 0, 0)
    
    if(!returnLM) {
      return(list_data)
    }else{
      return (step3)
    }

    
  } else {
    pred_linreg <- predict(step3,test_reg)
    resids <- test_reg[,y]-pred_linreg
    
    rmse1 <- rmse( test_reg[,y],pred_linreg)
    print(paste("RMSE on evaluation set: ", rmse1))
  }
  
  list_data <- c(step3, rmse1, step3_summary$sigma, resids)
  
  if(!returnLM) {
  return(list_data)
  }else{
  return (step3)
  }
}
  
EHModel_Regression_Robust <- function(df, y, splitRatio=.8, xseed = 0) {
  
  library(caTools)
  library(Metrics)
  
  if(xseed>0) {
    set.seed(xseed)
  }
  
  fla <- substitute(n ~ ., list(n = as.name(y)))
  fm <- as.formula(fla)
  
    split <- sample.split(df, SplitRatio = splitRatio)
    split
    
    train_reg <- subset(df, split == "TRUE")
    test_reg <- subset(df, split == "FALSE")
    
    m1 <- rlm(fm, train_reg)
    m1_summary <- summary(m1)
    print(m1_summary)
  
    pred_linreg <- predict(m1,test_reg)
    resids <- test_reg[,y]-pred_linreg
    
    rmse1 <- rmse( test_reg[,y],pred_linreg)
    print(paste("RMSE: ", rmse1))
    
    list_data <- list(c(m1), rmse1, m1_summary$sigma, resids)
    
    return(list_data)

}


EHExplore_TwoCategoricalColumns_Barcharts <- function(df, y)
{
  
  plot_list4 <- list()
  
  df <- df %>% select_if(function(x) is.character(x)|is.factor(x))
  
  df[,y] <- as.factor(df[,y])
  
  for(i in 1:ncol(df)) {
    
    
    df[,i] <- as.factor(df[ ,i])
    
    p <- ggplot(df, aes_string(x=df[ , i], fill=y)) +
      geom_bar(position = "fill") +
      ylab("Proportion") +
      xlab(colnames(df)[i]) +
      stat_count(geom="text", aes(label=stat(count)), position=position_fill(vjust=.5), color="black") +
      scale_color_d3()+
      scale_fill_d3()+
      theme(title = element_text(size=9), axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 9), axis.text.x = element_text(size = 8), panel.grid.major.x = element_blank(), panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(), panel.grid.major.y=element_line(color="gray"), panel.background = element_rect(fill = "slategray1", color="darkslategray")) +
      ggtitle(paste("Number and Proportion of ", y, " by ", names(df)[i])) + 
      coord_flip()
    
    p <- eval(substitute(p, list(i=i)))
    plot_list4[[i]] <- p
  }
  
  return (plot_list4)
}

EHModel_Regression_Logistic <-function(df, y, splitRatio = .8, xseed = 0, returnLM=FALSE)
{
  library(caTools)
  library(ROCR)
  
  if(xseed>0) {
    set.seed(xseed)
  }
  
  if(splitRatio==1) {
    fla <- substitute(n ~ ., list(n = as.name(y)))
    
    logistic_model <- glm(fla,
                          data = df,
                          family = "binomial")
    
    # Summary
    print(summary(logistic_model))
    
    listq = list()
    listq[1] <- logistic_model
    listq[2] <- 0
    listq[3] <- 0
    
    
    if(!returnLM) {
      return(listq)
    }else{
      return (logistic_model)
    }
  }
  
  split <- sample.split(df, SplitRatio = splitRatio)
  split
  
  train_reg <- subset(df, split == "TRUE")
  test_reg <- subset(df, split == "FALSE")
  
  fla <- substitute(n ~ ., list(n = as.name(y)))
  
  logistic_model <- glm(fla,
                        data = train_reg,
                        family = "binomial")
  
  # Summary
  print(summary(logistic_model))
  
  # Predict test data based on model
  predict_reg <- predict(logistic_model,
                         test_reg, type = "response")

  scored_class <- ifelse(predict_reg >0.5, 1, 0)
  class <- test_reg[,y]
  
  dfPred <- data.frame(class, scored_class)
  
  dfPred$class <- as.factor(dfPred$class)
  dfPred$scored_class <- as.factor(dfPred$scored_class)
  
  q <-confusionMatrix(data = dfPred$scored_class, reference = dfPred$class)
  print(q)
  
  dfPred_raw <- data.frame(class, predict_reg)
  
  roc(class ~ predict_reg, dfPred_raw)
  
roc1 <- roc(dfPred_raw$class,
              dfPred_raw$predict_reg, plot=TRUE)
xauc <- roc1$auc

print(roc1)

listq = list()
listq[1] <- logistic_model
listq[2] <- q$overall['Accuracy']
listq[3] <- logistic_model$aic
listq[4] <- xauc

if(!returnLM) {
  return(listq)
}else{
  return (logistic_model)
}

}


EHPrepare_ScaleAllButTarget <-function(df, y)
{
  
  df1 <- df %>%
    dplyr::select(-{{y}})
  
  df1 <- data.frame(scale(df1))
  df2 <- df %>%
    dplyr::select({{y}})
  
  df3 <- cbind(df1,df2)
  
  return(df3)
}

EHModel_Regression_Logistic_Iterations <- function(df, y, numOfIterations=100)
{
  
  acc = list()
  AIC = list()
  AUC = list()
  
  for (i in 1:numOfIterations)
  {
    q <- EHModel_Regression_Logistic(df, y)
    acc[i]=q[2]
    AIC[i]=q[3]
    AUC[i] = q[4]
  }
  
  accv <- unlist(acc)
  aveq <- mean(accv)
  
  aicv <- unlist(AIC)
  aicq <- mean(aicv)
  
  aucv <- unlist(AUC)
  aucq <- mean(aucv)
  
  print(paste("Accuracy: ", aveq))
  print(paste("AIC: ", aicq))
  print(paste("AUC: ", aucq))
  
}

EHModel_Regression_Standard_Iterations <- function(df, y, numOfIterations=100)
{
  
  rmse2 = list()
  rse = list()
  
  for (i in 1:numOfIterations)
  {
    q <- EHModel_Regression_StandardLM(df, y, xstepAIC=FALSE)
    rmse2[i]=q[2]
    rse[i]=q[3]
  }
  
  rsme2q <- unlist(rmse2)
  rsme2m <- mean(rsme2q)
  
  rsev <- unlist(rse)
  rsem <- mean(rsev)
  
  print(paste("Average RSME: ", rsme2m))
  print(paste("Average RSE: ", rsem))
  
  
}

EHModel_Regression_Robust_Iterations <- function(df, y, numOfIterations=100)
{
  
  rmse2 = list()
  rse = list()
  
  for (i in 1:numOfIterations)
  {
    q <- EHModel_Regression_Robust(df, y)
    rmse2[i]=q[2]
    rse[i]=q[3]
  }
  
  rsme2q <- unlist(rmse2)
  rsme2m <- mean(rsme2q)
  
  rsev <- unlist(rse)
  rsem <- mean(rsev)
  
  print(paste("Average RSME: ", rsme2m))
  print(paste("Average RSE: ", rsem))
  
}

EHPrepare_CreateDummies <- function(df, include=list(), exclude=list(), dropFirst=TRUE)
{
  
  library(tidytable)
  
  fact <- df %>%
    dplyr::select(is.factor|is.character)
  
  cols <- colnames(fact)
  
  if(length(include>0)){
    
    cols <- include
  }
  
  if(length(exclude>0)){
    
    cols <- cols[! cols %in% exclude]
  }
  
  df3 <- df %>%
    get_dummies.(cols,  drop_first = dropFirst, dummify_na=TRUE) %>%
    dplyr::select(-cols)
  
  df4 <- data.frame(df3) 
  
  return(df4)
  
}

EHPrepare_RestrictDataFrameColumnsToThoseInCommon <- function(df1, df2, exclude=list())
{
  
  library(janitor)
  cmp <- compare_df_cols(df1, df2)
  
  cmp_No1 <- cmp %>%
    dplyr::filter(is.na(df1))%>%
    dplyr::filter(!column_name %in% exclude)
  
  cmp_No1V <- cmp_No1$column_name
  
  df2R <- df2 %>%
    dplyr::select(!any_of(cmp_No1V))
  
  cmp_No2 <- cmp %>%
    dplyr::filter(is.na(df2)) %>%
    dplyr::filter(!column_name %in% exclude)
  
  cmp_No2V <- cmp_No2$column_name
  
  df1R <- df1 %>%
    dplyr::select(!any_of(cmp_No2V))
  
  
  rlist <- list(df1R, df2R)
  return(rlist)
  
}

EHPrepare_BoxCox <- function(df2, col, print=TRUE, newcol=FALSE)
{
  
  #For some reason boxcox fails if you use df as a parameter - so that's why it's df2
  
  hist(df2[,col])
  fla <- substitute(n ~ 1, list(n = as.name(col)))
  
  b <- boxcox(lm(fla, df2))
  lambda <- b$x[which.max(b$y)]
  df2[, col] <- (df2[,col] ^ lambda - 1) / lambda
  hist(df2[,col])
  return(df2)
  
}

EHModel_DecisionTree <- function(df4, target, seed=042760, levels=31, categorical=TRUE, printFancyTreeOnly=FALSE, printDTOnly=FALSE)
{
  #"Need to be the same factors" - Make sure to designate categorical=false if the targ123 is continuous
  # There are two trees - the tree from caret (train(formula, ...)) is what the rmse is based on.  
  # The other tree is not - it is also the one influenced by the number of levels.This is the 'fancy tree.'
  # I believe the fancy tree is also the one with all the stats.

  if (printDTOnly){
    printFancyTreeOnly=TRUE
  }
  
    targ123 = target
  
  if (categorical) {
    df4[, targ123] <- as.factor(df4[, targ123])
  } 
  
  fla <- substitute(n ~ ., list(n = as.name(targ123)))
  
  set.seed(seed)
  
  i <- createDataPartition(df4[,targ123], p=0.8, list=FALSE)
  
  dfEval <- df4[-i,]
  dfTrain <- df4[i,]
  
count(dfTrain[targ123])
  
  tc <- trainControl(method="cv", number=10)
  metric <- "Accuracy"
  
  
  library(rpart)
  
  levels2 = levels-1
  output.tree <- rpart(fla, data = dfTrain, control = rpart.control(maxdepth = levels2))
  
  
  library(rpart.plot)
  
  library(RColorBrewer)
  
  library(rattle)
  
  fancyRpartPlot(output.tree)
  
  Formula  = reformulate(".",response=targ123)
  dt <- train(Formula, data=dfTrain, method="rpart")
  
  if (printDTOnly) {
    library(rpart.plot)
    rpart.plot(dt$finalModel)
  }  
  
  predictions <- predict(dt, dfEval)
  dfPred <- as.data.frame(predictions)
  
  if(!printFancyTreeOnly) {
  if (categorical) {
    x <- factor(dfEval[, targ123])
    y <- confusionMatrix(predictions, x) 
    print(y)
  } else {
    
    #load Metrics package
    library(Metrics)
    rmseval <- rmse(dfEval[,targ123], dfPred$predictions)
    print(paste('Decision tree - RMSE on evaluation set: ', rmseval))
  }
  }
  
  x <- as.data.frame(cbind(dfEval[,targ123], dfPred))
  
  x1 <- x %>%
    rename("observeds" = 1) %>%
    mutate(observeds = as.double(observeds)) %>%
    mutate(predictions = as.double(predictions)) %>%
    mutate(residuals = observeds - predictions)
  
  
  newList <- list("dt" = dt, "errors" = x1)
  return(newList)

return(dt)

}

EHModel_RandomForest <- function(df4, target, seed=042760, categorical=TRUE, printRF = TRUE, printVarimp=TRUE, printPlot=TRUE, printConfusionMatrix=TRUE)
{
  
  #"Need to be the same factors" - Make sure to designate categorical=false if the targ123 is continuous
  
  targ123 <- target

  if (categorical) {
    df4[, targ123] <- as.factor(df4[, targ123])
  } 
  
  set.seed(seed)
  
  i <- createDataPartition(df4[,targ123], p=0.8, list=FALSE)
  
  dfEval <- df4[-i,]
  dfTrain <- df4[i,]
  
  count(dfTrain[targ123])
  
  tc <- trainControl(method="cv", number=10)
  metric <- "Accuracy"
  
  
  Formula  = reformulate(".",response=targ123)
  rf <- train(Formula, data=dfTrain, method="rf", trControl = tc)
  
  if (printRF){
    print(rf)
  }

  if (printPlot){
    print(plot(rf))
  }
  if (printVarimp){
    print(varImp(rf))
  }
  
  
  predictions <- predict(rf, dfEval)
  dfPred <- as.data.frame(predictions)
  
  if (categorical) {
    x <- factor(dfEval[, targ123])
    y <- confusionMatrix(predictions, x) 
    if (printConfusionMatrix){
    print(y)
    }
  } else {
    
    library(Metrics)
    rmseval <- rmse(dfEval[,targ123], dfPred$predictions)
    print(paste('Random Forest - RMSE on evaluation set: ', rmseval))
  }
  
  print(paste("Parameters:   mtry = ", rf$finalModel$mtry, ", ntree = ", rf$finalModel$ntree, ", nrnodes = ", rf$finalModel$forest$nrnodes))

  x <- as.data.frame(cbind(dfEval[,targ123], dfPred))
  
  x1 <- x %>%
  rename("observeds" = 1) %>%
  mutate(observeds = as.double(observeds)) %>%
  mutate(predictions = as.double(predictions)) %>%
  mutate(residuals = observeds - predictions)

  
  newList <- list("rf" = rf, "errors" = x1)
  return(newList)
  
}

EHModel_SVM <- function(df4, target, method = "linear", seed=042760, printSVM = TRUE, printPlot=FALSE, printConfusionMatrix =TRUE)
{
  
  #Scaling is done as part of pre-processing in train, so need not be done by hand.
  #For linear, c is tuned by the grid: expand.grid(C = seq(0.01, 2, length = 20).  For radial and poly, sigma and c are optimized automatically.
  
  targ123 <- target
  
    df4[, targ123] <- as.factor(df4[, targ123])

  set.seed(seed)
  
  i <- createDataPartition(df4[,targ123], p=0.8, list=FALSE)
  
  dfEval <- df4[-i,]
  dfTrain <- df4[i,]
  
  count(dfTrain[targ123])
  
  tc <- trainControl(method="repeatedcv", number=10, repeats=3)
  metric <- "Accuracy"
  
  library("stringi")     
  method1 <- stri_trans_totitle(method)
  method2 <- paste0("svm", method1)
  
  Formula  = reformulate(".",response=targ123)
  
  if (method1 == "Linear") {
    svm <- train(Formula, data=dfTrain, method=method2, trControl = tc, preProcess = c("center","scale"), tuneGrid = expand.grid(C = seq(0.01, 2, length = 20)))
  } else if (method1=="Radial"|method1=="Poly") {
  svm <- train(Formula, data=dfTrain, method=method2, trControl = tc, preProcess = c("center","scale"))
  } else {
    print("Unkown kernel. The choices are linear, radial or poly.")
    retun()
  }
    
  
  if (printSVM){
    print(svm)
  }
  
  if (printPlot){
    print(plot(svm))
  }
 
  
  predictions <- predict(svm, dfEval)
  dfPred <- as.data.frame(predictions)
  
    x <- factor(dfEval[, targ123])
    y <- confusionMatrix(predictions, x) 
    
    if (printConfusionMatrix){
      print(y)
    }
  
  #print(paste("Parameters:   mtry = ", rf$finalModel$mtry, ", ntree = ", rf$finalModel$ntree, ", nrnodes = ", rf$finalModel$forest$nrnodes))
  
    x <- as.data.frame(cbind(dfEval[,targ123], dfPred))
  
  x1 <- x %>%
    rename("observeds" = 1) %>%
    mutate(observeds = as.double(observeds)) %>%
    mutate(predictions = as.double(predictions)) %>%
    mutate(residuals = observeds - predictions)
  
  
  newList <- list("svm" = svm, "errors" = x1)
  return(newList)
  
}
EHCalculate_AUC_ForBinaryClasses <- function(dfPredictions, printPlot=TRUE, printConfusionMatrix=FALSE)
{
  
  #Observed come first, then Predictions!
  
  library(caTools)
  library(ROCR)
  
  dfPred <- dfPredictions %>%
    rename("obs1"=1, "pred1"=2) %>%
    dplyr::select(obs1, pred1)
  

  dfPred1 <- dfPred
  dfPred1$obs1a <- as.factor(dfPred1$obs1)
  dfPred1$pred1a <- as.factor(dfPred1$pred1)
  q <-confusionMatrix(data = dfPred1$pred1a, reference = dfPred1$obs1a)
  
  if (printConfusionMatrix){
  print(q)
  }
  
  roc1 <- roc(dfPred$obs1,
              dfPred$pred1, plot=printPlot)
  xauc <- roc1$auc
  
  newList <- list("AUC" = xauc, "ConfusionMatrix" = q)
  return(newList)
  
  
}