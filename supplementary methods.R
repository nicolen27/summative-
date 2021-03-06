
# load packages

library(knitr)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(rstatix)

# read in csv file
Ladybird <- read.csv("/cloud/project/Ladybird.csv")

# print the first lines of dataframe
print(head(Ladybird))




# princple component analysis
ladybirdsubset <-Ladybird[ ,c(3,5:16)]
ladybirdsubsetnona <- na.omit(Ladybird[ ,c(3,5:16)])
ladybird.pca <- prcomp(ladybirdsubsetnona, center=TRUE, scale=TRUE)

summary(ladybird.pca)


# seperate out ladybird weights w/ melanic and typical
weight_melanic <-  Ladybird$Mass.Ave[Ladybird$Colour.morph=="M"]
weight_typical <-  Ladybird$Mass.Ave[Ladybird$Colour.morph=="T"]

# create an unpaired ttest for loading into boxplot
test <- t.test(weight_melanic, weight_typical, var.equal=TRUE) %>% adjust_pvalue(method="bonferroni") %>% add_significance()


  scale_x_discrete(labels=c('Melanic','Typical'))+ stat_compare_means(method="t.test") 

# males distribution vs feamles for p time mean 

ggplot(Ladybird, aes(Sex,P.time.mean)) + geom_boxplot()+labs(x= 'Sex', y= 'Time took to attack aphid')
test <- t.test('sex' ,'P.time.mean', var.equal=TRUE) %>% adjust_pvalue(method="bonferroni") %>% add_significance()


ggplot(Ladybird, aes(Colour.morph,Mass.Ave))+ geom_boxplot()+labs(x= 'Colour Morph', y= 'Average Mass') +
  scale_x_discrete(labels=c('Melanic','Typical')) + stat_compare_means(method="t.test") 


ggplot(Ladybird, aes(Sex,P.time.mean)) + geom_boxplot()+labs(x= 'Sex', y= 'Time took to attack aphid')+ stat_compare_means(method="t.test") 

