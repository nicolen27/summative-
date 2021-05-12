library(knitr)
library(ggplot2)


Ladybird <- read.csv("/cloud/project/Ladybird.csv")
head(Ladybird)

ggplot(Ladybird, aes(Avg_area,Mass.Ave, color=Sex))+ geom_boxplot()

# princple component analysis
ladybirdsubset <-Ladybird[ ,c(3,5:16)]
ladybirdsubsetnona <- na.omit(Ladybird[ ,c(3,5:16)])
ladybird.pca <- prcomp(ladybirdsubsetnona, center=TRUE, scale=TRUE)

summary(ladybird.pca)