# derived from
# https://www.sciencedirect.com/topics/neuroscience/discriminant-function-analysis
# https://www.statmethods.net/advstats/discriminant.html
# https://my.ilstu.edu/~wjschne/444/DiscriminantFunctionAnalysis.html#(9)
# 

setwd("~/path/to/your/working/folder/")

# Linear Discriminant Analysis with Jacknifed Prediction
library(klaR)
library(magrittr)
library(pander)
library(MASS)
library(dplyr)
library(ggplot2)


#get data
#the data is in a csv table where each row are the dissimilarity distances viz. relevant anchor.
#there are additional columns for 'origin', 'provenance', 'sex' for the tested image.

skulls <- read.csv("table.csv", header = TRUE, sep=",")


#do lda
# 'origin' is the purported broad geographic origin of the sample. Elements after the tilde are the column names eg the anchor image distances.
fit <- lda(origin ~ asianfemale + african.american.female + eurofemale + asianmale + euromale + african.american.male, data=skulls)
fit # show results 

plot(fit)

# Panels of histograms and overlayed density plots
# for 1st discriminant function
plot(fit, dimen=1, type="both") # fit from lda 

# predictions
p <- predict(fit)
freqtable <- table(p$class, skulls$origin) 
rownames(freqtable) <- paste0("Predicted ", skulls$origin %>% levels)
freqtable %>% addmargins %>% pander("Observed vs. Predicted Frequencies")

prop.table(freqtable) %>% addmargins %>% pander("Proportions")


predictiontable <- data.frame(p$posterior, 
           ObservedGroup = skulls$origin, 
           PredictedGroup = p$class, p$x) 

predictiontable ["sample"] <- skulls$sample
predictiontable ["short-label"] <- skulls$label
View(predictiontable)
write.csv(predictiontable, file = "predictiontable-origin.csv")

#visualize
skulls$LDA1 <- p$x[,1]
skulls$LDA2 <- p$x[,2]

ggplot(skulls, aes(LDA1, fill = origin)) + geom_density(alpha = 0.5, color = NA) + theme(legend.position = "top")

plot(fit, abbrev = 1)

gMeans <- skulls %>% group_by(origin) %>% select(LDA1,LDA2) %>%  summarise_each(funs(mean)) 

gMeans %>% pander

ggplot(skulls, aes(LDA1, LDA2, color = origin)) + 
  geom_point(alpha = 0.5) + 
  geom_text(data = gMeans, 
            aes(label = origin),
            color = "black", 
            hjust = -0.1,
           vjust = -0.75
             ) + 
  geom_point(data = gMeans, 
             aes(fill = origin), 
             size = 4, 
             color = "black",
             pch = 21) + 
  geom_text(aes(label=label),hjust=0, vjust=0) +
  theme(legend.position = "none") +
  coord_equal()

library(klaR)
drawparti(grouping = skulls$origin, x = skulls$LDA1, y = skulls$LDA2, xlab = "LDA1", ylab = "LDA2")


# same again, but now for sex.

#do lda


sex.lda <- lda(sex ~ asianfemale + african.american.female + eurofemale + asianmale + euromale + african.american.male, data=skulls)
sex.lda # show results 

plot(sex.lda)

# Panels of histograms and overlayed density plots
# for 1st discriminant function
plot(sex.lda, dimen=1, type="both") # fit from lda 

# predictions
p2 <- predict(sex.lda)
freqtable <- table(p2$class, skulls$sex) 
rownames(freqtable) <- paste0("Predicted ", skulls$sex %>% levels)
freqtable %>% addmargins %>% pander("Observed vs. Predicted Frequencies")

prop.table(freqtable) %>% addmargins %>% pander("Proportions")


sexpredictiontable <- data.frame(p2$posterior, 
                              ObservedGroup = skulls$sex, 
                              PredictedGroup = p2$class, p2$x) 

sexpredictiontable ["sample"] <- skulls$sample
sexpredictiontable ["short-lable"] <- skulls$label
View(sexpredictiontable)

write.csv(sexpredictiontable, file = "predictiontable-sex.csv")
#

skulls$sexLDA1 <- p2$x[,1]
skulls$sexLDA2 <- p2$x[,2]

ggplot(skulls, aes(sexLDA1, fill = sex)) + geom_density(alpha = 0.5, color = NA) + theme(legend.position = "top")

plot(fit, abbrev = 1)

gMeans <- skulls %>% group_by(sex) %>% select(sexLDA1,sexLDA2) %>%  summarise_each(funs(mean)) 

gMeans %>% pander

ggplot(skulls, aes(sexLDA1, sexLDA2, color = sex)) + 
  geom_point(alpha = 0.5) + 
  geom_text(data = gMeans, 
            aes(label = sex),
            color = "black", 
            vjust = 1.75) + 
  geom_point(data = gMeans, 
             aes(fill = sex), 
             size = 4, 
             color = "black",
             pch = 21) + 
  geom_text(aes(label=label),hjust=0, vjust=0) +
  theme(legend.position = "none") +
  coord_equal()

library(klaR)
drawparti(grouping = skulls$origin, x = skulls$sexLDA1, y = skulls$sexLDA2, xlab = "sexLDA1", ylab = "sexLDA2")
