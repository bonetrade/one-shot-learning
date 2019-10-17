## derived from
## https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/

setwd("~/path/to/your/working/folder/")

#get data
#the data is in a csv table where each row are the dissimilarity distances viz. relevant anchor.
#there are additional columns for 'origin', 'provenance', 'sex' for the tested image.

skulls <- read.csv("table.csv", header = TRUE, sep=",")

# log transform 
log.skulls <- log(skulls[, 1:6])

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
skulls.pca <- prcomp(log.skulls,
                 center = TRUE,
                 scale. = TRUE) 
print(skulls.pca)

# plot method
plot(skulls.pca, type = "l")
# summary method
summary(skulls.pca)


library(devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)

##pca by sex
g <- ggbiplot(skulls.pca, obs.scale = 1, var.scale = 1,labels.size=5, 
              groups = skulls$sex, ellipse = TRUE, 
              circle = FALSE,
              labels = skulls$label)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top') +
  theme_linedraw(base_size = 11, base_family = "")
           
print(g)

##pca by origin
g2 <- ggbiplot(skulls.pca, obs.scale = 1, var.scale = 1,labels.size=5, 
              groups = skulls$origin, ellipse = TRUE, 
              circle = FALSE,
              labels = skulls$label)
g2 <- g2 + scale_color_discrete(name = '')
g2 <- g2 + theme(legend.direction = 'horizontal', 
               legend.position = 'top') +
  theme_linedraw(base_size = 11, base_family = "")
           
print(g2)

## pca by provenance
g3 <- ggbiplot(skulls.pca, obs.scale = 1, var.scale = 1,labels.size=5, 
              groups = skulls$provenance, ellipse = TRUE, 
              circle = FALSE,
              labels = skulls$label)
g3 <- g3 + scale_color_discrete(name = '')
g3 <- g3 + theme(legend.direction = 'horizontal', 
               legend.position = 'top') +
  theme_linedraw(base_size = 11, base_family = "")
           
print(g3)
