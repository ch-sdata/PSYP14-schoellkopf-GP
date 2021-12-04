# ASSIGNMENT GP Part 2


summary(Nations)


#no NAs 

library(psych) # for describe	
library(car) # for residualPlots, vif, pairs.panels, ncvTest	
library(lmtest) # bptest	
library(sandwich) # for coeftest vcovHC estimator	
library(boot) # for bootstrapping	
library(lmboot) # for wild bootsrapping	
library(dplyr)
library(dbplyr)
library(ggplot2)
library(lm.beta) # for lm.beta	
library(gridExtra) # for grid.arrange	
library(tidyverse) # for tidy format

library(psych)    # for describe function
require(smacof)   # needed for the sim2diss function
library(MASS)


summary(Nations)

describe(Nations)


# Dissimilarities
dissim_na= abs(sim2diss(Nations, method = 7, to.dist = 2))
dissim_na
summary(dissim_na)


# Non-metric multidimensional scaling

na_mds=isoMDS(dissim_na)
na_mds



#Getting coordinates
# 1 refers to 1st coordinate
# 2 refers to 2nd coordinate
x <- na_mds$points[,1] # 1 refers to 1st coordinate
y <- na_mds$points[,2] # 2 refers to 2nd coordinate

#Shepard code 

na_shep <- Shepard(dissim_na, na_mds$points)
plot(na_shep, pch = 20, xlab = "Dissimilarity", ylab = "Distance", xlim = range(na_shep$x, na.rm = TRUE, ylim = range(na_shep$x, na.rm = TRUE)))
lines(na_shep$x, na_shep$yf, type = "S")


#plotting the data on the axis 

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",
     xlim = range(na_mds$points[,1])*1.2, type = "n") 
text(x, y, labels = colnames(Nations), cex = 0.6)
abline(h=0, v=0, col = "gray60", lty = 2)

# some fancy stuff 


library(magrittr)
library(dplyr)
install.packages("ggpubr")
library(ggpubr)

mds <- Nations %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")
# Plot MDS
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(Nations),
          size = 1,
          repel = TRUE)


# K-means clustering
clust <- kmeans(mds, 3)$cluster %>%
  as.factor()
mds <- mds %>%
  mutate(groups = clust)
# Plot and color by groups "kmeans clustered metric" # but I dont know what that means! 
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = colnames(Nations),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)



### non linear 


# Cmpute MDS
library(MASS)
mds <- Nations %>%
  dist() %>%          
  isoMDS() %>%
  .$points %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")
# Plot MDS "Dissim 1"
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = colnames(Nations),
          size = 1,
          repel = TRUE)

# Cmpute MDS
library(MASS)
mds <- Nations %>%
  dist() %>%          
  sammon() %>%
  .$points %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")

# K-means clustering
clust <- kmeans(mds, 3)$cluster %>%
  as.factor()
mds <- mds %>%
  mutate(groups = clust) 
# Plot MDS # "kmeans clustered non_metric" # but I dont know if this is correct! 
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = colnames(Nations),
          color = "groups", 
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)


# correlations 

res.cor <- cor(Nations, method = "spearman")
mds.cor <- (1 - res.cor) %>%
  cmdscale() %>%
  as_tibble()
colnames(mds.cor) <- c("Dim.1", "Dim.2")
ggscatter(mds.cor, x = "Dim.1", y = "Dim.2", 
          size = 1,
          label = colnames(res.cor),
          repel = TRUE)
