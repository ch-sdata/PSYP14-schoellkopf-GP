## ASSIGNMENT GEOFFREY 

require(reshape2)
Charlotte_new=reshape(`PAQ_Charlotte`, direction = "wide",
                  idvar = "id", timevar="var")


DGP1 <- Charlotte_new

summary(DGP1) 

#NA's: help, breathe, support and sex has 3 levels? 
# also wrong as all are integers 

DGP1 = DGP1 %>%
  mutate(value.sex = factor(value.sex))

DGP1 = DGP1 %>%
  mutate(
    value.Q1_cry = as.numeric(value.Q1_cry),
    value.Q2_help = as.numeric(value.Q2_help), 
    value.Q3_breathe = as.numeric(value.Q3_breathe),
    value.Q4_freeze = as.numeric(value.Q4_freeze), 
    value.Q5_alien = as.numeric(value.Q5_alien), 
    value.Q6_inferior = as.numeric(value.Q6_inferior), 
    value.Q7_weep = as.numeric(value.Q7_weep), 
    value.Q8_Support = as.numeric(value.Q8_Support), 
    value.Q9_Nerd = as.numeric(value.Q9_Nerd), 
    value.age = as.numeric(value.age)
    )

DGP1 = DGP1 %>%
  drop_na(value.Q2_help, value.Q3_breathe, value.Q8_Support)

summary(DGP1) 

if(!require(dplyr)){install.packages('dplyr')}
library(dplyr)

DGP_new <- DGP1 %>% 
  rename(
    sex = value.sex,
    age = value.age,
    cry =value.Q1_cry,
    help = value.Q2_help, 
    breathe = value.Q3_breathe, 
    freeze = value.Q4_freeze, 
    alien = value.Q5_alien, 
    inferior = value.Q6_inferior, 
    weep = value.Q7_weep, 
    support = value.Q8_Support,
    nerd = value.Q9_Nerd
  )

# no we excluded the NA's 

DGP2 = DGP_new %>% 
  subset(, 4:12)

cor(DGP2)
cor.plot(DGP2)
plot(DGP2)


pca <- princomp(DGP2, cor=T)
pca
summary(pca, loadings = TRUE)

#in the "proportion of variance" we see how much variance get explained 


#scree plot 

plot(pca$sdev^2,          
     xlab= "Component number",  
     ylab="Component variance",
     type = "l", 
     main="Scree diagram",
     xaxt = 'n')                 
axis(side=1,at=c(1,2,3,4,5,6,7,8,9),labels=c("1","2","3","4","5","6","7","8","9"))


#another scree plot
plot(pca$sdev^2, xlab = "Component number",
     ylab = "Component variance", type = "l", main = "Scree diagram")

#log(eigenvalue) diagram 

plot(log(pca$sdev^2), xlab = "Component number",
     ylab = "log(Component variance)", type="l",
     main = "Log(eigenvalue) diagram")
axis(side=1,at=c(1,2,3,4,5,6,7,8,9),labels=c("1","2","3","4","5","6","7","8","9"))
abline(h=0, v=3, col = "gray60", lty = 2)
abline(h=0, v=2, col = "gray60", lty = 2)


if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")
library(factoextra)
library(FactoMineR)


biplot(pca, scale = 0) 


#calculate explained variance

variance = pca$sdev^2 / sum(pca$sdev^2)
variance

#### some stuff to try out #### 

str(pca)
pca$scores

dataframe_pca <- cbind(DGP1, pca$scores[,1:2])
dataframe_pca2 <- cbind(DGP1, pca$scores[,1:3])
library(ggplot2)

dataframe_pca = dataframe_pca %>%
  replace(dataframe_pca, value.sex == "2", 1)

dataframe_pca$value.sex <- replace(dataframe_pca$value.sex, dataframe_pca$value.sex == 2, 1)

ggplot(dataframe_pca, aes(Comp.1, Comp.2, col = value.sex, fill = value.sex )) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")



dataframe_pca2 = dataframe_pca2 %>%
  replace(dataframe_pca2, value.sex == "2", 1)

dataframe_pca2$value.sex <- replace(dataframe_pca2$value.sex, dataframe_pca2$value.sex == 2, 1)

ggplot(dataframe_pca2, aes(Comp.1, Comp.2, col = value.sex, fill = value.sex )) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")



pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")


library(ggplot2)

pca.data <- data.frame(Sample=rownames(pca$scores),
                       X=pca$scores[,1],
                       Y=pca$scores[,2])
pca.data

ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("My PCA Graph")

loading_scores <- pca$loadings[,1]
gene_scores <- abs(loading_scores) ## get the magnitudes
gene_score_ranked <- sort(gene_scores, decreasing=TRUE)
top_10_genes <- names(gene_score_ranked[1:2])

top_10_genes ## show the names of the top 10 genes

pca$loadings[top_10_genes,1] ## show the scores (and +/- sign)

########### 3D plot 

library(rgl)
text3d(pca$scores[,1:3],texts=rownames(iris))
text3d(pca$loadings[,1:3], texts=rownames(pca$loadings), col="red")
coords <- NULL
for (i in 1:nrow(pca$loadings)) {
  coords <- rbind(coords, rbind(c(0,0,0),pca$loadings[i,1:3]))
}
lines3d(coords, col="red", lwd=4)
