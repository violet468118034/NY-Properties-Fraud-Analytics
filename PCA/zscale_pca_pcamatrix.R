
library(dplyr)
# z-score 
zzzzz <- data.frame(ratioNormAll$RECORD)
#ratioNormAll
var=colnames(ratioNormAll[2:71])
var
for (i in 1:70){
  zzzzz[paste(var[i],"_z",sep="")] <- scale(ZratioNormAll[,var[i]])
}
colnames(zzzzz)[1] <-"RECORD"
save(zzzzz, file = "zhy_zscale.RData")

?plot
#####method 1
#
load("Zscale_CAT_NORM.RData")
Z_CAT_NORM = Z_CAT_NORM %>%
  select(-RECORD)
ir.pca <- prcomp(Z_CAT_NORM,
                 center = FALSE,
                 scale. = FALSE) 
print(ir.pca)
plot(ir.pca,type = "l")
summary(ir.pca)
pcamatrix = predict(ir.pca, newdata=Z_CAT_NORM)
pcamatrix<-as.data.frame(pcamatrix)
pcamatrix = pcamatrix %>%
  select(PC1:PC12)
save(pcamatrix,file = "pcamatrix.RData")


library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
             ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

#####method 2
library("FactoMineR")
res.pca <- PCA(zzzzz, graph = FALSE)
print(res.pca)
eigenvalues <- res.pca$eig
head(eigenvalues[, 1:2])


library("factoextra")
fviz_screeplot(res.pca, ncp=15)

#The correlation between a variable and a PC is called loading. 
#The variables can be plotted as points in the component space using 
#their loadings as coordinates.

# Coordinates of variables
head(res.pca$var$coord)
fviz_pca_var(res.pca)
#Correlation circle can help to visualize the most correlated 
#variables (i.e, variables that group together).


#The squared loadings for variables are called cos2 
# ( = cor * cor = coord * coord).
head(res.pca$var$cos2)
#The sum of the cos2 for variables on the principal components is equal to one.

#If a variable is perfectly represented by only two components, 
#the sum of the cos2 is equal to one. In this case the variables will be 
#positioned on the circle of correlations.

#For some of the variables, more than 2 components are required to 
#perfectly represent the data. In this case the variables are positioned 
#inside the circle of correlations.


#The cos2 values are used to estimate the quality of the representation
#The closer a variable is to the circle of correlations, the better its 
#representation on the factor map (and the more important it is to interpret 
#these components) Variables that are closed to the center of the plot are 
#less important for the first components.

fviz_pca_var(res.pca, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.5) + theme_minimal()



# Contributions of variables on PC1
fviz_pca_contrib(res.pca, choice = "var", axes = 1)
#If the contribution of the variables were uniform, 
#the expected value would be 1/length(variables) = 1/10 = 10%.

#The red dashed line on the graph above indicates the expected 
#average contribution. For a given component, a variable with a 
#contribution larger than this cutoff could be considered as important 
#in contributing to the component.




# Contributions of variables on PC2
fviz_pca_contrib(res.pca, choice = "var", axes = 2)




# Total contribution on PC1 and PC2
fviz_pca_contrib(res.pca, choice = "var", axes = 1:2)


# Control variable colors using their contributions
fviz_pca_var(res.pca, col.var="contrib")



# Change the gradient color
fviz_pca_var(res.pca, col.var="contrib") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=50) + theme_minimal()



res.desc <- dimdesc(res.pca, axes = c(1,2))
# Description of dimension 1
res.desc$Dim.1
