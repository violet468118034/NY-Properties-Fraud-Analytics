
library(ggplot2)
library(dplyr)
library(ggthemes)

##PCA new data
Z_CAT_NORM = Z_CAT_NORM %>%
  select(-RECORD)
pca1 = prcomp(Z_CAT_NORM, center = FALSE, scale. = FALSE)
plot(pca1, type = "l")
summary(pca1)
print(pca1)

## top 12
pca_matrix = predict(pca1, newdata = Z_CAT_NORM)
pca_matrix = as.data.frame(pca_matrix)[, c(1:12)]


## Euclidean Distance
pca_z = pca_matrix
pca_z$Total_score = sqrt(rowSums(pca_z[, c(1:12)]^2))
pca_z$ID = 1:nrow(pca_z)


## Select fraud
fraud_HA = pca_z %>% 
  arrange(-Total_score)
fraud_HA = head(fraud_HA, 10486)


## Fraud distribution
ggplot(pca_z, aes(Total_score))+
  geom_histogram(bins = 50, fill = "lightblue", color = "black")+
  xlim(0,20)+
  theme_gdocs()+
  labs(x = "Fraud Score",
       y = "Total",
       title = "Fraud Score Distribution",
       subtitle = "PCA")+
  theme(plot.title = element_text(hjust = 0.5,vjust = -0.1, face = "bold"))+
  theme(plot.subtitle = element_text(hjust = 0.5,vjust = -0.1, face = "italic"))




#### Autoencoder
pca_z1 = as.data.frame(scale(Z_CAT_NORM))
library(h2o)
h2o.init()

## Autoencode, discover hidden layer
pca_auto = as.h2o(pca_z1)
pca_auto_dl = h2o.deeplearning(names(pca_auto), training_frame = pca_auto, 
                               autoencoder = TRUE)

## Reconstruct the original data set using the reduced set of features 
## and calculate a means squared error between both
pca_auto_anom = h2o.anomaly(pca_auto_dl, pca_auto, per_feature = FALSE)
error = as.data.frame(pca_auto_anom)

## Clean errors
error$ID = 1:nrow(error)

## Select fraud
fraud = error %>% 
  arrange(-Reconstruction.MSE)
fraud = head(fraud, 10486)


##Fraud Distribution
ggplot(error, aes(Reconstruction.MSE))+
  geom_histogram(bins = 50, fill = "lightblue", color = "black")+
  xlim(0, 0.00003)+
  theme_gdocs()+
  labs(x = "Fraud Score",
       y = "Total",
       title = "Fraud Score Distribution",
       subtitle = "Autoencoder")+
  theme(plot.title = element_text(hjust = 0.5,vjust = -0.1, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,vjust = -0.1, face = "italic"))

###OVERLAP
overlap = as.data.frame(intersect(fraud_HA$ID, fraud$ID))
##overlaprate
count(overlap)/10486
