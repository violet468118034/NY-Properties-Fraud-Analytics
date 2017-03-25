#### Autoencoder
Z_CAT_NORM = Z_CAT_NORM %>%
  select(-RECORD)
pca_z1 = as.data.frame(scale(Z_CAT_NORM))
library(h2o)
h2o.shutdown()
h2o.init()



?h2o

## Autoencode, discover hidden layer
pca_auto1 = as.h2o(Z_CAT_NORM)
pca_auto_dl1 = h2o.deeplearning(names(pca_auto1), training_frame = pca_auto1, 
                               autoencoder = TRUE)

## Reconstruct the original data set using the reduced set of features 
## and calculate a means squared error between both
pca_auto_anom1 = h2o.anomaly(pca_auto_dl1, pca_auto1, per_feature = TRUE)
error1 = as.data.frame(pca_auto_anom1)

## Clean errors
error1$Total_score = sqrt(rowSums(error1[, c(1:70)]^2))
error1$ID = 1:nrow(error1)

## Select fraud
fraud1 = error1 %>% 
  arrange(-Total_score)
fraud1 = head(fraud1, 10486)

## Fraud contribution
score = colSums(fraud1[, -c(71:72)])
faud1 = rbind(fraud1, score)




##Fraud Distribution
ggplot(error, aes(Reconstruction.MSE))+
  geom_histogram(bins = 80)+
  xlim(0, 0.002)+
  ylim(0, 3000)+
  theme_economist()+
  xlab("Fraud Score")+
  ylab("Total")+
  ggtitle("Fraud Score Distribution")+
  theme(plot.title = element_text(hjust = 0.5,vjust = -0.1, face = "bold"))

ggplot(error, aes(Reconstruction.MSE*100000))+
  geom_histogram(bins = 100, fill = "lightblue")+
  theme_gdocs()+
  xlab("Fraud Score")+
  ylab("Total")+
  ggtitle("Fraud Score Distribution")+
  theme(plot.title = element_text(hjust = 0.5,vjust = -0.1, face = "bold"))

###OVERLAP
overlap1 = as.data.frame(intersect(fraud_HA$ID, fraud1$ID))
##overlaprate
count(overlap1)/10486
