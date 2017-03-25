
for (i in 2:length(nyc_z)){
  nyc_z[, i] <- ifelse(nyc_z[,i] == 0, 1, nyc_z[,i])
}

for (i in 2:length(nyc_z)){
  b <- nrow(subset(nyc_z, nyc_z[, i] == 0))
  print(b)
}

save(nyc_z, file = "nyc_z1.RData")
