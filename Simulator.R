# Shawn Gilroy
# GPLv3
# Simulated consumption from existing data

set.seed(65535)

# Apt data prices, means, sd's (see Beez)
pricePoints <- c(0,0.25,0.5,1,1.5,2,2.5,3,4,5,6,7,8,9,10,15,20)
consumptionMean <- c(5.856884058,5.425724638,5.257246377,5.049818841,4.714673913,4.413043478,4.081521739,3.685688406,3.151268116,2.674818841,2.16576087,1.799637353,1.424818841,1.113327289,0.923913043,0.490942029,0.350543478)
consumptionSD <- c(4.705973278,4.301995389,4.194430187,3.938184029,3.70212462,3.457774816,3.262748553,3.018651843,2.808459372,2.470158851,2.252749078,2.349112958,1.78097839,1.733577022,1.699213558,1.189442317,0.900553209)

# pre-allocate data frame frame, columns per each price point
preallocatedFrame <- data.frame(matrix(vector(),
                                       10000,
                                       length(pricePoints),
                                       dimnames = list(c(),
                                                       c(pricePoints))),
                                stringsAsFactors = FALSE)

# generate column list, for iterating through price points
pricePointsName <- names(preallocatedFrame)

#iterate through price densities, populate each column with rnorm values (n=1000) based on existing variance
for (i in 1:length(pricePointsName)) {
  #message(i)
  preallocatedFrame[,pricePointsName[i]] <- rnorm(1000, mean = consumptionMean[i], sd = 0.5 * max(consumptionSD[i]))
}

# temporarily cast into a matrix
tempMat <- as.matrix(preallocatedFrame)

# convert any negatives to zero (cant have neg consumption)
tempMat[tempMat < 0] <- 0

# cast back to df
preallocatedFrame2 <-as.data.frame(tempMat)

# write to csv
write.csv(preallocatedFrame2, file = "SimulatedValuesBig.csv", row.names = TRUE)

