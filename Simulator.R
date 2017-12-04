# Shawn Gilroy
# GPLv3
# Simulated consumption from existing data

library(beezdemand)

set.seed(65535)

sdMap <- 0.5
nPoints <- 10000

# Apt data prices, means, sd's
pricePoints <- c(0,0.25,0.5,1,1.5,2,2.5,3,4,5,6,7,8,9,10,15,20)
consumptionMean <- c(5.856884058,5.425724638,5.257246377,5.049818841,4.714673913,4.413043478,4.081521739,3.685688406,3.151268116,2.674818841,2.16576087,1.799637353,1.424818841,1.113327289,0.923913043,0.490942029,0.350543478)
consumptionSD <- c(4.705973278,4.301995389,4.194430187,3.938184029,3.70212462,3.457774816,3.262748553,3.018651843,2.808459372,2.470158851,2.252749078,2.349112958,1.78097839,1.733577022,1.699213558,1.189442317,0.900553209)

# pre-allocate a frame
preallocatedFrame <- data.frame(matrix(vector(),
                                       nPoints,
                                       length(pricePoints),
                                       dimnames = list(c(),
                                                       c(pricePoints))),
                                stringsAsFactors = FALSE)

# Naming conventions (they are odd with numerics)
pricePointsName <- names(preallocatedFrame)

for (i in 1:length(pricePointsName)) {
  message(sprintf("Generating price point: %f", pricePoints[i]))

  # Stubbed: based on re-sampling
  #preallocatedFrame[,pricePointsName[i]] <- sample(consValues, size=nPoints, replace=TRUE)

  # Based on means/sds
  preallocatedFrame[,pricePointsName[i]] <- rnorm(nPoints, mean = consumptionMean[i], sd = sdMap * consumptionSD[i])
}

# Restore colnames, add row #'s and columns for passes
colnames(preallocatedFrame) <- pricePoints
preallocatedFrame$row <- seq(from = 1, to = nrow(preallocatedFrame), by = 1)
preallocatedFrame$pass <- NA

# Round negatives to flat zero
tempMat <- as.matrix(preallocatedFrame)
tempMat[tempMat < 0] <- 0
preallocatedFrame <- as.data.frame(tempMat)

# Loop through beez to get # passing
for (i in 1:nrow(preallocatedFrame)) {
  # Callback
  if (i %% 1000 == 0) {
    message(paste("Calculating #:", i, "of", nPoints, sep = " "))
  }

  test <- data.frame(id = rep(preallocatedFrame[i, "row"], length(pricePoints)),
                     x = pricePoints,
                     y = c(unname(unlist(preallocatedFrame[i, 1:17]))))

  preallocatedFrame[i, "pass"] <- CheckUnsystematic(test)$TotalPass
}

# Select series that hit all 3 passes
passingSeriesFrame = preallocatedFrame[preallocatedFrame$pass == 3,]

if (nrow(passingSeriesFrame) > 1000) {
  message("Successfully written at least 1000 series")
  passingSeriesFrame = passingSeriesFrame[1:1000,]
  write.csv(passingSeriesFrame, file = "SimulatedValuesPassing.csv", row.names = TRUE)

} else {
  message("failed to produce at least 1000 systematic series")

}
