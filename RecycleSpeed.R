#--------------------------------------------------------------------------------------------------------------------
# HARLEQUINS RECYCLE SPEED ANALYSIS
#--------------------------------------------------------------------------------------------------------------------
# JAVA HEAP SIZE
options(java.parameters = "-Xmx5g")
#--------------------------------------------------------------------------------------------------------------------
# LIBRARIES
library(plotly)
library(bartMachine)
#--------------------------------------------------------------------------------------------------------------------
# DATA
# Use the QuinsRecycleSpeed.csv file: https://github.com/jmwallusch/QuinsRecycleSpeed/blob/main/QuinsRecycleSpeed.csv
#--------------------------------------------------------------------------------------------------------------------
# DUMMIES AND CATEGORICAL FEATURES
# Counterrucking 
CntR <- ifelse(RSA$Contested == "none", 0, 1)
# Pitch zone
zone <- ifelse(RSA$Zone == "Own22", 1, 
               ifelse(RSA$Zone == "Own22-10", 2, 
                      ifelse(RSA$Zone == "Own10-50", 3,
                             ifelse(RSA$Zone == "Opp50-10", 4, 
                                    ifelse(RSA$Zone == "Opp10-22", 5, 6)))))
# Clock
ClTm <- as.numeric(paste(RSA$Min,".",RSA$Sec, sep = ""))
#--------------------------------------------------------------------------------------------------------------------
