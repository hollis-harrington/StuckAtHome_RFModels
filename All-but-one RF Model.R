library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(tidyverse)
library(tidymodels)
library(data.table)
library(randomForest)
library(VSURF)
library(ie2misc)
library(plotly)
library(caret)
library(splitTools)

#################################################
### This First section is just importing data ###
#################################################

load('Processed_Data/Watershed Dataframes.Rdata')

####################################################
### The next section deals with making the model ###
####################################################

# dfTraining dictates the watershed you want to make a RF model in
# Change it when you want to make a RF model for a different watershed
dfTraining <- Training_BEF

# RF models will be made for the following solutes
YSolutes <- list("Cl", "NO3", "SO4", "Na", "K", 
                 "Mg", "Ca",  "DON", "DOC")

# knum is how many times a dataframe will be split
# kreps dictates how many times cross validation will occur
# knum of 5 and kreps of 3 means 15 RF models will be generated per solute
# (3 reps of the dataframe being split into 5 parts)
knum <- 5
kreps <- 3
seq_along(1:(knum*kreps))

# Sets seed and creates empty dataframes to save results in.
set.seed=500
VNSERepository <- setNames(data.frame(matrix(ncol = 9, nrow = (knum*kreps))), YSolutes)
R2Repository <- setNames(data.frame(matrix(ncol = 9, nrow = (knum*kreps))), YSolutes)


for (i in seq_along(1:length(YSolutes))) {
  
  # sets up the stratified k-fold cross validation
  eval(parse(text = paste("folds <- create_folds(dfTraining$", YSolutes[[i]], ", k = knum, seed = 2734, m_rep = kreps, type = 'stratified')",sep = "")))
  print(YSolutes[[i]])
  
  # SelectedX being a value of 3 is just a placeholder value
  rowNum <- 1
  SelectedX <- 3
  for (j in seq_along(folds)) {
    print(j)
    
    # Splits data into training data (insample) and testing data (out)
    #insample <- dfTraining[folds[[j]], ]
    
    # For the all-but-one models, insample is always the singular watershed you
    # want to excluded from the training dataset. This should be whatever
    # watershed was selected for dfTraining
    
    dfTrainingTemp <- dfMaster[dfMaster$Watershed != "BEF"]
    insample <- dfTrainingTemp
    out <- dfTraining
    
    TrainX_Dumb <- insample[ , c("FDOM_corrected_QSU", "NO3_corrected", 
                                 "SpConductivity", "TempC", "DOYSin", "DOYCos", 
                                 'pH', 
                                 'ODOMGL')]
    TrainY <- insample[ , c("Cl", "NO3", "SO4", "Na", "K", 
                            "Mg", "Ca",  "DON", "DOC")]
    
    print(i)
    set.seed=500
    
    # The first loop will select independent variables for RF models of a given
    # solute. Following loops will use the same variables.
    if(!is.list(SelectedX)) {
      vsurfVar <- VSURF(TrainX_Dumb, TrainY[[i]], mtry = 100)
    }
    
    # varselect.pred selects the variables found to be important by VSURF
    SelectedX <- TrainX_Dumb %>% select(vsurfVar$varselect.pred)
    
    # In the event that no variables were selected during the prediction step,
    # variable selection is pulled from the interpretation step.
    # Note, this likely doesn't trigger anymore.
    if (nrow(SelectedX) == 0) {
      SelectedX <- TrainX_Dumb %>% select(vsurfVar$varselect.interp)
    }
    
    # Store RF model as mod
    mod <- randomForest(SelectedX, TrainY[[i]])
    
    # The remainder of the loop is used to calculate R2 and NSE, and store them
    # in repository dataframes
    PredY <- predict(mod, out)
    LRData <- as.data.frame(PredY)
    eval(parse(text = paste("LRData$ActY <- out$", YSolutes[[i]], sep = "")))
    
    linModel <- lm(PredY~ActY, data=LRData)
    R2 <- cor(LRData$ActY, LRData$PredY)^2
    
    eval(parse(text = paste("VNSERepository[", rowNum, ",", i, "] <- vnse(predict(mod, out), out$", YSolutes[[i]],")", sep = "")))
    eval(parse(text = paste("R2Repository[", rowNum, ",", i, "] <- R2", sep = "")))
    
    rowNum <- rowNum + 1
  }
  print(VNSERepository)
}

# This line was for when there was a bug with how lm would handle
# linear regression models of datasets with only one X value. This bug has been
# patched and the line below shouldn't do anything anymore.
R2Repository[is.na(R2Repository)] <- -9999
summary(VNSERepository)

# Wrangling the NSE and R2 dataframes for storage
SumStats <- sapply(VNSERepository, function(x) c(summary(x), type = class(x)))
SumStatsR2 <- sapply(R2Repository, function(x) c(summary(x), type = class(x)))

# Change the name of the .csv below at the end of each run. I followed the
# format of test_model_watershed.
write.csv(SumStats, file = 'Models/NSE_AB1_BEF.csv')
write.csv(SumStatsR2, file = 'Models/R2_AB1_BEF.csv')