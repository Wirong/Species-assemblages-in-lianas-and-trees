#-------------------------------------------------------
# Libraries, Path setting, and functions applied
#-------------------------------------------------------
#devtools::install_github("cran/mvpart")

rm(list = ls())

library(mvpart)
library(labdsv)
library(R.utils)
library(sf)
library(tidyverse)
library(raster)
library(spatstat)
library(corrplot)

# Function to select best cp considering minimum xerror
## <https://stackoverflow.com/questions/37721047/selecting-cp-value-for-decision-tree-pruning-using-rpart>
cp.select <- function(big.tree) {
  min.x <- which.min(big.tree$cptable[, 4]) #column 4 is xerror
  for(i in 1:nrow(big.tree$cptable)) {
    if(big.tree$cptable[i, 4] < big.tree$cptable[min.x, 4] + big.tree$cptable[min.x, 5]) return(big.tree$cptable[i, 1]) #column 5: xstd, column 1: cp 
  }
}

