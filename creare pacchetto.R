create('deepForest')

library(devtools)
library(roxygen2)
#setwd("./Tesi/deepForest")
document()
setwd("..")
install('deepForest')
##### aspettare
library(deepForest)
?power_ranger
deepForest::predict.cv
#predict.cv lo mette tra gli oggetti nascosti essendoci gia la funzione predict generale, fa lo stesso con predict.randomForest
