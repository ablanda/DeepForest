#' random forest cv
#'
#' fonisce le previsioni della cv, da utilizzare dentro sapply(1:k,...)
#'
#' @param i i-esimo fold
#' @param formula formula del modello
#' @param folds prodotto da kfold function
#' @param y  vettore. variabile risposta
#' @param mtry 
#' @param x matrice variabili esplicative
#'
#' @export
#' @import ranger
# training_frame<-dati[1:100,1:40]
# validation_frame<-dativ[1:100,1:40]
# y=1
# x = NULL
# num.trees = 200
# n_forest = 8
# random_forest = 4
# pmtry=NULL
# eta = 1
# num.threads =NULL
# evaluation_metric='mse'
# work.dir = getwd()
# out.of.memory = T
# early.stop = 4
# write.forest = TRUE
# save.memory = FALSE
# id = NULL
# continue=0
# k=3
# system.time(prova<-power_ranger(y=1,training_frame=dati,validation_frame=dativ,early.stop=4))
# str(prova)
ranger_cv<-function(i,formula,data,num.trees, folds,mtry,splitrule,probability,write.forest, num.threads, save.memory)
{
    m<- ranger(
      formula=formula,
      data = data[-folds[[i]],],
      num.trees = num.trees,
      mtry = mtry,
      splitrule = splitrule,
      probability = probability,
      write.forest = write.forest,
      num.threads = num.threads,
      save.memory = save.memory,seed =  sample(1:1e4,1)
    )
if(probability)
  pred <- predict(m, data = data[folds[[i]],])$predictions[,-1]
else   pred <- predict(m, data = data[folds[[i]],])[[1]]

return(pred)
}
