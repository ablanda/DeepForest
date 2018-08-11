#' Cascade Forest's implementation in R
#'
#' This function attempts to replicate Cascade Forest in original way of paper by package ranger.
#'
#' For implementation details of Cascade Forest: \url{https://arxiv.org/pdf/1702.08835.pdf}.
#'
#' @param x A vector containing the names or indices of the predictor variables to use in building the model. If x is missing,then all columns except y are used.
#' @param y The name of the response variable in the model.If the data does not contain a header, this is the column index number starting at 0, and increasing from left to right. (The response must be either an integer or a categorical variable).
#' @param training_frame Training data of class \code{data.frame} or \code{matrix}.
#' @param validation_frame Validation data.
#' @param work.dir Type: character. When \code{out.of.memory == TRUE}, the four models of each layers will be saved to disk in file \code{df1:early.stop.RData}. If you don't provide a working directory, the models will be saved inside that directory.
# @param out.of.memory Type: logical. If you want save the model.
#' @param early.stop Number of layers.
#' @param n_forest Total forest number for every layer.
#' @param random_forest Number of Random forest.
#' @param num.trees Number of trees.
#' @param pmtry Percentage of variables to possibly split at in each node. Default is the (rounded down) square root of the number variables divided by total numbers.
# @param eta proporzione delle colonne originarie selezionate dal secondo stadio in poi
#' @param num.threads Number of threads.
#' @param continue It's used for prediction if \code{n_forest==continue}. Else if \code{n_forest>continue} for add layers to Cascade Forest.
#' @param write.forest Save \code{ranger.forest} object, required for prediction. Set to \code{FALSE} to reduce memory usage if no prediction intended.
#' @param save.memory Use memory saving (but slower) splitting mode. Warning: This option slows down the tree growing, use only if you encounter memory problems.
#' @param id prefix of saved files
#' @param k number of k-folds, if is NULL (default) use OOB
#'
#' @author Blanda Alessandro
#' @export
#' @import ModelMetrics ranger
#' @examples
#' \dontrun{
#' rm(list=ls())
#' # Load libraries
#' library(devtools)
#' install_github( 'ablanda/deepForest ')
#' library(deepForest)
#' # Download MNIST data here: \url{https://pjreddie.com/projects/mnist-in-csv/}
#' dati<-read.csv('mnist_train.csv',header=F)
#' dativ<-read.csv('mnist_test.csv',header=F)
#'
#' dati[,1]<-as.factor(dati[,1])
#' dativ[,1]<-as.factor(dativ[,1])
#' m<-power_ranger(y=1,training_frame = dati[1:100,],validation_frame = dativ[1:100,],n_forest=8,random_forest = 4,early.stop=4,k=3)
#' pred<-matrix(0,nrow(dativ),nlevels(dati[,1])-1)
#' for(h in 1:(nlevels(dati[,1])-1)){
#' pred_level<-sapply(1:8,function(j) m$pred_val[[1]][[j]][,h])
#'  pred[,h] <-rowMeans(pred_level)}
#' }
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
# early.stop = 4
# write.forest = TRUE
# save.memory = FALSE
# id = NULL
# continue=0
# k=3
# system.time(prova<-power_ranger(y=1,training_frame=dati,validation_frame=dativ,early.stop=4))
# str(prova)

power_ranger <-
  function(x = NULL,
           y,
           training_frame,
           validation_frame = NULL,
           num.trees = 200,
           pmtry = NULL,
           n_forest = 4,
           random_forest = 2,
           eta = FALSE,
           num.threads = 30,
           work.dir = getwd(),
           # out.of.memory = T,
           early.stop = 10,
           continue = 0,
           write.forest = TRUE,
           save.memory = FALSE,
           id = NULL,
           k=NULL) {
    if(!is.null(pmtry))
      if(pmtry>1 | pmtry<0)
        stop('pmtry has to set 0<=pmtry<=1')
    out.of.memory<-T
    if (is.numeric(y))
      y <- colnames(training_frame)[y]
    if (is.null(x))
      x <-
        colnames(training_frame)[!colnames(training_frame) %in% y]
    if (is.numeric(x))
      x <- colnames(training_frame)[x]
    if(!is.null(k)) folds<-deepForest::kfold(training_frame[,y], k = k,stratified =F)
    formula <- as.formula(paste(y, '~.'))
    OOBerr <- matrix(NA, early.stop, n_forest + 1)
    valerr <- matrix(NA, early.stop, n_forest + 1)
    colnames(OOBerr) <-
      colnames(valerr) <-
      c(rep('et', n_forest - random_forest),
        rep('rf', random_forest),
        'mean')
    model <-
      sapply(1:early.stop, function(i)
        vector('list', n_forest), simplify = F)

    predtrain <-
      sapply(1:early.stop, function(i)
        vector('list', n_forest), simplify = F)
    predval <-
      sapply(1:early.stop, function(i)
        vector('list', n_forest), simplify = F)

    features_used <- vector('list', early.stop)
    features_used[[1]] <- x
    probability = is.factor(training_frame[, y])
    for (i in 1:early.stop) {
      x <-
        colnames(training_frame)[!colnames(training_frame) %in% y]
      if(eta & i>1){
        if(probability)
        features_used[[i]] <-colnames(training_frame)[(ncol(training_frame)-n_forest*(nlevels(training_frame[,y])-1)+1):ncol(training_frame)]
        else features_used[[i]] <-colnames(training_frame)[(ncol(training_frame)-n_forest+1):ncol(training_frame)]

        x<-features_used[[i]]
      }
      print(grep('predict',colnames(training_frame),value=T))
      if (n_forest - random_forest > 0) {
        for (j in 1:(n_forest - random_forest))
        {
          if (i <= continue) {
            load(paste(work.dir,'/','model', i, j, id, '.RData', sep = ''))
            model[[i]][[j]] <- m
            rm('m')
          }
          else
            model[[i]][[j]] <-
              ranger(
                formula,
                data = training_frame[, c(y, x)],
                num.trees = num.trees,
                mtry = 1,
                splitrule = 'extratrees',
                probability = probability,
                write.forest = write.forest,
                num.threads = num.threads,seed = sample(1:1e4,1)
              )
          if (!is.factor(training_frame[, y])) {
            if(is.null(k))
              predtrain[[i]][[j]] <- model[[i]][[j]]$predictions
            else {
              p<-sapply(1:k,function(h) ranger_cv(h,formula,training_frame[, c(y, x)],num.trees, folds,mtry=1,splitrule = 'extratrees',probability,write.forest, num.threads, save.memory=F),simplify=F)
              p<-unlist(p)
              predtrain[[i]][[j]]<-p[order(unlist(folds))]
            }
            if (!is.null(validation_frame))
              predval[[i]][[j]] <-
                predict(model[[i]][[j]], data = validation_frame)[[1]]
          }
          else
          {
            if(is.null(k))
              predtrain[[i]][[j]] <- model[[i]][[j]]$predictions[, -1]
            else
            {
              p<-sapply(1:k,function(h) ranger_cv(h,formula,training_frame[, c(y, x)],num.trees, folds,mtry=1,splitrule = 'extratrees',probability,write.forest, num.threads, save.memory=F),simplify=F)
              if(nlevels(training_frame[, y])>2){
                p<-do.call(rbind,p)
                predtrain[[i]][[j]]<-p[order(unlist(folds)),]
              }
              else {
                p<-unlist(p)
                predtrain[[i]][[j]]<-p[order(unlist(folds))]
              }
            }
            if (!is.null(validation_frame))
              predval[[i]][[j]] <-
                predict(model[[i]][[j]], data = validation_frame)[[1]][, -1]
          }
          OOBerr[i, j] <- model[[i]][[j]]$prediction.error
          if (out.of.memory)
          {
            m <- model[[i]][[j]]
            save(m, file = paste(work.dir,'/','model', i, j, id, '.RData', sep = ''))
            rm('m')
            model <-
              sapply(1:early.stop, function(i)
                vector('list', n_forest), simplify = F)
          }
        }
      }
      if (random_forest > 0) {
        for (j in (n_forest - random_forest + 1):n_forest)
        {
          if(is.null(pmtry)){
            mtry = if (!is.factor(training_frame[, y]))
              max(floor((length(x)) / 3), 1)
            else
              floor(sqrt(length(x)))
          }
          else mtry=length(x)*pmtry
          if (i <= continue) {
            load(paste(work.dir,'/','model', i, j, id, '.RData', sep = ''))
            model[[i]][[j]] <- m
            rm('m')
          }
          else
            model[[i]][[j]] <-
              ranger(
                formula,
                data = training_frame[, c(y, x)],
                num.trees,
                mtry = mtry,
                probability = probability,
                write.forest = write.forest,
                save.memory = save.memory,
                num.threads = num.threads,seed =  sample(1:1e4,1)
              )
          if (!is.factor(training_frame[, y])) {
            if(is.null(k))
              predtrain[[i]][[j]] <- model[[i]][[j]]$predictions
            else
            {
              p<-sapply(1:k,function(h) ranger_cv(h,formula,training_frame[, c(y, x)],num.trees, folds,mtry=mtry,splitrule = NULL,probability,write.forest, num.threads, save.memory=save.memory),simplify=F)
              p<-unlist(p)
              predtrain[[i]][[j]]<-p[order(unlist(folds))]
            }
            if (!is.null(validation_frame))
              predval[[i]][[j]] <-
                predict(model[[i]][[j]], data = validation_frame)[[1]]
          }
          else
          {
            if(is.null(k))
              predtrain[[i]][[j]] <- model[[i]][[j]]$predictions[, -1]
            else
            {
              p<-sapply(1:k,function(h) ranger_cv(h,formula,training_frame[, c(y, x)],num.trees, folds,mtry=mtry,splitrule = NULL,probability,write.forest, num.threads, save.memory=save.memory),simplify=F)
              if(nlevels(training_frame[, y])>2){
                p<-do.call(rbind,p)
                predtrain[[i]][[j]]<-p[order(unlist(folds)),]
              }
              else {
                p<-unlist(p)
                predtrain[[i]][[j]]<-p[order(unlist(folds))]
              }
            }
            if (!is.null(validation_frame))
              predval[[i]][[j]] <-
                predict(model[[i]][[j]], data = validation_frame)[[1]][, -1]
          }
          OOBerr[i, j] <- model[[i]][[j]]$prediction.error

          if (out.of.memory)
          {
            m <- model[[i]][[j]]
            save(m, file = paste(work.dir,'/','model', i, j, id, '.RData', sep = ''))
            rm('m')
            model <-
              sapply(1:early.stop, function(i)
                vector('list', n_forest), simplify = F)
          }
        }
      }
      if (!is.factor(training_frame[, y])) {

        OOBerr[i, n_forest + 1]<-ModelMetrics::mse(as.vector(training_frame[,y]),rowMeans(do.call(cbind,predtrain[[i]])))

      }

      if(nlevels(training_frame[,y])==2){
        pred <-rowMeans(do.call(cbind,predtrain[[i]]))
        OOBerr[i, n_forest + 1] <-
          Esame::tabella.sommario(pred,training_frame[, y],print=F)[[2]][,'err_tot']
      }
      #   else {
      #     pred<-matrix(0,nrow(training_frame),nlevels(training_frame[,y])-1)
      #     for(h in 1:(nlevels(training_frame[,y])-1)){
      #         pred_level<-sapply(1:length(predtrain[[i]]),function(j) predtrain[[i]][[j]][,h])
      #         pred[,h] <-rowMeans(do.call(cbind,pred_level))
      #     }
      #   }
      print(i)
      print('OOB.err')
      print(OOBerr[i, ])

      if(nlevels(training_frame[,y])>2){
        pred<-do.call(cbind,predtrain[[i]])
        training_frame <- cbind(training_frame,pred)
        colnames(training_frame)[!colnames(training_frame) %in% c(y,features_used[[1]])]<-paste('predict',1:(n_forest*(nlevels(training_frame[,y])-1)*i),sep='')
        if (!is.null(validation_frame))
        {
          pred<-do.call(cbind,predval[[i]])
          validation_frame <- cbind(validation_frame, pred)
          colnames(validation_frame)[!colnames(validation_frame) %in% c(y,features_used[[1]])]<-paste('predict',1:(n_forest*(nlevels(validation_frame[,y])-1)*i),sep='')
        }
      }
      else {
        training_frame <- cbind(training_frame, predtrain[[i]])

        colnames(training_frame)[(ncol(training_frame) -
                                    n_forest + 1):ncol(training_frame)] <-paste('predict',(1:n_forest)+n_forest*(i-1),sep='')

        if (!is.null(validation_frame))
        {
          validation_frame <- cbind(validation_frame, predval[[i]])

          colnames(validation_frame)[(ncol(validation_frame) -
                                        n_forest + 1):ncol(validation_frame)] <-paste('predict',(1:n_forest)+n_forest*(i-1),sep='')
        }
      }
    }
    esOOB <- which.min(na.omit(OOBerr[, n_forest + 1]))
    out <-
      list(
        sing_model = model,
        out.of.memory = out.of.memory,
        work.dir=work.dir,
        id = id,
        OOB = list(err = na.omit(OOBerr), early.stop = esOOB),
        model = list(
          features_used = features_used,
          y = y,
          ntrees = num.trees,
          n_forest = n_forest,
          random_forest = random_forest,
          perc_mtry = pmtry
          #  ,eta = eta
        ),
        pred_train = predtrain,
        pred_val=predval
      )
    class(out) <- 'dfOOB'
    return(out)
  }
