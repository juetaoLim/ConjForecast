#takes in train, test sets for independent (x) and dependent variables (y)
#pushes out list comprising:
#forecasts, which is a vector of model predictions given test set independent variables
#forecasts_r, residuals of forecasts to generate forecast SE for predictive density approximation
#forecasts_v, newey-west HAC SE of intercept term in forecasts
#approximate the predictive density variance, assuming that forecast generating process is Gaussian
#compute in-sample forecast errors and estimate HAC covariance matrix for intercept

#adapted from https://github.com/lcallot/lassovar/blob/master/R/lassovar-ada.R
#computes effective degrees of freedom for ridge regression/elastic net using the active set
ridge.df<-function(x,lambda){
  
  x<-(cbind(1,x))
  df.ridge <- NULL
  for(l in lambda){
    df.ridge<-c(df.ridge,sum(diag(x%*% solve(t(x) %*% x + l*diag(ncol(x)))%*% t(x))))
  }
  return(df.ridge)
}


forecaster <- function(x_train,x_test,y_train,y_names=k,naive_ind="AcuteConjunctivitis Lag 1"){
  #naive forecast
  y_naive= x_train[,which(colnames(x_train)==naive_ind)]
  e_naive = y_train - y_naive
  y_pred_naive = y_naive[length(y_naive)]
  v_na <- sum(e_naive^2)/(length(e_naive))
  
  #AR
  x_names <- sub(" .*", "", colnames(x_train))
  df_ar <- x_train[,which(x_names==y_names)]
  df_ar <- data.frame(y_train,df_ar,check.names = F)
  mod_ar <- glm(y_train~.,data=df_ar)
  mod_ar <- step(mod_ar,direction="backward",trace=0)
  
  mod_ar_nameind <- gsub('`', '', names(mod_ar$coefficients))
  if (length(mod_ar$coefficients) >1 & "(Intercept)" %in% mod_ar_nameind ){
    y_pred_ar <- mod_ar$coefficients %*% c(1,x_test[which(names(x_test) %in%mod_ar_nameind)])}
  
  if (length(mod_ar$coefficients) ==1 & "(Intercept)" %in% mod_ar_nameind){
    y_pred_ar <- mod_ar$coefficients}
  
  if (!("(Intercept)" %in% mod_ar_nameind)){
    y_pred_ar <- mod_ar$coefficients %*% c(x_test[which(names(x_test) %in%mod_ar_nameind)])}
  
  # y_pred_ar <- mod_ar$coefficients %*% c(1,x_test[which(x_names==y_names)])
  
  e_ar <- mod_ar$residuals
  v_ar <- (e_ar %*% e_ar)/(length(e_ar) - length(mod_ar$coefficients))
  #LASSO
  cv_lasso <- cv.glmnet(x=x_train,y=y_train,standardize=TRUE)
  mod_lasso <- glmnet(y=y_train,x=x_train,lambda=cv_lasso$lambda.min,standardize=TRUE)
  y_pred_lasso <- predict.glmnet(mod_lasso,newx=t(as.matrix(x_test)))
  e_las <- predict.glmnet(mod_lasso,newx=x_train) - y_train
  v_las <- sum(e_las^2)/(length(e_las) - length(which(mod_lasso$beta!=0)))
  
  #ridge
  cv_r <- cv.glmnet(x=x_train,y=y_train,standardize=TRUE,alpha=0)
  mod_r <- glmnet(y=y_train,x=x_train,lambda=cv_r$lambda.min,standardize=TRUE,alpha=0)
  y_pred_r <- predict.glmnet(mod_r,newx=t(as.matrix(x_test)))
  edf <- ridge.df(x=x_train,lambda = mod_r$lambda)
  e_r <- predict.glmnet(mod_r,newx=x_train) - y_train
  v_r <- sum(e_r^2)/(length(e_r) - edf)
 
  #enet 
  cv_enet <- cv.glmnet(x=x_train,y=y_train,standardize=TRUE,alpha=0.5)
  mod_enet <- glmnet(y=y_train,x=x_train,lambda=cv_enet$lambda.min,standardize=TRUE,alpha=0)
  y_pred_enet <- predict.glmnet(mod_enet,newx=t(as.matrix(x_test)))
  edf <- ridge.df(x=x_train,lambda = mod_enet$lambda)
  e_enet <- predict.glmnet(mod_enet,newx=x_train) - y_train
  v_enet <- sum(e_enet^2)/(length(e_enet) - edf)

  #boosting
  #boosting
  #boosting
  boost_df_train <- data.frame(Y=round(y_train),x_train,check.names=F)
  boost_df_test <- data.frame(t(x_test),check.names=F)
  mod_gbm <- gbm(Y ~ ., data = boost_df_train)
  pred_gbm <- predict(mod_gbm,newdata=boost_df_test)
  e_gbm <- predict(mod_gbm,newdata=boost_df_train) - y_train
  #no degrees of freedom, set equal to 1 for denominator
  v_gbm <- sum(e_gbm^2)/length(e_gbm)
  
  #sparse group LASSO, group lasso suppressed, takes too long for p > n problem
  #take groups as each covariate (has many lags)
  # groups <- sub(" .*", "", colnames(x_train))
  # groups <- as.numeric(as.factor(groups))
  
  #simple sort of numeric as lags are same
  # groups <- sort(groups)
  # cv_Group <- cv.gglasso(x=x_train,y=y_train, group = groups)
  # mod_gl <- gglasso(x=x_train,y=y_train,lambda=cv_Group$lambda.min)
  # y_pred_GL <- predict(mod_gl,newx=x_test)
  # 
  # e_gl <- predict(mod_gl,newx=x_train) - y_train
  # v_gl <- sum(e_gl^2)/(length(e_gl) - length(which(e_gl$beta!=0)))
  
  e_ar <- lm(mod_ar$residuals~1)
  ar_v2 <- NeweyWest(e_ar)
  
  las_r <- predict.glmnet(mod_lasso,newx=x_train) - y_train
  las_v2 <- lm(las_r~1)
  las_v2 <- NeweyWest(las_v2)
  
  r_r <- predict.glmnet(mod_r,newx=x_train) - y_train
  r_v2 <- lm(r_r~1)
  r_v2 <- NeweyWest(r_v2)
  
  na_r <- e_naive
  na_v2 <- lm(e_naive~1)
  na_v2 <- NeweyWest(na_v2)
  
  enet_r <- predict.glmnet(mod_enet,newx=x_train) - y_train
  enet_v2 <- lm(enet_r~1)
  enet_v2 <- NeweyWest(enet_v2)

  gbm_r <- mod_gbm$fit - y_train
  gbm_v2 <- lm(gbm_r~1)
  gbm_v2 <- NeweyWest(gbm_v2)

  # sgl_r <- predict(cv_sparseGroup,newx=x_train,s="lambda.min") - y_train
  # sgl_v2 <- lm(sgl_r~1)
  # sgl_v2 <- NeweyWest(sgl_v2)
  
  # forecast combination of disease case counts
  # forecasts <- c(y_pred_ar,y_pred_lasso,y_pred_r,y_pred_SGL,pred_gbm)
  # forecasts = c(forecasts,mean(forecasts))
  # names(forecasts) <- c('y_pred_ar','y_pred_lasso','y_pred_r','y_pred_SGL','pred_gbm',"combination")
  # 
  # forecasts_v <- c(v_ar,v_las,v_r,v_alas,v_gbm,v_sgl)
  # forecasts_v2 <- c(ar_v2,las_v2,r_v2,alas_v2,gbm_v2,sgl_v2)
  # forecasts_r <- cbind(mod_ar$residuals,las_r,r_r,alas_r,gbm_r,sgl_r)
  # 
  # names(forecasts_v2) <- c('y_pred_ar_var2','y_pred_lasso_var2','y_pred_r_var2','y_pred_alasso_var2','y_pred_SGL_var2','pred_gbm_var2')
  # colnames(forecasts_r) <- c('y_pred_ar_resid','y_pred_lasso_resid','y_pred_r_resid','y_pred_alasso_resid','y_pred_SGL_resid','pred_gbm_resid')
  # names(forecasts_v) <-c('y_pred_ar_var','y_pred_lasso_var','y_pred_r_var','y_pred_alasso_var','y_pred_SGL_var','pred_gbm_var')
  
  forecasts <- c(y_pred_naive,y_pred_ar,y_pred_lasso,y_pred_r,y_pred_enet,pred_gbm)
  forecasts = c(forecasts,mean(forecasts))
  names(forecasts) <- c('y_na','y_pred_ar','y_pred_lasso','y_pred_r',"y_pred_enet",'y_pred_gbm',"combination")
  
  forecasts_v <- c(v_na,v_ar,v_las,v_r,v_enet,v_gbm)
  forecasts_v2 <- c(na_v2,ar_v2,las_v2,r_v2,enet_v2,gbm_v2)
  forecasts_r <- cbind(na_r,mod_ar$residuals,las_r,r_r,enet_r,gbm_r)
  
  names(forecasts_v2) <- c('y_pred_na_var2','y_pred_ar_var2','y_pred_lasso_var2','y_pred_r_var2','y_pred_enet_var2','pred_gbm_var2')
  colnames(forecasts_r) <- c('y_pred_na_resid','y_pred_ar_resid','y_pred_lasso_resid','y_pred_r_resid','y_pred_enet_resid','pred_gbm_resid')
  names(forecasts_v) <-c('y_pred_na_var','y_pred_ar_var','y_pred_lasso_var','y_pred_r_var','y_pred_enet_var','pred_gbm_var')
  
  out <- list(forecasts,forecasts_v,forecasts_v2,forecasts_r)
  names(out) <- c("forecasts","forecasts_v","forecasts_v2","forecasts_r")
  return(out)
  
}

