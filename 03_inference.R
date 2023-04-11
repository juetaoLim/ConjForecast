##load in specific dataframe
rm(list=ls())
require(glmnet)
require(stargazer)
cd <- "~/OneDrive - Nanyang Technological University/forecastSG2/"
files <- list.files(paste0(cd,"out/forecast_df/"))
source(paste0(cd,"functions/forecasts.R"))
#create directory for forecasts to be stored
subDir <- file.path(paste0(cd,"out/inference_ouput/"))
ifelse(!dir.exists(subDir), dir.create(subDir), FALSE)

#disease names and lists to be referenced later
dis_names <- gsub("\\..*","", files)

store_dis <- list()
k<-1
for (i in files){
  
  load(paste0(cd,"out/forecast_df/",i)) 
  temp_list <- get(dis_names[k])
  temp_forecast_store <- list()
  
  store_glm <- list()
  
  #for each time horizon, generate point forecasts using specified models
  for (j in 1:length(temp_list)){
    temp_df <- temp_list[[j]]
    x = temp_df[,-1]
    y = temp_df[,1]
    #lasso as selection
    cv_lasso <- cv.glmnet(x=x,y=y,standardize=TRUE)
    mod_lasso <- glmnet(y=y,x=x,lambda=cv_lasso$lambda.min,standardize=TRUE)
    #glm as inference
    coef_ind <- which(mod_lasso$beta!=0)
    glm_df <- data.frame(y=y,x[,coef_ind],check.names=F)
    mod_glm <- glm(y~.,data=glm_df)
    store_glm[[j]] <- mod_glm
  }
  names(store_glm) <- paste("Horizon", 1:length(temp_list))
  store_dis[[k]] <- store_glm
  k <- k + 1
}
names(store_dis) <- dis_names
#generate nice tables
for (i in 1:length(store_dis)){
  
  stargazer(store_dis[[i]][1:3],out=paste0(subDir,dis_names[i],"1_to_3.txt"),title=paste0("Coefficients for ",dis_names[i]," Horizon 1 to 3" ),column.labels = paste0("Horizon ",1:3))  
  stargazer(store_dis[[i]][4:6],out=paste0(subDir,dis_names[i],"4_to_6.txt"),title=paste0("Coefficients for ",dis_names[i]," Horizon 4 to 6" ),column.labels = paste0("Horizon ",4:6))    
  stargazer(store_dis[[i]][7:9],out=paste0(subDir,dis_names[i],"7_to_9.txt"),title=paste0("Coefficients for ",dis_names[i]," Horizon 7 to 9" ),column.labels = paste0("Horizon ",7:9))    
  stargazer(store_dis[[i]][10:12],out=paste0(subDir,dis_names[i],"10_to_12.txt"),title=paste0("Coefficients for ",dis_names[i]," Horizon 10 to 12" ),column.labels = paste0("Horizon ",10:12))    
  
}

