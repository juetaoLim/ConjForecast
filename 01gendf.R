##generates forecast dataframes for seperate conditions for forecasts to be assessed upon
rm(list=ls())
cd <- "~/OneDrive - Nanyang Technological University/forecastSG2/"
load(file=paste(cd,"out/merged_2021-2012.rds",sep=""))
#order dataframe according to dates
df_all <- df_all[order(df_all$epiInd),]
#take dates as seperate dataframe
dates <- df_all$epiInd
#generate lag matrix for weather
#weather <- df$weather

#remove dots from column name

#remove imported diseases, diseases with consistently 0 case counts, generate name list from df_all
df_all$Epidemiology.Wk <- NULL
df_all$Start <- NULL
df_all$Start.to.End <- NULL
df_all$End <- NULL
df_all$epiInd <- NULL
df_all$Murine.Typhus <- NULL
df_all$Botulism <- NULL
df_all$Tetanus <- NULL
df_all$Japanese.Encephalitis <- NULL
df_all$Ebola.Virus.Disease <- NULL
df_all$Zika <- NULL
df_all$Avian.Influenza <- NULL
df_all$Haemophilus.influenzae.type.b <- NULL
df_all$Pertussis <- NULL
df_all$Leptospirosis <- NULL
df_all$Nipah <- NULL
df_all$SARS <- NULL
df_all$Rubella <- NULL
df_all$Monkeypox <- NULL
df_all$Diphtheria <- NULL
df_all$Chikungunya.Fever <- NULL
df_all$Malaria <- NULL
df_all$Yellow.Fever <- NULL
df_all$Plague <- NULL
df_all$Poliomyelitis <- NULL
df_all$Cholera <- NULL
df_all$Encephalitis <- NULL
df_all$psi <- NULL
df_all <- data.frame(apply(df_all,MARGIN=2,as.numeric))

names(df_all) <- gsub("\\.", "", names(df_all))
#generate forecast dataframes based on specific time horizons
require(quantmod)
n_lags <- 1:8

#first, generate base dataframes based on number of lags
#define forecast horizon in weeks, consider only direct forecasts
#each dataframe is based on the forecast horizon 
forecast_horizon <- 1:12
store_X <- list()
for (j in forecast_horizon){
  for (i in 1:ncol(df_all)){
    temp_x <- df_all
    X <- list()
    for (ii in 1:ncol(temp_x)){
    temp_x_component   <- Lag(temp_x[,ii],k=n_lags + j -1)
    colnames(temp_x_component) <- paste(colnames(temp_x)[ii],"Lag",n_lags)
    X[[ii]] <- temp_x_component
    }
  }
  store_X[[j]] <- X
}

store_X <- lapply(store_X,function(x)do.call(cbind,x))


#manually only select non-weather, disease variables in df_all
disease_ind <- 1:21
#generate individual dataframe lists based on disease type
for (i in disease_ind){
  temp_y <- df_all[,i]

  #append forecast variables into dataframe
  temp_y_large <- lapply(store_X,function(x,y=temp_y,z=colnames(df_all)[i]){out <- cbind(y,x)
                                                                            colnames(out)[1] <- z
                                                                            return(out)})
  #only take complete entries
  temp_y_large <- lapply(temp_y_large,function(x)x[complete.cases(x),])
  
  names(temp_y_large) <- paste("Forecast Horizon",forecast_horizon)
  #name the first entry
  assign(colnames(df_all)[i],temp_y_large)
  save(list=colnames(df_all)[i],file=paste0(cd,"out/forecast_df/",colnames(df_all)[i],".rds"))
}





