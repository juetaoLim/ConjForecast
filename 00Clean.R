rm(list=ls())
library(readxl)
library(dplyr)
cd <- "~/OneDrive - Nanyang Technological University/forecastSG2/"

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

df <- read_excel_allsheets(paste0(cd,"raw/2021-2012.xlsx"))
df <- lapply(df,function(x) {
                            colnames(x) <- x[1,]
                            x <- x[-1,]
                            x$`NA` <- NULL
                            # x$Start <- NULL
                            # x$End <- NULL
                            # x$`Start.to.End` <- NULL
                            # x <- x[complete.cases(x),]
                            x$`Epidemiology Wk`[which(nchar(x$`Epidemiology Wk`)==1)] <- paste("0",x$`Epidemiology Wk`[which(nchar(x$`Epidemiology Wk`)==1)],sep="")
                            return(x)})
for (i in 1:length(df)){
  df[[i]] <- data.frame(df[[i]],epiInd = paste(names(df)[i],df[[i]]$`Epidemiology Wk`,sep="-"))
}

df_all <- bind_rows(df)
df_all <- df_all[order(df_all$epiInd),]

#read in climate files, aggreate to min, mean, max temp by epiomd

df.csv <- read.csv(paste0(cd,"raw/Climate_2012to2022.csv"))
df.csv$`epiweekV`[which(nchar(df.csv$`epiweekV`)==1)] <- paste("0",df.csv$`epiweekV`[which(nchar(df.csv$`epiweekV`)==1)],sep="")
df.csv <- df.csv %>% mutate(epiInd = paste0("20",substr(df.csv$dateV,start=7,stop=9),"-",df.csv$epiweekV)) %>%
                  group_by(epiInd)  %>%
                  summarize(mean_temp = mean(V2mtemperature_k,na.rm=T),
                            min_temp = min(V2mtemperature_k,na.rm=T),
                            max_temp = max(V2mtemperature_k,na.rm=T),
                            mean_tp = mean(VTotalprecipitation_m,na.rm=T),
                            min_tp = min(VTotalprecipitation_m,na.rm=T),
                            max_tp = max(VTotalprecipitation_m,na.rm=T),
                            mean_ah = mean(AbsoluteHumidity_gmminus3,na.rm=T),
                            min_ah = min(AbsoluteHumidity_gmminus3,na.rm=T),
                            max_ah = max(AbsoluteHumidity_gmminus3,na.rm=T),
                            mean_rh = mean(RelativeHumidity1_perc,na.rm=T),
                            min_rh = min(RelativeHumidity1_perc,na.rm=T),
                            max_rh = max(RelativeHumidity1_perc,na.rm=T),
                            mean_lfi = mean(VLeafareaindexHIGH_ind,na.rm=T),
                            min_lfi = min(VLeafareaindexHIGH_ind,na.rm=T),
                            max_lfi = max(VLeafareaindexHIGH_ind,na.rm=T))

df_all <- merge(df_all,df.csv,by='epiInd')

require(MMWRweek)
#read in ambient air pollutant dataframe
cd_aap <- paste0(cd, "raw/aap")
aap_store <- lapply(paste(cd_aap,list.files(cd_aap),sep="/"),read.csv)

#clean data for each sector's measurement
aap_store <- lapply(aap_store,function(x){
  x$date <- as.Date(x$date)
  #change dates to epiweek
  epiweek_vec <- MMWRweek(x$date)
  epiweek_vec
  epiweek_vec$MMWRweek[which(nchar(epiweek_vec$MMWRweek)==1)] <- paste("0",epiweek_vec$MMWRweek[which(nchar(epiweek_vec$MMWRweek)==1)],sep="")
  epiInd <- paste(epiweek_vec$MMWRyear,epiweek_vec$MMWRweek,sep="-")
  x <- data.frame(x,epiInd=epiInd)
  #aggregate at epiInd level
  x <- x %>% 
    group_by(epiInd)  %>%
    summarize(pm25 = mean(pm25,na.rm=T),
              pm10 = mean(pm10,na.rm=T),
              o3 = mean(o3,na.rm=T),
              no2 = mean(no2,na.rm=T),
              so2 = mean(so2,na.rm=T),
              co = mean(co,na.rm=T),
              psi = mean(psi,na.rm=T))
  x$no2 <- NULL
  return(x)
})

aap_store <- do.call(rbind,aap_store)
aap_store <- aap_store %>%
            group_by(epiInd)  %>%
            summarize(pm25 = mean(pm25,na.rm=T),
                      pm10 = mean(pm10,na.rm=T),
                      o3 = mean(o3,na.rm=T),
                      so2 = mean(so2,na.rm=T),
                      co = mean(co,na.rm=T),
                      psi = mean(psi,na.rm=T))

df_all <-merge(df_all,aap_store,by='epiInd')
#ind <- which(!(aap_store$epiInd %in% df_all$epiInd))
#check merge
save(df_all,file=paste0(cd,"/out/merged_2021-2012.rds"))
