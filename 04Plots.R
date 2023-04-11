rm(list=ls())
cd <- "~/OneDrive - Nanyang Technological University/forecastSG2/"

#function plots forecasts

forecastPlotter <- function(y,yf,xLab,yLab,stepAhead,legendModLab){
  plot(y=c(min(c(y,yf),na.rm=T),max(c(y,yf),na.rm=T)),
       x=c(0,length(y)),
       col="white",
       xaxt='n',
       xlab=xLab,
       ylab=yLab)
  train_ind <- max(which(is.na(yf)))
  polygon(x=c(-100,train_ind,train_ind,-100),y=c(-9999,-9999,9999,9999),col="azure2",border="white")
  points(y=yf,x=seq(1,length(yf)),col="darkred",pch=20)
  lines(y=y,x=seq(1,length(y)))
  legend(x="topright",legend=c("Observed",paste0(legendModLab," ",stepAhead," week ahead forecast")),lty=c(1,NA),pch=c(NA,20),col=c("black","darkred"),bty='n',cex=0.8)
  mtext(text="train data",side=1,adj=0.1,padj=-2)
  mtext(text="test data",side=1,adj=0.9,padj=-2)
  axis(side=1,at=seq(0,length(y),by=52),labels=F)
  axis(side=1,at=seq(0,length(y),by=52)+26,labels=seq(14,22,by=1),tick=F)
  box()
  
}

#function plots forecast against actual 
files_p <- list.files(paste0(cd,"/out/forecast_ouput/point/"))

for (j in files_p){
 load(paste0(cd,"out/forecast_ouput/point/",j))
}

files_r <- list.files(paste0(cd,"/out/forecast_ouput/resid/"))

for (j in files_r){
  load(paste0(cd,"out/forecast_ouput/resid/",j))
}

storeForecasts <-list()
storeResids <- list()
for (j in 1:8){
  storeForecasts[[j]] <- get(paste0(j,"_Forecast"))
  storeResids[[j]] <- get(paste0(j,"_Forecast_resids"))
}



for (i in 1:length(storeForecasts)){
  temp <- storeForecasts[[i]]
  
  pdf(paste0(cd,"/out/forecast_plots/breakdown/forecastsCombiStepAhead",i,".pdf"),width=8,height=5)
  par(las=1,cex.axis=0.8)
  forecastPlotter(y=temp[,20],
                  yf=temp[,7],
                  xLab="Year",
                  yLab="Conjunctivitis Case Counts",
                  legendModLab="Forecast combination",
                  stepAhead=i)
  dev.off()
  
  pdf(paste0(cd,"/out/forecast_plots/breakdown/forecastsARStepAhead",i,".pdf"),width=8,height=5)
  par(las=1,cex.axis=0.8)
  forecastPlotter(y=temp[,20],
                  yf=temp[,2],
                  xLab="Year",
                  yLab="Conjunctivitis Case Counts",
                  legendModLab="AR (Baseline)",
                  stepAhead=i)
  dev.off()
  
  pdf(paste0(cd,"/out/forecast_plots/breakdown/forecastsLASSOStepAhead",i,".pdf"),width=8,height=5)
  par(las=1,cex.axis=0.8)
  forecastPlotter(y=temp[,20],
                  yf=temp[,3],
                  xLab="Year",
                  yLab="Conjunctivitis Case Counts",
                  legendModLab="LASSO",
                  stepAhead=i)
  dev.off()
  
  pdf(paste0(cd,"/out/forecast_plots/breakdown/forecastsENETStepAhead",i,".pdf"),width=8,height=5)
  par(las=1,cex.axis=0.8)
  forecastPlotter(y=temp[,20],
                  yf=temp[,5],
                  xLab="Year",
                  yLab="Conjunctivitis Case Counts",
                  legendModLab="Elastic Net",
                  stepAhead=i)
  dev.off()
  
  pdf(paste0(cd,"/out/forecast_plots/breakdown/forecastsRIDGEStepAhead",i,".pdf"),width=8,height=5)
  par(las=1,cex.axis=0.8)
  forecastPlotter(y=temp[,20],
                  yf=temp[,4],
                  xLab="Year",
                  yLab="Conjunctivitis Case Counts",
                  legendModLab="Ridge",
                  stepAhead=i)
  dev.off()
  
  pdf(paste0(cd,"/out/forecast_plots/breakdown/forecastsGBMStepAhead",i,".pdf"),width=8,height=5)
  par(las=1,cex.axis=0.8)
  forecastPlotter(y=temp[,20],
                  yf=temp[,6],
                  xLab="Year",
                  yLab="Conjunctivitis Case Counts",
                  legendModLab="GBM",
                  stepAhead=i)
  dev.off()
  
  pdf(paste0(cd,"/out/forecast_plots/breakdown/forecastsNAIVEStepAhead",i,".pdf"),width=8,height=5)
  par(las=1,cex.axis=0.8)
  forecastPlotter(y=temp[,20],
                  yf=temp[,1],
                  xLab="Year",
                  yLab="Conjunctivitis Case Counts",
                  legendModLab="Naive",
                  stepAhead=i)
  dev.off()
  
}




forecastPlotter2 <- function(y,yf,xLab,yLab,stepAhead,panelLab){
  y  <- y/10
  yf <- yf/10
  plot(y=c(min(c(y,yf),na.rm=T),max(c(y,yf),na.rm=T)),
       x=c(min(c(y,yf),na.rm=T),max(c(y,yf),na.rm=T)),
       col="white",
       xlab=xLab,
       ylab=yLab)
  abline(a=0,b=1, col = "gray")
  points(y=yf,x=y,col="darkred",pch=20)
  # legend(x="topleft",legend=c("fore",paste0(stepAhead," week ahead forecast")),lty=c(1,NA),pch=c(NA,20),col=c("black","darkred"),bty='n',cex=0.8)
  mtext(text="underpredict",side=1,adj=0.9,padj=-2,cex=0.8)
  mtext(text="overpredict",side=3,adj=0.1,padj=2,cex=0.8)
  mtext(panelLab,side=3,adj=0,cex=0.8)
  box()
  
}

pdf(paste0(cd,"/out/forecast_plots/forecasts2.pdf"),width=10,height=5)
# par(las=1,cex.axis=0.8)
panelName = c("A","B","C","D","E","F","G","H")
par(las=1,cex.axis=0.9,cex.lab=0.85,mfrow=c(2,4),pty='s',mar=c(0.2,0.1,0.1,0.1),mai=c(0.2,0.55,0.1,0.2), mgp=c(2,1,0))
for (i in 1:length(storeForecasts)){
  temp <- storeForecasts[[i]]
  forecastPlotter2(y=temp[,20],
                  yf=temp[,7],
                  xLab="Observation",
                  yLab=paste0("Forecast Horizon ",i),
                  stepAhead=i,
                  panelLab = panelName[i])
}

dev.off()

dataPlotter <- function(y,xLab,yLab,panelLab){
  plot(y=c(min(y,na.rm=T),max(y,na.rm=T)),
       x=c(0,length(y)),
       col="white",
       xaxt='n',
       xlab=xLab,
       ylab=yLab)
  lines(y=y,x=seq(1,length(y)))
  axis(side=1,at=seq(0,length(y),by=52),labels=F)
  axis(side=1,at=seq(0,length(y),by=52)+26,labels=seq(14,22,by=1),tick=F,cex.axis=0.7)
  axis(side=1,at=seq(0,length(y),by=4),labels=F,col.ticks = "darkred",lwd.ticks=0.2,tck=-0.03)
  mtext(panelLab,side=3,adj=0,cex=0.8)
  box()
  
}

#plots for URTI case counts and weather
load(paste0(cd,"out/merged_2021-2012.rds"))
pdf(paste0(cd,"/out/forecast_plots/data.pdf"),width=5,height=9)
par(las=1,cex.axis=0.9,cex.lab=0.85,mfrow=c(5,2),mar=c(1,3.5,1,0.5),mai=c(0.3,0.55,0.2,0.2))
dataPlotter(y= as.numeric(df_all$Acute.Upper.Respiratory.Tract.infections),
                xLab="",
                yLab="Acute Conjunctivitis Case Counts",
            panelLab='A')

dataPlotter(y= as.numeric(df_all$mean_temp),
            xLab="",
            yLab="Mean Temperature",
            panelLab='B')

dataPlotter(y= as.numeric(df_all$mean_tp),
            xLab="",
            yLab="Total Precipitation",
            panelLab='C')

dataPlotter(y= as.numeric(df_all$mean_rh),
            xLab="",
            yLab="Relative Humidity",
            panelLab='D')

dataPlotter(y= as.numeric(df_all$mean_ah),
            xLab="",
            yLab="Absolute Humidity",
            panelLab='E')

dataPlotter(y= as.numeric(df_all$pm25),
            xLab="",
            yLab=expression('PM'[2.5]*' Surface Concentration'), 
            panelLab='F')

dataPlotter(y= as.numeric(df_all$pm10),
            xLab="",
            yLab=expression('PM'[10]*' Surface Concentration'),
            panelLab='G')

dataPlotter(y= as.numeric(df_all$o3),
            xLab="",
            yLab=expression('O'[3]*' Surface Concentration'),
            panelLab='H')

dataPlotter(y= as.numeric(df_all$so2),
            xLab="",
            yLab=expression('SO'[2]*' Surface Concentration'),
            panelLab='I')

dataPlotter(y= as.numeric(df_all$co),
            xLab="",
            yLab="CO Surface Concentration",
            panelLab='J')


dev.off()

#density forecasts#

forecastPlotter3 <- function(y,yf,f_var,xLab,yLab,stepAhead,legendModLab){
  dens <- mapply(function(x,y){rnorm(n=10000,mean=x,sd=y^0.5)},x=yf,y=f_var)
  dens <- apply(dens,MARGIN=2,function(x)quantile(x,probs=c(0.25,0.975),na.rm=T))
  dens_u <- apply(dens,MARGIN=2,max)
  dens_l <- apply(dens,MARGIN=2,min)
  xLab = "Year"
  yLab = "Conjunctivitis Case Counts"
  plot(y=c(min(c(y,yf),na.rm=T),max(c(y,yf),na.rm=T)),
       x=c(0,length(y)),
       col="white",
       xaxt='n',
       xlab=xLab,
       ylab=yLab)
  train_ind <- max(which(is.na(yf)))
  polygon(x=c(-100,train_ind,train_ind,-100),y=c(-9999,-9999,9999,9999),col="azure2",border="white")
  #plot density
  x_poly_dens <- which(!is.na(dens_u))
  
  polygon(x=c(x_poly_dens,rev(x_poly_dens),min(x_poly_dens)),
          y=c(dens_u[x_poly_dens],dens_l[rev(x_poly_dens)],dens_u[min(x_poly_dens)]),col=rgb(1, 0, 0,0.5),border=rgb(1, 0, 0,0.5))
  
  points(y=yf,x=seq(1,length(yf)),col="blue",pch=20,cex=0.5)
  lines(y=y,x=seq(1,length(y)))
  legend(x="topright",
         legend=c("Observed",paste0(legendModLab," ",stepAhead," week point forecast"),
                  paste0(legendModLab," ",stepAhead," week density forecast")),
         lty=c(1,NA,NA),
         pch=c(NA,20,15),
         col=c("black","blue",rgb(1, 0, 0,0.5)),bty='n',cex=0.8)
  
  mtext(text="train data",side=1,adj=0.1,padj=-2)
  mtext(text="test data",side=1,adj=0.9,padj=-2)
  axis(side=1,at=seq(0,length(y),by=52),labels=F)
  axis(side=1,at=seq(0,length(y),by=52)+26,labels=seq(14,22,by=1),tick=F)
  box()
  
}

# colnames(temp)
# y=temp[,16]
# yf=temp[,5]
# f_var = temp[,11]
#compute density forecast approximation, using SD as newey-west HAC estimator

for (i in 1:length(storeForecasts)){
  temp <- storeForecasts[[i]]
  
  # pdf(paste0(cd,"/out/forecast_plots/breakdown/densforecastsCombiStepAhead",i,".pdf"),width=8,height=5)
  # par(las=1,cex.axis=0.8)
  # forecastPlotter(y=temp[,16],
  #                 yf=temp[,5],
  #                 f_var=temp[]
  #                 xLab="Year",
  #                 yLab="Conjunctivitis Case Counts",
  #                 legendModLab="Forecast combination",
  #                 stepAhead=i)
  # dev.off()
  
  pdf(paste0(cd,"/out/forecast_plots/breakdown/densforecastsNAStepAhead",i,".pdf"),width=8,height=5)
  par(las=1,cex.axis=0.8)
  forecastPlotter3(y=temp[,20],
                   yf=temp[,1],
                   f_var=temp[,8],
                   xLab="Year",
                   yLab="Conjunctivitis Case Counts",
                   legendModLab="Naive",
                   stepAhead=i)
  dev.off()
  
  pdf(paste0(cd,"/out/forecast_plots/breakdown/densforecastsARStepAhead",i,".pdf"),width=8,height=5)
  par(las=1,cex.axis=0.8)
  forecastPlotter3(y=temp[,20],
                  yf=temp[,2],
                  f_var=temp[,9],
                  xLab="Year",
                  yLab="Conjunctivitis Case Counts",
                  legendModLab="AR (Baseline)",
                  stepAhead=i)
  dev.off()
  
  pdf(paste0(cd,"/out/forecast_plots/breakdown/densforecastsLASStepAhead",i,".pdf"),width=8,height=5)
  par(las=1,cex.axis=0.8)
  forecastPlotter3(y=temp[,20],
                  yf=temp[,3],
                  f_var=temp[,10],
                  xLab="Year",
                  yLab="Conjunctivitis Case Counts",
                  legendModLab="LASSO",
                  stepAhead=i)
  dev.off()
  
  pdf(paste0(cd,"/out/forecast_plots/breakdown/densforecastsRidgeStepAhead",i,".pdf"),width=8,height=5)
  par(las=1,cex.axis=0.8)
  forecastPlotter3(y=temp[,20],
                   yf=temp[,4],
                   f_var=temp[,11],
                   xLab="Year",
                   yLab="Conjunctivitis Case Counts",
                   legendModLab="Ridge",
                   stepAhead=i)
  dev.off()
  
  pdf(paste0(cd,"/out/forecast_plots/breakdown/densforecastsENETStepAhead",i,".pdf"),width=8,height=5)
  par(las=1,cex.axis=0.8)
  forecastPlotter3(y=temp[,20],
                  yf=temp[,5],
                  f_var=temp[,12],
                  xLab="Year",
                  yLab="Conjunctivitis Case Counts",
                  legendModLab="Elastic Net",
                  stepAhead=i)
  dev.off()
  
  pdf(paste0(cd,"/out/forecast_plots/breakdown/densforecastsGBMStepAhead",i,".pdf"),width=8,height=5)
  par(las=1,cex.axis=0.8)
  forecastPlotter3(y=temp[,20],
                  yf=temp[,6],
                  f_var=temp[,13],
                  xLab="Year",
                  yLab="Conjunctivitis Case Counts",
                  legendModLab="GBM",
                  stepAhead=i)
  dev.off()
  
}

