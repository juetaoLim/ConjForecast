rm(list=ls())
cd <- "~/OneDrive - Nanyang Technological University/forecastSG2/"
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

require(forecast)
require(scoringRules)
computeAssessments <- function(y, 
                               yf,
                               f_var,
                               horizon){
  
  #compute forecast error metrics
  ind <- which(!is.na(yf[,1]))
  y <- y[ind]
  yfn <- yf[ind,]
  
  yf_d <- yfn[,-ncol(yfn)]
  f_var <- f_var[ind,]
  #density forecast scoring
  crps_sam <-logs_sam <-dss_sam <- list()
  for (i in 1:ncol(yf_d)){
    dens <- mapply(function(x,y){rnorm(n=10000,mean=x,sd=y^0.5)},x=yf_d[,i],y=f_var[,i],SIMPLIFY = F)
    dens <- do.call(rbind,dens)
    crps_sam[[i]] <- crps_sample(y=y,dat=dens)
    logs_sam[[i]] <- logs_sample(y=y,dat=dens)
    dss_sam[[i]]  <- dss_sample(y=y,dat=dens) 
  }
  
  crps_sam <- lapply(crps_sam,function(x){
    x <- x[which(is.numeric(x))]
    return(mean(x))
  })
  
  logs_sam <- lapply(logs_sam,function(x){
    x <- x[which(is.numeric(x))]
    return(mean(x))
  })
  
  dss_sam <- lapply(dss_sam,function(x){
    x <- x[which(is.numeric(x))]
    return(mean(x))
  })
  
  crps_sam <- c(unlist(crps_sam),NA) #append back forecast combinations, as not evalauted for density forecasts
  logs_sam <- c(unlist(logs_sam),NA)
  dss_sam <- c(unlist(dss_sam),NA)
  
  
  #point forecast scoring
  mae <- abs(y - yfn)
  mae <- apply(mae,MARGIN=2,mean)
  
  rmse <- (y - yfn)^2
  rmse <- apply(rmse,MARGIN=2,mean)
  rmse <- sqrt(rmse)
  
  mape <- abs((y - yfn)/y)
  mape <- apply(mape,MARGIN=2,mean)
  
  mase <- mae[2:7]/mae[1]
  mase <- c(1,mase)
  
  assessments <- rbind(mae,rmse,mape,mase,crps_sam,logs_sam,dss_sam)
  rownames(assessments) <- c('mae','rmse','mape','mase','crps','logs','sam')
  
  
  #compute forecast errors pairwise, compute pairwise DM test p-value for one sided test at 0.05 level
  errors = y - yfn
  if(colSums(errors)[2]==colSums(errors)[3]) { errors <- errors[,-3]}
  store <- list()
  for (i in 1:ncol(errors)){
    store[[i]] <- apply(errors[,-i],
                        MARGIN=2,
                        function(x,y=errors[,i],z=horizon)dm.test(x,y,alternative="two.sided",h=z,power=1))
    store[[i]] <- lapply(store[[i]],function(x)x$p.value)
    store[[i]] <- unlist(store[[i]])
    store[[i]] <- append(store[[i]],values=1,after=i-1)
  }
  
  store <- do.call(rbind,store)
  rownames(store) <- colnames(errors)
  round(store,digits=3)
  
  
  out <- list(assessments,store)
  return(out)
}


covid_ind <- 36+52+52


storeAssessments <- list()
for (i in 1:length(storeForecasts)){
  
  Y <- storeForecasts[[i]][,20]
  Yf <- storeForecasts[[i]][,1:7]
  F_var <- storeForecasts[[i]][,14:19]
  
  ind <- seq(1,length(Y)-covid_ind)
  
  storeAssessments[[i]] <- computeAssessments(y=Y[ind], 
                                              yf=Yf[ind,], f_var = F_var[ind,],
                                              horizon=i)
}

#do as plots

mae <-  do.call(rbind,lapply(storeAssessments,function(x)x[[1]][1,]))
rmse <- do.call(rbind,lapply(storeAssessments,function(x)x[[1]][2,]))
mape <- do.call(rbind,lapply(storeAssessments,function(x)x[[1]][3,]))
mase <- do.call(rbind,lapply(storeAssessments,function(x)x[[1]][4,]))

assessmentPlotter1 <- function(metricMatrix,
                               xlab="Forecast Horizon (Weeks Ahead)",
                               ylab,col=c("red","blue","darkgreen","cadetblue4","orange","black","red"),
                               panelName,
                               leg=F,PCH=c(13,15,16,17,18,19,8)){
  names <- c("Naive","AR (Baseline)","LASSO","Ridge","ENET","GBM","Comb")
  horizon = 1:8
  plot(y=c(min(c(metricMatrix),na.rm=T),max(c(metricMatrix),na.rm=T)),
       x=c(1,nrow(metricMatrix)),
       col="white",
       xaxt='n',
       xlab=xlab,
       ylab=ylab)
  
  for (i in 1:ncol(metricMatrix)){
    lines(metricMatrix[,i],col=col[i])
    points(y=metricMatrix[,i],x=seq(1,length(metricMatrix[,i])),col=col[i],pch=PCH[i],cex=1.2)
  }
  
  if (leg){
    legend(x="topleft",legend=names,col=col,pch=PCH,bty='n')
  }  
  axis(side=1,at=seq(0,nrow(metricMatrix),by=1))
  mtext(panelName,side=3,adj=0,cex=0.8)
  box()
  
}

pdf(paste0(cd,"/out/forecast_plots/forecasts3_nocovid.pdf"),width=10,height=5)
# par(las=1,cex.axis=0.8)
par(las=1,cex.axis=0.9,cex.lab=0.85,mfrow=c(1,4),pty='s',mar=c(0.2,0.1,0.1,0.1),mai=c(0.2,0.55,0.1,0.2), mgp=c(3,1,0))
assessmentPlotter1(metricMatrix=mape,ylab="Mean Absolute Percentage Error",panelName="A",leg=T)
assessmentPlotter1(metricMatrix=rmse,ylab="Mean Squared Forecast Error",panelName="B")
assessmentPlotter1(metricMatrix=mae,ylab="Mean Absolute Forecast Error",panelName="C")
assessmentPlotter1(metricMatrix=mase,ylab="Mean Absolute Scaled Error",panelName="D")
dev.off()



##plot heatmaps for DM test statistics
# temp <- storeAssessments[[1]][[2]]
# temp[which(temp>=0.05)] <- 1
# temp[which(temp<0.05)] <- 0
# rownames(temp) = colnames(temp) <- c("AR","LASSO","GBM","Comb","Naive")
require(pheatmap)
require(gridExtra)
require(ggplot2)
storePlots <- list()
labs <- c("A","B","C","D","E","F","G","H")
for (i in 1:length(storeAssessments)){
  temp <- storeAssessments[[i]][[2]]
  temp[which(temp>=0.05)] <- 1
  temp[which(temp<0.05)] <- 0
  rownames(temp) = colnames(temp) <- c("NAI","AR","LAS",'RR','ENET',"GBM","CO")
  leg=T
  if (i<8){leg=F}
  temp[lower.tri(temp)] <- NA
  diag(temp) <- NA
  temp <- pheatmap::pheatmap(temp,
                             border_color = "lightgrey",
                             color=c("red","black"), 
                             treeheight_row = 0,
                             treeheight_col = 0,cluster_rows=F, cluster_cols=F,
                             legend=leg,
                             legend_breaks=c(0,1),
                             legend_labels=c("NE","E"),main=paste0(labs[i],": Horizon ",i))
  
  
  storePlots[[i]]  <- temp[[4]]
}

#hack, as all forecasts equivalent at horizon 1
storePlots[[1]] <- storePlots[[2]]
g <- grid.arrange(arrangeGrob(grobs= storePlots,ncol=4))
ggsave(paste0(cd,"/out/forecast_plots/forecasts4_nocovid.pdf"),g)



#do as tables for (density forecast assessment)
tab_rownames <- paste("Horizon", 1:8)
tab_colnames <- c("Naive", 'AR','LASSO','Ridge','Elastic Net', 'GBM', 'Forecast Combination')
crps <-  do.call(rbind,lapply(storeAssessments,function(x)x[[1]][5,]))
logs <- do.call(rbind,lapply(storeAssessments,function(x)x[[1]][6,]))
dss <- do.call(rbind,lapply(storeAssessments,function(x)x[[1]][7,]))
colnames(crps) <- colnames(logs) <- colnames(dss) <-  tab_colnames
rownames(crps) <- rownames(logs) <- rownames(dss) <- tab_rownames
write.csv(crps,paste0(cd,"out/forecast_tables/crps_nocov.csv"))
write.csv(logs,paste0(cd,"out/forecast_tables/logs_nocov.csv"))
write.csv(dss,paste0(cd,"out/forecast_tables/dss_nocov.csv"))
