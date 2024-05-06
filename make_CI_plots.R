require(rstan)
require(bayesplot)
require(ggplot2)

var.list <- dir('fitted_models/individual')

betas <- list()

for (index in 1:length(var.list)) {
  load(paste('fitted_models/individual/',var.list[index],sep=''))
  betas[[index]] <- extract(fit)$mu
}

#saveRDS(list(var.list,betas),file='individual_model_fits.RDS')

#my.list <- readRDS('individual_model_fits.RDS')
#var.list <- my.list[[1]]
#betas <- my.list[[2]]

var.names <- unname(sapply(var.list,function(x){paste(unlist(strsplit(x,'_'))[4:5],collapse='_')}))

gain.rates <- NULL

mutation.rates <- NULL

death.rates <- NULL

for (i in 1:length(var.names)) {
  gain.rates <- cbind(gain.rates,exp(betas[[i]][,2]-betas[[i]][,1]))
  mutation.rates <- cbind(mutation.rates,exp(betas[[i]][,4]-betas[[i]][,6]))
  death.rates <- cbind(death.rates,exp(betas[[i]][,3]-betas[[i]][,5]))
}

colnames(gain.rates) <- var.names
colnames(mutation.rates) <- var.names
colnames(death.rates) <- var.names

sp <- NULL

for (i in 1:length(var.names)) {
  
  Q <- array(dim=c(4000,3,3))
  Q[,1,2] <- exp(betas[[i]][,1])
  Q[,1,3] <- exp(betas[[i]][,2])
  Q[,2,1] <- exp(betas[[i]][,3])
  Q[,2,3] <- exp(betas[[i]][,4])
  Q[,3,1] <- exp(betas[[i]][,5])
  Q[,3,2] <- exp(betas[[i]][,6])
  
  sp.ratio.i <- apply(Q,1,function(x){diag(x) <- 0; diag(x) <- -rowSums(x); svd(x)$u[3,3]/svd(x)$u[2,3]})
  
  sp <- cbind(sp,sp.ratio.i)
  
}

colnames(sp) <- var.names

gain.rates <- gain.rates[,order(apply(gain.rates,2,median))]
gain.props <- apply(gain.rates,2,function(x){length(which(x>1))/length(x)})

pdf('gain.pdf',width=3,height=8)
mcmc_intervals(gain.rates,prob=.85,prob_outer=.95,outer_size=0.75,inner_size = 1,point_size = 2) + vline_at(1) + 
  #  annotate('text',x=12,y=c(1:ncol(gain.rates)),label=paste(rev(round(gain.props*100,1)),'% > 1',sep=''),size=3,family='serif',hjust = 1)
  annotate('text',x=15,y=c(1:ncol(gain.rates)),label=paste(rev(round(gain.props*100,1)),'% > 1',sep=''),size=3,family='serif',hjust = 1)

dev.off()

mutation.rates <- mutation.rates[,order(apply(mutation.rates,2,median))]
mutation.props <- apply(mutation.rates,2,function(x){length(which(x>1))/length(x)})

pdf('mutation.pdf',width=3,height=8)
mcmc_intervals(mutation.rates,prob=.85,prob_outer=.95,outer_size=0.75,inner_size = 1,point_size = 2) + vline_at(1) + 
  #  annotate('text',x=50,y=c(1:ncol(mutation.rates)),label=paste(rev(round(mutation.props*100,1)),'% > 1',sep=''),size=3,family='serif',hjust = 1)
  annotate('text',x=65,y=c(1:ncol(mutation.rates)),label=paste(rev(round(mutation.props*100,1)),'% > 1',sep=''),size=3,family='serif',hjust = 1)

dev.off()

death.rates <- death.rates[,order(apply(death.rates,2,median))]
death.props <- apply(death.rates,2,function(x){length(which(x>1))/length(x)})

pdf('death.pdf',width=3,height=8)
mcmc_intervals(death.rates,prob=.85,prob_outer=.95,outer_size=0.75,inner_size = 1,point_size = 2) + vline_at(1) + 
  #  annotate('text',x=50,y=c(1:ncol(death.rates)),label=paste(rev(round(death.props*100,1)),'% > 1',sep=''),size=3,family='serif',hjust = 1)
  annotate('text',x=65,y=c(1:ncol(death.rates)),label=paste(rev(round(death.props*100,1)),'% > 1',sep=''),size=3,family='serif',hjust = 1)

dev.off()

sp <- sp[,order(apply(sp,2,median))]
sp.props <- apply(sp,2,function(x){length(which(x>1))/length(x)})

pdf('sp.pdf',width=3,height=8)
mcmc_intervals(sp,prob=.85,prob_outer=.95,outer_size=0.75,inner_size = 1,point_size = 2) + vline_at(1) + 
  #  annotate('text',x=12,y=c(1:ncol(death.rates)),label=paste(rev(round(sp.props*100,1)),'% > 1',sep=''),size=3,family='serif',hjust = 1)
  annotate('text',x=15,y=c(1:ncol(death.rates)),label=paste(rev(round(sp.props*100,1)),'% > 1',sep=''),size=3,family='serif',hjust = 1)

dev.off()