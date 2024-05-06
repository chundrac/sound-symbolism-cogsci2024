require(phytools)
require(phangorn)
require(tidyr)
require(rstan)

n_cores = 16

options(mc.cores = parallel::detectCores())
rstan_options(auto_write=TRUE)

rstan_options(threads_per_chain = 32)

#N.B.: this code assumes that trees involved are binary branching, and that the number of branches = the number of nodes - 1

my.sound <- commandArgs(trailingOnly = TRUE)[1]

my.concept <- commandArgs(trailingOnly = TRUE)[2]
my.concept <- gsub('_',' ',my.concept)

my.seed <- commandArgs(trailingOnly = TRUE)[3]

if (my.seed != 'mcc') {
  my.seed <- as.integer(my.seed)
  set.seed(my.seed)
}

CONCEPTS <- my.concept

trees <- readLines('../metadata/datasets_to_use.txt')

char.data <- list()
fam <- list()
concept <- list()
Bs <- list()
Ns <- list()
parent <- list()
child <- list()
brlen <- list()

i = 1
for (tree in trees) {
  tree.sample <- read.tree(paste('../../lexibank/data/mrbayes_posteriors/',tree,'.posterior.tree',sep=''))
  if (my.seed != 'mcc') {
    sampled.tree <- tree.sample[[sample(1:length(tree.sample),1)]]
  }
  else {
    sampled.tree <- maxCladeCred(tree.sample)
  }
  forms <- read.csv(paste('../../lexibank/lexibank-analysed/raw/',tree,'/cldf/forms_w_asjp.csv',sep=''))
  cognates <- read.csv(paste('../../lexibank/lexibank-analysed/raw/',tree,'/cldf/cognates.csv',sep=''))
  params <- read.csv(paste('../../lexibank/lexibank-analysed/raw/',tree,'/cldf/parameters.csv',sep=''))
  cog.data <- merge(forms,cognates,by.x='ID',by.y='Form_ID')
  cog.data <- merge(cog.data,params,by.x='Parameter_ID',by.y='ID')
  cog.data <- cog.data[,c('Language_ID','Concepticon_Gloss','asjp_segs','Cognateset_ID')]
  cog.data$Cognateset_ID <- paste(cog.data$Concepticon_Gloss,paste(tree,cog.data$Cognateset_ID,sep='_'),sep='|')
  
  if (any(CONCEPTS %in% cog.data$Concepticon_Gloss)) {
    cog.data.sub <- cog.data[cog.data$Concepticon_Gloss %in% CONCEPTS,]
    cog.data.sub <- as.data.frame(aggregate(asjp_segs ~ Language_ID + Cognateset_ID, cog.data.sub, FUN=function(x){paste(x,collapse=' ')}))
    cog.data.wide <- spread(cog.data.sub, key=Cognateset_ID, value=asjp_segs)
    rownames(cog.data.wide) <- cog.data.wide$Language_ID
    cog.data.wide <- cog.data.wide[,2:ncol(cog.data.wide),drop=FALSE]
    coded.data <- apply(cog.data.wide,c(1,2),function(x){if (is.na(x)) {'ABSENT'} else grepl(my.sound,x)})
    coded.data <- apply(coded.data,2,as.factor)
    
    sampled.tree <- keep.tip(sampled.tree,which(sampled.tree$tip.label %in% rownames(coded.data)))
    sampled.tree <- reorder.phylo(sampled.tree,order='pruningwise')
    
    coded.data <- coded.data[sampled.tree$tip.label,,drop=FALSE]
    
    for (j in 1:ncol(coded.data)) {
      if ('FALSE' %in% unique(coded.data[,j]) | 'TRUE' %in% unique(coded.data[,j])) {
        #print(unique(coded.data[,j]))
        bin.data <- to.matrix(coded.data[,j],seq=c('ABSENT','FALSE','TRUE'))
        #print(colSums(bin.data[,2:3]))
        #print(sum(bin.data[,3])/sum(colSums(bin.data[,2:3])))
        bin.data <- rbind(as.matrix(bin.data),matrix(1,nrow=sampled.tree$Nnode,ncol=ncol(bin.data)))
        char.data[[i]] <- bin.data
        
        fam[[i]] <- tree
        parent[[i]] <- sampled.tree$edge[,1]
        child[[i]] <- sampled.tree$edge[,2]
        brlen[[i]] <- sampled.tree$edge.length
        Bs[[i]] <- length(sampled.tree$edge.length)
        Ns[[i]] <- nrow(bin.data)
        concept[[i]] <- strsplit(colnames(coded.data)[j],'\\|')[[1]][1]
        
        i <- i+1
      }
    }
  }
}

J <- unique(unlist(lapply(char.data,ncol)))
N <- max(unlist(lapply(char.data,nrow)))
B <- max(unlist(lapply(brlen,length)))
D <- length(char.data)

brlen.final <- matrix(nrow=D,ncol=B)
parent.final <- matrix(nrow=D,ncol=B)
child.final <- matrix(nrow=D,ncol=B)
char.data.final <- array(dim=c(D,N,J))

for (i in 1:D) { 
  brlen.new <- rep(0,B)
  brlen.new[1:Bs[[i]]] <- brlen[[i]]
  brlen.final[i,] <- brlen.new
  parent.new <- rep(0,B)
  parent.new[1:Bs[[i]]] <- parent[[i]]
  parent.final[i,] <- parent.new
  child.new <- rep(0,B)
  child.new[1:Bs[[i]]] <- child[[i]]
  child.final[i,] <- child.new
  char.data.new <- matrix(0,nrow=N,ncol=J)
  char.data.new[1:Ns[[i]],] <- char.data[[i]]
  char.data.final[i,,] <- char.data.new
}

concept <- as.numeric(as.factor(unlist(concept)))
fam <- as.factor(unlist(fam))
Bs <- unlist(Bs)
Ns <- unlist(Ns)

print(paste(my.sound,CONCEPTS,N,D))

data.list <- list(
  N=N,
  B=B,
  D=D,
  J=J,
  L=max(as.numeric(fam)),
  Bs=Bs,
  K=max(concept),
  concept=concept,
  fam=as.numeric(fam),
  child=child.final,
  parent=parent.final,
  brlen=brlen.final,
  tiplik=char.data.final
)

fit <- stan(file='stan_models/seg_by_concept.stan',data=data.list,cores=n_cores,control = list(adapt_delta=.99))

save.image(gsub(' ','_',paste('fitted_models/individual/concepts_by_seg_',my.sound,'_',my.concept,'_',my.seed,'.Rdata',sep='')))
