contrast.list <- readRDS('contrasts.RDS')

concepts.to.keep <- c('TONGUE',
                      'BREAST',
                      'NOSE',
                      'ROUND',
                      'I',
                      'WE')

birth.df <- contrast.list[[1]]
birth.df <- birth.df[birth.df$birth.c1 %in% concepts.to.keep & birth.df$birth.c2 %in% concepts.to.keep,]

death.df <- contrast.list[[2]]
death.df <- death.df[death.df$death.c1 %in% concepts.to.keep & death.df$death.c2 %in% concepts.to.keep,]

mut.df <- contrast.list[[3]]
mut.df <- mut.df[mut.df$mut.c1 %in% concepts.to.keep & mut.df$mut.c2 %in% concepts.to.keep,]

sp.df <- contrast.list[[4]]
sp.df <- sp.df[sp.df$sp.c1 %in% concepts.to.keep & sp.df$sp.c2 %in% concepts.to.keep,]


consonants <- unique(c(unique(birth.df$cons),unique(death.df$cons),unique(mut.df$cons)))

for (cons in consonants) {
  
  contrasts <- birth.df[birth.df$cons==cons,c(1,2,4)]
  
  colnames(contrasts) <- c('cond1','cond2','prop')
  
  contrasts$cond1 <- as.character(contrasts$cond1)
  contrasts$cond2 <- as.character(contrasts$cond2)
  
  contrasts <- contrasts[contrasts$prop >= .5,]
  
  nodes <- unique(c(contrasts$cond1,contrasts$cond2))
  
  write(paste('\\documentclass{standalone}
\\usepackage[pdf]{graphviz}
\\begin{document}
   \\digraph{birth',cons,'}{
   rankdir="LR"
',sep='')
        ,file=paste('graphics_cogsci/birth',cons,'.tex',sep='')
  )
  
  for (i in 1:nrow(contrasts)) {
    if (contrasts[i,3] > .95) {
      write(paste('"',contrasts[i,1],'"',' ','->',' ','"',contrasts[i,2],'"','[style="solid"];',sep=''),file=paste('graphics_cogsci/birth',cons,'.tex',sep=''),append=T)
    }
    else if (contrasts[i,3] > .85) {
      write(paste('"',contrasts[i,1],'"',' ','->',' ','"',contrasts[i,2],'"','[style="dashed"];',sep=''),file=paste('graphics_cogsci/birth',cons,'.tex',sep=''),append=T)
    }
    else {
      write(paste('"',contrasts[i,1],'"',' ','->',' ','"',contrasts[i,2],'"','[style="dotted"];',sep=''),file=paste('graphics_cogsci/birth',cons,'.tex',sep=''),append=T)
    }
  }
  
  write('   }
\\end{document}',file=paste('graphics_cogsci/birth',cons,'.tex',sep=''),append=T)
  
}


for (cons in consonants) {
  
  contrasts <- death.df[death.df$cons==cons,c(1,2,4)]
  
  colnames(contrasts) <- c('cond1','cond2','prop')
  
  contrasts$cond1 <- as.character(contrasts$cond1)
  contrasts$cond2 <- as.character(contrasts$cond2)
  
  contrasts <- contrasts[contrasts$prop >= .5,]
  
  nodes <- unique(c(contrasts$cond1,contrasts$cond2))
  
  write(paste('\\documentclass{standalone}
\\usepackage[pdf]{graphviz}
\\begin{document}
   \\digraph{death',cons,'}{
   rankdir="LR"
',sep='')
        ,file=paste('graphics_cogsci/death',cons,'.tex',sep='')
  )
  
  for (i in 1:nrow(contrasts)) {
    if (contrasts[i,3] > .95) {
      write(paste('"',contrasts[i,1],'"',' ','->',' ','"',contrasts[i,2],'"','[style="solid"];',sep=''),file=paste('graphics_cogsci/death',cons,'.tex',sep=''),append=T)
    }
    else if (contrasts[i,3] > .85) {
      write(paste('"',contrasts[i,1],'"',' ','->',' ','"',contrasts[i,2],'"','[style="dashed"];',sep=''),file=paste('graphics_cogsci/death',cons,'.tex',sep=''),append=T)
    }
    else {
      write(paste('"',contrasts[i,1],'"',' ','->',' ','"',contrasts[i,2],'"','[style="dotted"];',sep=''),file=paste('graphics_cogsci/death',cons,'.tex',sep=''),append=T)
    }
  }
  
  write('   }
\\end{document}',file=paste('graphics_cogsci/death',cons,'.tex',sep=''),append=T)
  
}


for (cons in consonants) {
  
  contrasts <- mut.df[mut.df$cons==cons,c(1,2,4)]
  
  colnames(contrasts) <- c('cond1','cond2','prop')
  
  contrasts$cond1 <- as.character(contrasts$cond1)
  contrasts$cond2 <- as.character(contrasts$cond2)
  
  contrasts <- contrasts[contrasts$prop >= .5,]
  
  nodes <- unique(c(contrasts$cond1,contrasts$cond2))
  
  write(paste('\\documentclass{standalone}
\\usepackage[pdf]{graphviz}
\\begin{document}
   \\digraph{mut',cons,'}{
   rankdir="LR"
',sep='')
        ,file=paste('graphics_cogsci/mut',cons,'.tex',sep='')
  )
  
  for (i in 1:nrow(contrasts)) {
    if (contrasts[i,3] > .95) {
      write(paste('"',contrasts[i,1],'"',' ','->',' ','"',contrasts[i,2],'"','[style="solid"];',sep=''),file=paste('graphics_cogsci/mut',cons,'.tex',sep=''),append=T)
    }
    else if (contrasts[i,3] > .85) {
      write(paste('"',contrasts[i,1],'"',' ','->',' ','"',contrasts[i,2],'"','[style="dashed"];',sep=''),file=paste('graphics_cogsci/mut',cons,'.tex',sep=''),append=T)
    }
    else {
      write(paste('"',contrasts[i,1],'"',' ','->',' ','"',contrasts[i,2],'"','[style="dotted"];',sep=''),file=paste('graphics_cogsci/mut',cons,'.tex',sep=''),append=T)
    }
  }
  
  write('   }
\\end{document}',file=paste('graphics_cogsci/mut',cons,'.tex',sep=''),append=T)
  
}


for (cons in consonants) {
  
  contrasts <- sp.df[sp.df$cons==cons,c(1,2,4)]
  
  colnames(contrasts) <- c('cond1','cond2','prop')
  
  contrasts$cond1 <- as.character(contrasts$cond1)
  contrasts$cond2 <- as.character(contrasts$cond2)
  
  contrasts <- contrasts[contrasts$prop >= .5,]
  
  nodes <- unique(c(contrasts$cond1,contrasts$cond2))
  
  write(paste('\\documentclass{standalone}
\\usepackage[pdf]{graphviz}
\\begin{document}
   \\digraph{sp',cons,'}{
   rankdir="LR"
',sep='')
        ,file=paste('graphics_cogsci/sp',cons,'.tex',sep='')
  )
  
  for (i in 1:nrow(contrasts)) {
    if (contrasts[i,3] > .95) {
      write(paste('"',contrasts[i,1],'"',' ','->',' ','"',contrasts[i,2],'"','[style="solid"];',sep=''),file=paste('graphics_cogsci/sp',cons,'.tex',sep=''),append=T)
    }
    else if (contrasts[i,3] > .85) {
      write(paste('"',contrasts[i,1],'"',' ','->',' ','"',contrasts[i,2],'"','[style="dashed"];',sep=''),file=paste('graphics_cogsci/sp',cons,'.tex',sep=''),append=T)
    }
    else {
      write(paste('"',contrasts[i,1],'"',' ','->',' ','"',contrasts[i,2],'"','[style="dotted"];',sep=''),file=paste('graphics_cogsci/sp',cons,'.tex',sep=''),append=T)
    }
  }
  
  write('   }
\\end{document}',file=paste('graphics_cogsci/sp',cons,'.tex',sep=''),append=T)
  
}

