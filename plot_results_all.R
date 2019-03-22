# With DL, uncomment next line
res = read.csv('models_final_dl/loss_summary.csv')
pdf('figures_dl/Supplementary_Shuffled_Results.pdf')

# No DL:
# res = read.csv('models_final/loss_summary.csv')
# pdf('figures/Overall_Results.pdf',width=(18.9/2.4))


DO_DL = TRUE

models = c('dl_l','rf','ridge','svr')
model_names=c('Deep Learning','Random Forests','Ridge Regression','Support Vector Machines')


library(RColorBrewer)
cols = brewer.pal(n = 4, name = 'Spectral')

frac_fits = as.character(c(1,0.9,0.8,0.6,0.4))
losses = c('mse','min','avg')
## Just going to plot with gamma == 0.99
gammas = as.character(c(90,95,99))
names(cols) = models
ys = array(dim = length(frac_fits))
names(ys) = frac_fits
up_ys = c(18,16,14,25); names(up_ys)=gammas
line_types = c(1,2)

par(mfrow=c(2,2),bty='n',las=1)
for(g in gammas){
  plot(NA,NA,xlim=c(0.4,1), ylim=c(0,25),#up_ys[g]), 
       main=paste('Active-rank on top ',g, '%', sep=''),
       xlab='Training sample size relative to full data (%)',ylab='Total model score',xaxt='n')
  axis(1,at=c(0.4,.6,.8,1), labels = 100*c(0.4,.6,.8,1))
  for (m in models){
    if(DO_DL | (!DO_DL & m != 'dl_l')){
      c = 1
      for(l in losses){
        if(l != 'mse'){
          for(f in frac_fits){
            ind = intersect(intersect(grep(l, res$losses), grep(g, res$losses)), grep(f, res$losses))
            ys[f] = res[ind,m]
          }
          lines(frac_fits, ys, lty=line_types[c], col=cols[m],lwd=2)
          c = c+1
        }
        
      }
    }
  }
}

l='mse'
plot(NA,NA,xlim=c(0.4,1), ylim=c(0,25), main='Mean Squared Error',
     xlab='Training sample size relative to full data (%)',ylab='Total model score',xaxt='n')
axis(1,at=c(0.4,.6,.8,1), labels = 100*c(0.4,.6,.8,1))
for (m in models){
  if(DO_DL | (!DO_DL & m != 'dl_l')){
    for(f in frac_fits){
      ind = intersect(grep(l, res$losses), grep(f, res$losses))
      ys[f] = res[ind,m]
    }
    lines((frac_fits), ys, lty=4, col=cols[m],lwd=2)
  }
}
legend('left',col=cols,legend = model_names,lwd=2,lty=4,
      cex=0.8,bty='n')

dev.off()

