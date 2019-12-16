rm(list = ls())
library(ggplot2)
library(gridExtra)
library(viridis)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}


col_pal = viridis(7, option = 'plasma')
plot(c(1:7) ~ 1, col = col_pal, pch = 16, cex = 2)
load('~/projects/WarmingWebs/R_code2/acp_arrows.Rdata')
load('~/projects/WarmingWebs/R_code2/acp_elipsesRdata')
load('~/projects/WarmingWebs/R_code2/TL_BM.Rdata')
load('/home/bg33novu/projects/WarmingWebs/R_code2/legend.Rdata')


# plot(g.mean.see)
# plot(g1)
# plot(TL_BM)

g.mean.see = g.mean.see + 
  theme(    
    axis.text.x = element_text(size = 20),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size=32)
)

g1 = g1 + 
  theme(    
    axis.text.x = element_text(size = 20),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size=32)
  )

TL_BM = TL_BM + 
  xlab('Trophic level')+
  ylab('Species body masses (log10(g))')+
  theme(
    axis.text.x = element_text(size = 20),
    axis.title.x = element_text(size = 30),
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 30),
    plot.title = element_text(size=32)
  )

legend = legend +
  theme(
    legend.title = element_text(size = 50),
    legend.text = element_text(size = 30)
  )

legend = g_legend(g.legend)

pdf('~/projects/WarmingWebs/paper/figures/fig2_viridis.pdf', width = 30, height = 10)

grid.arrange(g1, g.mean.see, TL_BM, legend, ncol = 4, widths = c(10,10,10,3))

dev.off()

