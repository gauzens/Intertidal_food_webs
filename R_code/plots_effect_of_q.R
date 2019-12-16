rm(list = ls())
library(ggplot2)
library(gridExtra)
library(viridis)
library("grid")

do.plot = function(df, title){
  experimentals = ggplot(df, 
                         aes(x=temperature, y=persistence, group = as.factor(mean.temp), fill = as.factor(mean.temp), colour = as.factor(mean.temp)), 
  )+
    stat_summary(geom="point", fun.y=mean, cex = 0.8)+
    geom_smooth( alpha=0.2, cex = 0.3) +
    scale_fill_viridis_d(option = "plasma", name = paste('Average \nsummer temp.'))+
    scale_color_viridis_d(option = "plasma", name = paste('Average \nsummer temp.'))+
    theme_classic()+
    ggtitle(title)+
    theme(
      plot.title = element_text(size = 14, hjust = 0),
      # axis.title.x=element_blank(),
      # axis.text.x=element_blank(),
      axis.text.y = element_text(size = 8),
      axis.title.y = element_text(size = 10),
      legend.position = 'bottom',
      plot.margin = unit(c(1.5,0.1,0.1,1), "cm"),
    )
  return(experimentals)
}

prepare_tab = function(tab.exp){
  
  names(tab.exp) = c('name', 'region', 'init_temp', 'richness', 'temperature', 'nb_ext_nn_basal', 'nb_ext', 'resilience', 'oi', 'tl', 'connectance')
  tab.exp$region = sub("u'", "", tab.exp$region)
  tab.exp$region = sub("'", "", tab.exp$region)
  tab.exp$region = sub(" ", "", tab.exp$region)
  tab.exp$prop_ext = 1 - tab.exp$nb_ext/tab.exp$richness
  tab.exp$mean.temp = round(ave(tab.exp$init_temp, tab.exp$region, FUN = mean, na.rm = TRUE), 1)
  seetemps = read.table('~/projects/WarmingWebs/R_code2/see_temps', header = T, sep = ',')
  
  tab.exp$mean.summer = NA
  for (name in seetemps$name){
    cat(name, '\n')
    xx = grep(pattern = name, x = tab.exp$name, fixed = TRUE)
    tab.exp$mean.summer[grep(pattern = name, x = tab.exp$name, fixed = TRUE)] = round(seetemps$MeanSummer[seetemps$name == name],1)
  }
  
  temp_incr = tab.exp$temperature - tab.exp$init_temp
  temps = unique(tab.exp$init_temp)
  warming = unique(tab.exp$temp_incr)
  netws = unique(tab.exp$name)
  
  
  max.min = c(NA, NA, NA)
  for  (nom in unique(tab.exp$name)){
    aaa = tab.exp[tab.exp$name == nom, ]
    aaa = aaa[aaa$temperature <=50, ]
    res = c(unique(aaa$init_temp), unique(aaa$mean.temp), max(aaa$prop_ext) - min(aaa$prop_ext))
    max.min = rbind(max.min, res)
  }
  
  
  # cbPalette <- c('blue4', 'dodgerblue3', 'lightskyblue', 'lightseagreen', 'gold', 'darkorange2', 'red')
  df = data.frame(persistence = tab.exp$prop_ext, temperature = tab.exp$temperature, init_temp = tab.exp$mean.summer, mean.temp = tab.exp$mean.summer)
  df = df[df$temperature <=50, ]
  
  return(df)
}

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}


# tab.exp = read.csv('/homes/bg33novu/projects/WarmingWebs/WarmingHPC/outputs/experimentalfinals.csv', header = F)
tab.exp = read.csv('/home/bg33novu/projects/WarmingWebs/WarmingHPC/outputs/experimentals_K5.csv', header = F)
tab.exp1.2 =  read.csv('/home/bg33novu/projects/WarmingWebs/WarmingHPC/outputs/experimentals_K5_h1.2.csv', header = F)
tab.exp1.8 =  read.csv('/home/bg33novu/projects/WarmingWebs/WarmingHPC/outputs/experimentals_K5_h1.8.csv', header = F)

df1.2 = prepare_tab(tab.exp1.2)
df1.8 = prepare_tab(tab.exp1.8)
df1.5 = prepare_tab(tab.exp)

p1.2 = do.plot(df1.2, 'q = 1.2')
p1.8 = do.plot(df1.8, 'q = 1.8')
p1.5 = do.plot(df1.5, 'q = 1.5')

legend = g_legend(p1.2)
p1.2 = p1.2 + theme(legend.position="none")
p1.8 = p1.8 + theme(legend.position="none")
p1.5 = p1.5 + theme(legend.position="none")

pdf('~/projects/WarmingWebs/paper/figures/effect_of_q.pdf', width = 15)
grid.arrange(p1.2, p1.5, p1.8, legend, layout_matrix = rbind(c(1,2,3), c(4)), heights = c(5,1))
dev.off()

