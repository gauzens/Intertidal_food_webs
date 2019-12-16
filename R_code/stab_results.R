rm(list = ls())
library(nlme)
detach(tab)
library(ggplot2)
library(measurements)
library(RColorBrewer)
library(viridis)
library("ggsci")
library(car)

error.bars<-function(x,y,xbar,ybar, coul)
{arrows(x,y-ybar,x,y+ybar,code=3,angle=90,length=0.05, col=coul)
  #arrows(x-xbar,y,x+xbar,y,code=3,angle=90,length=0.05, col=coul)
}

mmToInches = function(x){
  return(x/25.4)
}

tab = read.csv('/home/bg33novu/projects/WarmingWebs/WarmingHPC/outputs/warming_exp_k5.csv', header = FALSE)
names(tab) = c('name', 'region', 'repl', 'depth', 'area', 'tempsee', 'init_temp', 'richness', 'warming', 'nb_ext_nn_basal', 'nb_ext', 'resilience', 'oi', 'tl', 'connectance')
tab$prop_ext = 1 - tab$nb_ext/tab$richness
tab$mean.temp = round(ave(tab$init_temp, tab$region, FUN = mean), 1)
radius2 = tab$area / pi
tab$size = (1/6) * pi * tab$depth *(3*radius2 + tab$depth*tab$depth)

latitude = tab$tempsee
latitude[grepl('Portugal_txt', tab$name)] = '38 42 38'
latitude[grepl('Canada_txt/PP', tab$name)] = '48 29 33'
latitude[grepl('Canada_txt/SF', tab$name)] = '48 36 43'
latitude[grepl('England_txt/MB', tab$name)] = '50 21 24'
latitude[grepl('England_txt/W', tab$name)] = '50 19 00'
latitude[grepl('Portugal_txt/CR', tab$name)] = '38 42 38'
latitude[grepl('Portugal_txt/RV', tab$name)] = '39 17 11'
latitude[grepl('Mad_txt/PC', tab$name)] = '32 46 32'
latitude[grepl('Mad_txt/RM', tab$name)] = '32 38 44'
latitude[grepl('Mad_txt/RM', tab$name)] = '32 38 44'
latitude[grepl('Brasil\\(SP\\)_txt/', tab$name)] = '-23 35 00'
latitude[grepl('Brasil\\(CE\\)_txt/FX', tab$name)] = '3 13 04'
latitude[grepl('Brasil\\(CE\\)_txt/GJ', tab$name)] = '3 14 14'
latitude[grepl('Moz_txt', tab$name)] = '-25 58 36'

unique(cbind.data.frame(tab$name, latitude))

latitude2 = as.numeric(conv_unit(latitude, "deg_min_sec", "dec_deg"))

latitude2[grepl('Portugal_txt/L1', tab$name)] = 39.1508
latitude2[grepl('Portugal_txt/L2', tab$name)] = 39.1508
latitude2[grepl('Portugal_txt/L3', tab$name)] = 39.245223
latitude2[grepl('Portugal_txt/L4', tab$name)] = 39.245223

tab$latitude = latitude2
latitude3 = ave(latitude2, tab$region, FUN = mean)
tab$latitudem = latitude3
rm(latitude)
attach(tab)
inits = unique(init_temp)
warm = unique(warming)
netws = unique(name)



#### reading info about topology and environment######

data = read.csv('/home/bg33novu/projects/WarmingWebs/results/network_topologies.csv', header = T)
names(data) = c("name", "temp_sea", "temp", "area", "detph" ,"elevation", "Nb species", "Nb links", "Connectance", "Mean omnivory", 
                "PredPreyRatio", "Mean generalism", "% basal", "%intermediate", "%top", "Mean TL", "Mean TL top species", "Avg path length" )
library(ggplot2)
head(data)
# size = data$area*data$detph
# considering pools as spherical caps:
radius2 = data$area / pi
size = (1/6) * pi * data$detph *(3*radius2 + data$detph*data$detph)

data$min.sea.temp = NA
data$max.sea.temp = NA
data$mean.sea.temp = NA
data$mean.summer = NA
data$min.summer = NA
data$max.summer = NA

#########################

seetemps = read.table('~/projects/WarmingWebs/R_code2/see_temps', header = T, sep = ',')
for (name in seetemps$name){
  cat(name, '\n')
  xx = grep(pattern = name, x = data$name, fixed = TRUE)
  data$min.sea.temp[xx] = seetemps$Min[seetemps$name == name]
  data$max.sea.temp[grep(pattern = name, x = data$name, fixed = TRUE)] = seetemps$Max[seetemps$name == name]
  data$mean.sea.temp[grep(pattern = name, x = data$name, fixed = TRUE)] = seetemps$Mean[seetemps$name == name]
  data$mean.summer[grep(pattern = name, x = data$name, fixed = TRUE)] = seetemps$MeanSummer[seetemps$name == name]
  data$min.summer[grep(pattern = name, x = data$name, fixed = TRUE)] = seetemps$MaxSummer[seetemps$name == name]
  data$max.summer[grep(pattern = name, x = data$name, fixed = TRUE)] = seetemps$MinSummer[seetemps$name == name]
}

data$amplitude = data$max.sea.temp - data$min.sea.temp


#### doing correspondences between the two dataframes ########
tab$name = gsub('/web2.txt', '', tab$name)
tab$name = gsub("\\[u'", '', tab$name)
tab$name = gsub("'", '', tab$name)
data$name[1]
which(tab$name == data$name[1])
tab$name[which(tab$name == data$name[1])]
tab$name[1]
names(tab)
tab = merge(tab, data[,c(1,6,25)])
detach(tab)
attach(tab)
# length(names(data))
#####################################################


#####################################################
##################33 starting effects: ##############
#####################################################

tab.init = tab.init = tab[tab$warming == 0, ]
plot(tab.init$prop_ext ~ tab.init$init_temp, ylab = "Persistence", xlab = "Initial web temperature")
lines(tapply(tab.init$prop_ext, tab.init$init_temp, mean) ~ sort(inits))
tapply(tab.init$prop_ext, tab.init$mean.temp, mean)

plot(tab.init$prop_ext ~ tab.init$tempsee, xlab = "Persistence", ylab = "local sea temperature")
points(as.numeric(names(tapply(tab.init$prop_ext, tab.init$tempsee, mean))), tapply(tab.init$prop_ext, tab.init$tempsee, mean), col = 'red', pch = 16)

boxplot(tab.init$prop_ext ~ tab.init$tempsee, new = TRUE)
pdf('/homes/bg33novu/projects/WarmingWebs/paper/figures/init_persistence.pdf')
df.init = data.frame(Persistence = tab.init$prop_ext, init_temp = tab.init$tempsee, region = tab.init$region)
ggplot(df.init, aes(group = init_temp, y = Persistence, x = init_temp)) +
  geom_boxplot()+
  geom_point()+
  xlab("Average summer temperature")
dev.off()
see2 = (df.init$init_temp)^2
df.init$see2 = see2
summary(lm(df.init$Persistence ~ df.init$init_temp))
summary(lm(df.init$Persistence ~ see2))
summary(lme(Persistence ~ see2, random = ~1|region, data = df.init))

# ------------------------------------------------------------------------------------------------ #



######### plot prop of extinctions #########


df = data.frame(Persistence = tab$prop_ext, Warming = tab$warming, init_temp = tab$tempsee, mean.temp = tab$tempsee)

pdf('/home/bg33novu/projects/WarmingWebs/paper/figures/Fig.3NCC_color_blind.pdf', width = mmToInches(82), height = mmToInches(62))

ggplot(df, 
       aes(x=Warming, y=Persistence, group = as.factor(mean.temp), fill = as.factor(mean.temp), colour = as.factor(mean.temp)), 
       )+
  stat_summary(geom="point", fun.y=mean, cex = 0.4)+
  geom_smooth(method = 'lm', alpha = 0.5, cex = 0.2)+
  # scale_color_manual(values=cbPalette, name = paste('Average \nsummer temp.'))+
  # scale_fill_manual(values=cbPalette, name = paste('Average \nsummer temp.'))+
  # guides(fill=guide_legend(ncol=2))+
  # guides(color=guide_legend(ncol=2))+
  scale_fill_viridis_d(option = "plasma", name = paste('Average \nsummer \nsea temp.:'))+
  scale_color_viridis_d(option = "plasma", name = paste('Average \nsummer \nsea temp.:'))+
  theme_classic()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 10),
    plot.margin = unit(c(1.1,0.1,0.1,0.8), "cm"),
    legend.key.size=unit(0.5,"cm")
    # legend.title = '',
    # legend.text = 
    # legend.position = 'bottom'
  )

dev.off()

ggplot(df, 
       aes(x=Warming, y=Persistence, group = init_temp, colour = init_temp), 
)+
  stat_summary(geom="point", fun.y=mean, cex = 0.4)+
  geom_smooth(method = 'lm', alpha = 0.5, cex = 0.2)+
  # scale_color_manual(values=cbPalette, name = paste('Local \ntemp.'))+
  # scale_fill_manual(values=cbPalette, name = paste('Local \ntemp.'))+
  # guides(fill=guide_legend(ncol=2))+
  # guides(color=guide_legend(ncol=2))+
  theme_classic()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 10),
    plot.margin = unit(c(1.1,0.1,0.1,0.8), "cm"),
    legend.key.size=unit(0.5,"cm")
    # legend.title = '',
    # legend.text = 
    # legend.position = 'bottom'
  )




dev.off()

# detach(tab)
# ggsave('~/projects/WarmingWebs/plots/short_gradientggplot.pdf')

########################## plot using average pool temperature #######################

df = data.frame(Persistence = tab$prop_ext, Warming = tab$warming, mean.temp = tab$mean.temp)

pdf('/home/bg33novu/projects/WarmingWebs/paper/figures/Fig.3NCC_color_blind.pdf', width = mmToInches(82), height = mmToInches(62))

ggplot(df, 
       aes(x=Warming, y=Persistence, group = as.factor(mean.temp), fill = as.factor(mean.temp), colour = as.factor(mean.temp)), 
)+
  stat_summary(geom="point", fun.y=mean, cex = 0.4)+
  geom_smooth(method = 'lm', alpha = 0.5, cex = 0.2)+
  # scale_color_manual(values=cbPalette, name = paste('Average \nsummer temp.'))+
  # scale_fill_manual(values=cbPalette, name = paste('Average \nsummer temp.'))+
  # guides(fill=guide_legend(ncol=2))+
  # guides(color=guide_legend(ncol=2))+
  scale_fill_viridis_d(option = "plasma", name = paste('Average \npool temp.:'))+
  scale_color_viridis_d(option = "plasma", name = paste('Average \npool temp.:'))+
  theme_classic()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 10),
    plot.margin = unit(c(1.1,0.1,0.1,0.8), "cm"),
    legend.key.size=unit(0.5,"cm")
    # legend.title = '',
    # legend.text = 
    # legend.position = 'bottom'
  )

dev.off()





# make logit transformation as between 0 and 1
library(car)
library(MASS)
library(nlme)
library(tidyr)
logit_prop = logit(prop_ext)
tab$logit_prop = logit_prop




#####################################################
##### initial nework structures #####################
#####################################################

tab.init = tab.init = tab[tab$temp_incr == 0, ]
TLs = tapply(tab.init$TL, sort(tab.init$temp), mean, na.rm = TRUE)
ois = tapply(tab.init$oi, sort(tab.init$temp), mean, na.rm = TRUE)
Cs = tapply(tab.init$C, sort(tab.init$temp), mean)
Ls = nb_s * nb_s * Cs 

plot(slopes ~ sort(temps), col = colors)
plot(slopes ~ nb_s, col = colors, xlab = "richness")
plot(TLs ~ sort(temps), col = colors, ylab = "trophic levels",xlab = "init temp")
plot(slopes ~ TLs, col = colors, ylab = "slopes",xlab = "init TL")
plot(slopes ~ ois, col = colors, ylab = "slopes",xlab = "init oi")
plot(slopes ~ Cs, col = colors, ylab = "slopes",xlab = "connectance")
plot(slopes ~ Ls, col = colors, ylab = "slopes",xlab = "Number of links")
plot(ois ~ sort(temps))



model = lm(slopes ~ TLs*Cs + ois)
step.AIC = stepAIC(model)
anova(lm(slopes ~ TLs*ois*Cs))
model.best = lm(slopes ~ TLs + ois) 
anova(model.best)


model0 = lm(prop_ext ~ temp*temp_incr)
model1 = lm(prop_ext ~ as.factor(temp)*temp_incr)
model2 = glm(prop_ext ~ temp*temp_incr, family = quasi)
model3 = lme(prop_ext ~ temp*temp_incr, random = ~ 1 |name, data = tab)



######### plot total number of extinctions, per FW #########
# ylimit = c(min(nb_ext), max(nb_ext))
ylimit = c(0,2)
plot(nb_ext ~ temp_incr, ylim = ylimit, col = 'white')

plot(nb_ext ~ temp_incr, col = 'white')
# lines(tapply(nb_ext, temp_incr, mean) ~ warming, ylim = ylimit)
i = 1
for (netw in netws){
  # cat(netw)
  # cat('\n')
  col = i
  data = tab[name == netw,]
  yaxis = tapply(data$nb_ext, data$temp_incr, mean)
  plot(yaxis ~ warming, col = col)
  col = netw
  i = i+1
}


#############################################################################
################### Statistical models ####################################
#############################################################################


logit.prop_ext = logit(tab$prop_ext)

# first define a variable for beach:
tab$beach = gsub("u'", "", tab$repl)
tab$beach = gsub("'", "", tab$beach)
unique(tab$beach)
unique(cbind.data.frame(tab$region, tab$beach))
tab$beach = gsub("\\d+$", "", tab$beach)
# unique(tab$beach)
tab$beach[grep("PP+", tab$beach)] = "PP"
tab$beach[grep("SF+", tab$beach)] = "SF"
# unique(tab$beach)
# unique(cbind.data.frame(tab$region, tab$beach))
# tab$beach[grepl('FX', tab$beach)]

random_model = ~ warming |region/beach/name   # need to change the control here
random_model = ~ 1 | region/beach/name
random_model = ~ 1 | name

model.elev = lme(logit.prop_ext ~ amplitude + elevation + size + abs(latitude) + tempsee*warming, random = random_model, data = tab)
model.elev = lme(logit.prop_ext ~ amplitude + elevation + size + abs(latitude) + tempsee*warming, random = random_model, data = tab)

B.c = BIC(model.elev)
# stepAIC(model.elev)

# minus one variable
model.s = lme(logit.prop_ext ~ amplitude + elevation + abs(latitude) + tempsee*warming, random = random_model, data = tab)
B.s = BIC(model.s)
model.l = lme(logit.prop_ext ~ amplitude + elevation + size + tempsee*warming, random = random_model, data = tab)
B.l = BIC(model.l)
model.a = lme(logit.prop_ext ~ elevation + size + abs(latitude) + tempsee*warming, random = random_model, data = tab)
B.a = BIC(model.a)

# minus 2
model.ae = lme(logit.prop_ext ~ size + abs(latitude) + tempsee*warming, random = random_model, data = tab)
B.ae = BIC(model.ae)
model.as = lme(logit.prop_ext ~ elevation + abs(latitude) + tempsee*warming, random = random_model, data = tab)
B.as = BIC(model.as)
model.al = lme(logit.prop_ext ~ elevation + size + tempsee*warming, random = random_model, data = tab)
B.al = BIC(model.al)
model.es = lme(logit.prop_ext ~ amplitude + abs(latitude) + tempsee*warming, random = random_model, data = tab)
B.es = BIC(model.es)
model.el = lme(logit.prop_ext ~ amplitude + size + tempsee*warming, random = random_model, data = tab)
B.el = BIC(model.el)
model.sl = lme(logit.prop_ext ~ amplitude + elevation + tempsee*warming, random = random_model, data = tab)
B.sl = BIC(model.sl)

# minus3
model.aes = lme(logit.prop_ext ~ abs(latitude) + tempsee*warming, random = random_model, data = tab)  #  <===========
B.aes = BIC(model.aes)
model.ael = lme(logit.prop_ext ~ size + tempsee*warming, random = random_model, data = tab)
B.ael = BIC(model.ael)
model.esl = lme(logit.prop_ext ~ amplitude + tempsee*warming, random = random_model, data = tab)
B.esl = BIC(model.esl)

#minus4
model.aesl = lme(logit.prop_ext ~ tempsee*warming, random = random_model, data = tab)
B.aesl = BIC(model.aesl)

BIC(lme(logit.prop_ext ~ abs(latitude)*tempsee*warming, random = random_model, data = tab))

# without interaction: 
model.add = lme(logit.prop_ext ~ tempsee+warming, random = random_model, data = tab)
B.add = BIC(model.add)
model.w = lme(logit.prop_ext ~ warming, random = random_model, data = tab)
B.w = BIC(model.w)

xx = rbind(c('B.c', 'B.s', 'B.l', 'B.a', 'B.ae', 'B.as', 'B.al', 'B.es', 'B.el', 'B.sl', 'B.aes', 'B.ael', 'B.esl', 'B.aesl'),
      c(B.c, B.s, B.l, B.a, B.ae, B.as, B.al, B.es, B.el, B.sl, B.aes, B.ael, B.esl, B.aesl))
xx[2, ] = round(as.numeric(xx[2, ]), 2)
# stepAIC(model.elev)

anova(model.aesl)
summary(model.aesl)
BIC(model.elev)
AIC(model.aesl)



#########################################################################################
################ make linear models for each of the individual locations ################
#########################################################################################

output = function(){
  stats = function(){
    # print(as.character(unique(subtab$region)))
    if (min(subtab$prop_ext) < 1){
      x = lme(logit_prop ~ warming, random = random_model, data = subtab)
      return(c(anova(x)$`F-value`[2], anova(x)$`p-value`[2], summary(x)$tTable[2,1]))
    }else{
      return(c(NA, NA))
    }
  }
  
  i = 0
  cat('region \t mean_temp \t Fvalue \t pvalue \t coeff\n')
  for (temp in sort(unique(tempsee))){
    i = i + 1 
    subtab = tab[tab$tempsee == temp,]
    subtab$logit_prop = logit(prop_ext[tab$tempsee == temp])
    # cat(unique(subtab$mean.temp), '\t')
    res = tryCatch(stats(), error = function(e)
      return(NA)
    )
    cat(substr(as.character(unique(subtab$region)), 3, 12), '\t', unique(subtab$tempsee), '\t', res[1], '\t', res[2], '\t', res[3], '\n' )
  }
}

library(stringr)
random_model = ~ 1 | beach/name
results.stats = as.data.frame(capture.output(output()), col.names = F)
results.stats = results.stats %>% separate('capture.output(output())', 
                                           c('region', 'mean_temp', 'Fvalue', 'pvalue', 'coeff'), 
                                           sep = '\t')

results.stats = results.stats[-1,]
results.stats
plot(results.stats$coeff ~ results.stats$mean_temp, xlab = "Local see temperature", ylab = "slope of the persistence to warming regression")
summary(lm(results.stats$coeff ~ as.numeric(results.stats$mean_temp)))

# check how intercepts depends on initial temperature
plot(lm(results.stats$coeff ~ as.numeric(results.stats$mean_temp)))
m = list()
output()

# test if even moz has a significnt increase
test = tab[tab$mean.temp == 23.8, ]
model.test = lme(logit_prop ~ warming, random = ~1|name, data = test)
summary(model.test)
anova(model.test)



####################################################
######3 model based on pool temperature (for SI) ###
####################################################

random_model = ~ warming |region/beach/name   # need to change the control here
random_model = ~ 1 | region/beach/name
random_model = ~ 1 | name

model.elev = lme(logit.prop_ext ~ amplitude + elevation + size + abs(latitude) + mean.temp*warming, random = random_model, data = tab)
model.elev = lme(logit.prop_ext ~ amplitude + elevation + size + abs(latitude) + mean.temp*warming, random = random_model, data = tab)

B.c = BIC(model.elev)
# stepAIC(model.elev)

# minus one variable
model.s = lme(logit.prop_ext ~ amplitude + elevation + abs(latitude) + mean.temp*warming, random = random_model, data = tab)
B.s = BIC(model.s)
model.l = lme(logit.prop_ext ~ amplitude + elevation + size + mean.temp*warming, random = random_model, data = tab)
B.l = BIC(model.l)
model.a = lme(logit.prop_ext ~ elevation + size + abs(latitude) + mean.temp*warming, random = random_model, data = tab)
B.a = BIC(model.a)

# minus 2
model.ae = lme(logit.prop_ext ~ size + abs(latitude) + mean.temp*warming, random = random_model, data = tab)
B.ae = BIC(model.ae)
model.as = lme(logit.prop_ext ~ elevation + abs(latitude) + mean.temp*warming, random = random_model, data = tab)
B.as = BIC(model.as)
model.al = lme(logit.prop_ext ~ elevation + size + mean.temp*warming, random = random_model, data = tab)
B.al = BIC(model.al)
model.es = lme(logit.prop_ext ~ amplitude + abs(latitude) + mean.temp*warming, random = random_model, data = tab)
B.es = BIC(model.es)
model.el = lme(logit.prop_ext ~ amplitude + size + mean.temp*warming, random = random_model, data = tab)
B.el = BIC(model.el)
model.sl = lme(logit.prop_ext ~ amplitude + elevation + mean.temp*warming, random = random_model, data = tab)
B.sl = BIC(model.sl)

# minus3
model.aes = lme(logit.prop_ext ~ abs(latitude) + mean.temp*warming, random = random_model, data = tab)  #  <===========
B.aes = BIC(model.aes)
model.ael = lme(logit.prop_ext ~ size + mean.temp*warming, random = random_model, data = tab)
B.ael = BIC(model.ael)
model.esl = lme(logit.prop_ext ~ amplitude + mean.temp*warming, random = random_model, data = tab)
B.esl = BIC(model.esl)

#minus4
model.aesl = lme(logit.prop_ext ~ mean.temp*warming, random = random_model, data = tab)
B.aesl = BIC(model.aesl)

BIC(lme(logit.prop_ext ~ abs(latitude)*mean.temp*warming, random = random_model, data = tab))

# without interaction: 
model.add = lme(logit.prop_ext ~ mean.temp+warming, random = random_model, data = tab)
B.add = BIC(model.add)
model.w = lme(logit.prop_ext ~ warming, random = random_model, data = tab)
B.w = BIC(model.w)

xx = rbind(c('B.c', 'B.s', 'B.l', 'B.a', 'B.ae', 'B.as', 'B.al', 'B.es', 'B.el', 'B.sl', 'B.aes', 'B.ael', 'B.esl', 'B.aesl'),
           c(B.c, B.s, B.l, B.a, B.ae, B.as, B.al, B.es, B.el, B.sl, B.aes, B.ael, B.esl, B.aesl))
xx[2, ] = round(as.numeric(xx[2, ]), 2)
xx

summary(model.aesl)
anova(model.aesl)
# stepAIC(model.elev)





df = data.frame(Persistence = tab$prop_ext, Warming = tab$warming, init_temp = tab$tempsee, mean.temp = tab$latitudem)

ggplot(df, 
       aes(x=Warming, y=Persistence, group = as.factor(mean.temp), fill = as.factor(init_temp), colour = as.factor(init_temp)), 
)+
  stat_summary(geom="point", fun.y=mean, cex = 0.4)+
  geom_smooth(method = 'lm', alpha = 0.3, cex = 0.2)+
  scale_color_manual(values=cbPalette, name = paste('Average \nsummer temp.'))+
  scale_fill_manual(values=cbPalette, name = paste('Average \nsummer temp.'))+
  # guides(fill=guide_legend(ncol=2))+
  # guides(color=guide_legend(ncol=2))+
  theme_classic()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 10),
    plot.margin = unit(c(1.1,0.1,0.1,0.8), "cm"),
    legend.key.size=unit(0.5,"cm")
    # legend.title = '',
    # legend.text = 
    # legend.position = 'bottom'
  )
df = data.frame(Persistence = tab$prop_ext, Warming = tab$warming, init_temp = as.factor(tab$tempsee), mean.temp = tab$latitudem)
ggplot(df, 
       aes(x=Warming, y=Persistence, group = init_temp, fill = init_temp, colour = init_temp), 
)+
  stat_summary(geom="point", fun.y=mean, cex = 0.5)+
  geom_smooth(method = 'lm', alpha = 0.5, cex = 0.2)+
  scale_fill_viridis_d(option = "plasma")+
  scale_color_viridis_d(option = "plasma")+
  # scale_color_manual(values= scale_color_brewer(palette = "Dark2"), name = paste('Average \nsummer temp.'))+
  # scale_fill_manual(values= scale_color_brewer(palette = "Dark2"), name = paste('Average \nsummer temp.'))+
  # guides(fill=guide_legend(ncol=2))+
  # guides(color=guide_legend(ncol=2))+
  theme_classic()+
  # scale_color_brewer(palette = "YlGnBu")+
  # scale_fill_brewer(palette = "YlGnBu")+
  # scale_color_futurama()+
  
  theme(
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 10),
    plot.margin = unit(c(1.1,0.1,0.1,0.8), "cm"),
    legend.key.size=unit(0.5,"cm")
    # legend.title = '',
    # legend.text = 
    # legend.position = 'bottom'
  )
  # scale_color_brewer(palette = "Dark2")





#######################################
### old stuff
# AIC / BIC comparisons
# model_c = lme(logit(prop_ext) ~ tempsee*warming*abs(latitude)*size, random = ~ 1 |name, data = tab)
# model_s = lme(logit(prop_ext) ~ tempsee*warming*abs(latitude), random = ~ 1 |name, data = tab)
# model_l = lme(logit(prop_ext) ~ tempsee*warming*size, random = ~ 1 |name, data = tab)
# model_t = lme(logit(prop_ext) ~ warming*abs(latitude)*size, random = ~ 1 |name, data = tab)
# 
# model_ls = lme(logit(prop_ext) ~ tempsee*warming, random = ~ 1 |name, data = tab)
# model_lt = lme(logit(prop_ext) ~ warming*size, random = ~ 1 |name, data = tab)
# model_ts = lme(logit(prop_ext) ~ warming*abs(latitude), random = ~ 1 |name, data = tab)
# 
# model_lst = lme(logit(prop_ext) ~ warming, random = ~ 1 |name, data = tab)
# 
# 
# model_corrected = lme(logit(prop_ext) ~ tempsee*warming + abs(latitude), random = ~ 1 |name, data = tab)
# model_see = lme(logit(prop_ext) ~ tempsee*warming + latitude, random = ~ 1 |name, data = tab)
# model_bis = lme(logit(prop_ext) ~ tempsee*latitude, random = ~ 1 |name, data = tab)
# 
# AIC(model_see)
# AIC(model_corrected)
# # c = complete, s = minus size, l = minus lattitude ...
# mod.names = c('c', 's', 'l', 't', 'ls', 'lt', 'ts', 'null')
# bics = c(BIC(model_c), BIC(model_s), BIC(model_l), BIC(model_t), BIC(model_ls), BIC(model_lt), BIC(model_ts), BIC(model_lst))
# aics = c(AIC(model_c), AIC(model_s), AIC(model_l), AIC(model_t), AIC(model_ls), AIC(model_lt), AIC(model_ts), AIC(model_lst))
# cbind(mod.names, aics, bics)
# 
# model.best = model_ls
# 
# library(lmerTest)
# model_c.bis = lme(logit(prop_ext) ~ tempsee*warming*abs(latitude)*size*elevation, random = ~ 1 |name, data = tab, method = 'ML')
# stepAIC(model_c.bis)
# AIC(model_ls)
# BIC(model_ls)
# 
# model.try = lme(logit(prop_ext) ~ tempsee + warming + abs(latitude) + size + 
#                   elevation + tempsee:warming + tempsee:abs(latitude) + warming:abs(latitude) + 
#                   tempsee:size + warming:size + abs(latitude):size + tempsee:elevation + 
#                   warming:elevation + abs(latitude):elevation + size:elevation + 
#                   tempsee:warming:abs(latitude) + tempsee:warming:size + warming:abs(latitude):size + 
#                   tempsee:warming:elevation + tempsee:abs(latitude):elevation + 
#                   warming:abs(latitude):elevation + tempsee:size:elevation + 
#                   warming:size:elevation + tempsee:warming:abs(latitude):elevation + 
#                   tempsee:warming:size:elevation, random =~1 |name, data = tab, method = 'ML')
# 
# AIC(model.try)
# BIC(model.try)
# 
# 
# model.intelligent = lme(logit(prop_ext) ~ elevation + size + tempsee*warming, random = ~ 1 |name, data = tab)
# BIC(model.intelligent)
# BIC(model_ls)
# anova(model.intelligent)
# 
# model = lme(logit(prop_ext) ~ init_temp*warming, random = ~ 1 |name, data = tab)
# model2 = lme(logit(prop_ext) ~ init_temp*warming*size, random = ~ 1 |name, data = tab)
# model3 = lme(logit(prop_ext) ~ init_temp*warming + size + size:warming, random = ~ 1 |name, data = tab)
# model4 = lme(logit(prop_ext) ~ init_temp*warming + size, random = ~ 1 |name, data = tab)
# model5 = lme(logit(prop_ext) ~ init_temp+warming, random = ~ 1 |name, data = tab)
# 
# #checking elevation
# model.elev = lme(logit(prop_ext) ~ init_temp*warming*elevation*size*abs(latitude), random = ~ 1 |name, data = tab)
# 
# BIC(model.elev)
# BIC(model.best)
# BIC(lme(logit(prop_ext) ~ init_temp*warming+elevation, random = ~ 1 |name, data = tab))
# 

