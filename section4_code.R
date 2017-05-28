# Rstan --------------------
library(rstan)
setwd("/Users/shikunwang/Desktop/BIOSTAT682/final_project")
source("brook1/setup.R")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)
temp=c(50.15934066, 51.26849315, 52.47123288, 51.25549451, 49.7700831, 49.8547486,
       51.96648045, 50.8547486, 53.95251397, 50.10055866, 49.3557423, 50.7877095)
event=c(188, 188, 177, 182, 204, 171, 142, 185, 148, 168, 163, 165)
tem=rep(temp,83)
eve=rep(event,83)
idkey <- 
  crashes.year.county %>%arrange(County,Crash.Year) %>% 
  mutate(id=as.numeric(factor(County)))%>%
  select(id,County)%>%
  group_by(County)%>%
  summarise(cid=first(id))%>%ungroup
crashes.year.county$temp=tem
crashes.year.county$event=eve
## The data frame sent to Stan to fit the model
da <- crashes.year.county %>% arrange(County, Crash.Year) %>%
  mutate(id = as.numeric(factor(County)),
         tyear = Crash.Year - 2004,
         y = n_crash, 
         cpop = County.Population,
         area_sqmi_log = log(county.land.sqmi),
         unemp = county.unemployment.rate,
         income_log = log(county.percapita.income),
         temp = tem,
         event = event) %>%
  select(id,tyear,y,area_sqmi_log,unemp,income_log, cpop,temp,event) 

modparam <- 
  list(x1 = model.matrix(~ 1 + tyear + income_log + unemp + temp + event,
                         data=da),
       x2 = cbind(unique(da$area_sqmi_log)),
       lcpop100 = log(da$cpop / 100000),
       id = da$id,
       y = da$y,
       Ncounties = length(unique(da$id)),
       Nobs = nrow(da),
       Nyears = length(unique(da$tyear)),
       p = 6,
       k = 1)
fit1=stan("brook1/brook_mod_apr7.stan",data=modparam,iter=20000, warmup=5000, chain=1)
save(fit1,file="fit1.Rdata")
f1=summary(fit1)
load('fit1.Rdata')
# Analysis -----------------------

f1$summary[grepl("theta",rownames(f1$summary)),]
sum1 <-summary(fit11, pars = c("beta"), probs = c(0.1, 0.9))$summary[,c(1,4,5,7)]
sum2 <- summary(fit11, pars = c("theta", "alpha","tau","sigma"), probs = c(0.1, 0.9))$summary[,c(1,4,5,7)]
ta=t(round(sum2,4))
xtable(ta[,1:7],digits=4)

b1postmean <- apply(betapost[,,2],2,mean)
CIcutoff <- 0.1
CIlevel <- 1 - 2*CIcutoff
CIquantiles <- c(CIcutoff, 1-CIcutoff)
post_meanci <- function(x) return(c(mean(x), quantile(x, probs=CIquantiles)))

b2postCI <-
  data.frame(t(apply(exp(betapost[,,2]),
          2, function(x) return(setNames(post_meanci(x),
          c('exp_mtrend','exp_mtrend_lwr80','exp_mtrend_upr80'))))))
rownames(b2postCI)=idkey$County
b2postCI=b2postCI[order(b2postCI$exp_mtrend,decreasing=T),]

library(grid)
text_1 <- textGrob("(No change)", gp=gpar(fontsize=7))
text_95 <- textGrob("(5% decrease)", gp=gpar(fontsize=7))

p=ggplot(b2postCI, aes(x=factor(1:83), y=exp_mtrend)) + 
  geom_errorbar(aes(ymin=exp_mtrend_lwr80, ymax=exp_mtrend_upr80), width=.1) +
  geom_point()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_discrete(labels = rownames(b2postCI))+
  annotation_custom(text_1,xmin=-2.5,xmax=-2.5,ymin=0.995,ymax=0.995) + 
  annotation_custom(text_95,xmin=-2.5,xmax=-2.5,ymin=0.945,ymax=0.945) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=7))+
  xlab("")+ylab( expression(e^beta[1]))+ggtitle("Annual rate of change (multiplicative)")
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)


p=ggplot(b2postCI, aes(y=factor(1:83), x=exp_mtrend)) + 
  geom_errorbarh(aes(xmin=exp_mtrend_lwr80, xmax=exp_mtrend_upr80), width=.1) +
  geom_point()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_discrete(labels = rownames(b2postCI))+
  annotation_custom(text_1,ymin=-2.5,ymax=-2.5,xmin=1,xmax=1) + 
  annotation_custom(text_95,ymin=-2.5,ymax=-2.5,xmin=.95,xmax=.95) +
  theme(axis.text.y = element_text(size=7))+
  ylab("")+xlab( expression(e^beta[1]))+ggtitle("Annual rate of change (multiplicative)")
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)


n=NULL;m=NULL
mupost=extract(fit11, pars='mu', permuted=T)[[1]]
for(i in 1:83) n=cbind(n,rep(idkey$County[i],12))

n=t(matrix(n,nrow=1))
m=rep(2004:2015,83)
  mu=matrix(mupost,ncol=50000)
  mu1=apply(mu,1,mean)
  mu2=data.frame(V=exp(mu1))
mu2$County=n
mu2$year=m
  ggplot(mu2,aes(x=2004:2015))+geom_point()
  mu2$up=NULL
  mu2$up=ifelse(match(mu2$County,upcounties)>0,1,-1)
  x_melt = melt(mu2, id.vars=c('V', 'year'))
  
  upcounties <- c('Ingham','Washtenaw','Wayne','Macomb','Genesee','Saginaw','Oakland')
  
  ggplot(x_melt, aes(x=year, y=V, color=value)) +geom_line()+
    geom_point() + facet_grid(~variable) + theme(legend.position='none')
  
  
  
  # Plot map ------------------------------------------------------

  df = mutate(crashes.year.county, rate = n_crash / County.Population * 100000)
  gb = group_by(df, County)
  toplot = summarize(gb, avg = mean(rate), delta = rate[12]-rate[1])
  toplot = toplot[order(toplot$avg, decreasing = T), ]
  cc=cincome[914:996,1:2]
  toplot$income=cc$county.percapita.income[match(cc$County,toplot$County)]
  
  pp=cpop[which(cpop$year==2015),][,c(1,3)]
  toplot$population=pp$County.Population[match(pp$County,toplot$County)]
  
  p1 = ggplot(toplot[1:20,], aes(x = avg, y = delta, label = County,color=income,size=population)) +
    geom_point()+ geom_text(col=1) + xlab("Average Annual Rate of Crashes per 100k") + 
    ylab("Change in Annual Crash Rate from 2004-2015") + 
    ggtitle("Bicycling Crashes from 2004-2015")+ theme_bw()+ 
    scale_colour_gradient(low = "blue",high="red")

  library(ggmap)
  m <- map_data("county", "Michigan") 
  x=crashes.year.county
  
  x$rate=crashes.year.county$n_crash/(crashes.year.county$County.Population/100000)
  nam=unique(x$County)
  tmp=NULL
  tmp$nam=nam
  tmp$crash=rep(0,length(nam))
  for (i in 1:length(unique(x$County))){
    tmp$crash[i]=mean(x$rate[which(x$County==nam[i])])
  }
  m$rate=rep(0,length(m$subregion))
  
  for(i in 1:length(tmp$nam)){
    m$rate[match(tmp$nam[i],m$subregion)]=tmp$crash[i]
  }
  
  proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
  m$subregion=proper(m$subregion)
p2=  ggplot(data = m) + 
    geom_polygon(aes(x = long, y = lat, fill = rate, group = group), color = "white") + 
    coord_fixed(1.3) +scale_fill_gradient(low='lightblue', high='orange')+ggtitle("Map of average crash rate from 2004 to 2015")+
     theme_bw()+xlab('longitude')+ylab('latitude')
grid.arrange(p1,p2,ncol=2)
