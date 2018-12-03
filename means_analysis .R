#Kayla Mathes Long term Soil Respiration data from Ameriflux and FASET paired plots 
#Analysis of Rs means 

#Import data
all_data <- read.csv("Data/SoilR_processed_data_mathes/compiled_Rs_forR.csv")


#Load packages 
library(ggplot2)
library(dplyr)
library(plotrix)
library(grid)

#create a subset dataframe with doy range in growing season 

means_subset <- all_data%>%
  filter(doy>129)%>%
  filter(doy<279)%>%
  select(year, paired_id, paired_unit, efflux, site)%>%
  filter(year !=2008)

#create a dataframe with the mean efflux value by plot per year 

means_summary <- means_subset%>%
  group_by(paired_id, year, site, paired_unit)%>%
  summarize(ave_efflux= mean(efflux), std_err= std.error(efflux), std_dev= sd(efflux))%>%
  na.omit(efflux)

#create graphs showing efflux means across time by plot 

ggplot(means_summary, aes( x = year, y = ave_efflux, color=paired_id))+
  geom_line(size = 1, alpha = 0.5)+
  facet_wrap(~site)+
  scale_color_manual("", values = c('1C' = 'grey55','1F' = 'grey54','2C' = 'black','2F' = 'gray1', '3C' = 'red', '3F' = 'red1', '4C' = 'orange1', '4F' = 'orange', '5C' = 'yellowgreen', '5F' = 'yellow4', '6C' = 'turquoise', '6F' = 'turquoise1', '7C' = 'steelblue3', '7F' = 'steelblue4', '8C' = 'purple1', '8F' = 'purple'))
  
#subset the mean efflux by plot 

pair_8 <- filter(means_summary, paired_id == "8C" | paired_id == "8F")


ggplot(pair_8, aes( x = year, y = ave_efflux, color=site))+
  geom_line(size = 1, alpha = 0.5)+
  geom_errorbar(aes(ymin=ave_efflux - std_err, ymax=ave_efflux + std_err), width=.1) 


#Calcuate means and graph means and std error across all plots by year between FASET and AMERIFLUX 
means_across_plots <- means_summary%>%
  group_by(year, site)%>%
  summarize(total_ave_efflux=mean(ave_efflux),  std_err= std.error(ave_efflux), std_dv = sd(ave_efflux))%>%
  mutate(CV=std_dv/total_ave_efflux*100)

ggplot(means_across_plots, aes( x = year, y = total_ave_efflux, color=site))+
  geom_line(size = 1, alpha = 0.5)+
  geom_errorbar(aes(ymin=total_ave_efflux - std_err, ymax=total_ave_efflux + std_err), width=.1) 

##calculate CV for AMERIFLUX and FASET across years  
ggplot(means_across_plots, aes( x = year, y = CV, color=site))+
         geom_line(size = 1, alpha = 0.5)

## create delta values for mean efflux values across years per plot (shows last iteration)
summary<-list()
paired_unit <- c(1:8)
 for (i in 1:8) {
pair[i] <- subset(means_summary, means_summary$paired_unit==i)
yeargroup <- split(pair$ave_efflux, as.factor(pair$year))
summary[[i]] <- diff
for (i in 1:9) {
diff[i] <- yeargroup[[i]][2] - yeargroup[[i]][1]
}
return(diff[i])
 }

##messy as HELL delta code 
diff1 <- data.frame()
paired_unit <- c(1:8)
for (i in 1:8) {
  pair <- subset(means_summary, means_summary$paired_unit==1)
  yeargroup <- split(pair$ave_efflux, as.factor(pair$year))
  
  for (i in 1:9) {
    diff1[i,1] <- yeargroup[[i]][2] - yeargroup[[i]][1]
  }
}

diff2 <- data.frame()
paired_unit <- c(1:8)
for (i in 1:8) {
  pair <- subset(means_summary, means_summary$paired_unit==2)
  yeargroup <- split(pair$ave_efflux, as.factor(pair$year))
  
  for (i in 1:9) {
    diff2[i,1] <- yeargroup[[i]][2] - yeargroup[[i]][1]
  }
}

diff3 <- data.frame()
paired_unit <- c(1:8)
for (i in 1:8) {
  pair <- subset(means_summary, means_summary$paired_unit==3)
  yeargroup <- split(pair$ave_efflux, as.factor(pair$year))
  
  for (i in 1:9) {
    diff3[i,1] <- yeargroup[[i]][2] - yeargroup[[i]][1]
  }
}

diff4 <- data.frame()
paired_unit <- c(1:8)
for (i in 1:8) {
  pair <- subset(means_summary, means_summary$paired_unit==4)
  yeargroup <- split(pair$ave_efflux, as.factor(pair$year))
  
  for (i in 1:9) {
    diff4[i,1] <- yeargroup[[i]][2] - yeargroup[[i]][1]
  }
}

diff5 <- data.frame()
paired_unit <- c(1:8)
for (i in 1:8) {
  pair <- subset(means_summary, means_summary$paired_unit==5)
  yeargroup <- split(pair$ave_efflux, as.factor(pair$year))
  
  for (i in 1:9) {
    diff5[i,1] <- yeargroup[[i]][2] - yeargroup[[i]][1]
  }
}

diff6 <- data.frame()
paired_unit <- c(1:8)
for (i in 1:8) {
  pair <- subset(means_summary, means_summary$paired_unit==6)
  yeargroup <- split(pair$ave_efflux, as.factor(pair$year))
  
  for (i in 1:9) {
    diff6[i,1] <- yeargroup[[i]][2] - yeargroup[[i]][1]
  }
}

diff7 <- data.frame()
paired_unit <- c(1:8)
for (i in 1:8) {
  pair <- subset(means_summary, means_summary$paired_unit==7)
  yeargroup <- split(pair$ave_efflux, as.factor(pair$year))
  
  for (i in 1:9) {
    diff7[i,1] <- yeargroup[[i]][2] - yeargroup[[i]][1]
  }
}

diff8 <- data.frame()
paired_unit <- c(1:8)
for (i in 1:8) {
  pair <- subset(means_summary, means_summary$paired_unit==8)
  yeargroup <- split(pair$ave_efflux, as.factor(pair$year))
  
  for (i in 1:9) {
    diff8[i,1] <- (yeargroup[[i]][2] - yeargroup[[i]][1])
  }
}
##Add years and pair_unit to diff columns (shows last iteration)

diff1<- diff1%>%
  mutate(year=c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"),paired_unit=c("1", "1", "1", "1", "1", "1", "1", "1","1"))

diff2<- diff2%>%
  mutate(year=c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"),paired_unit=c("2", "2", "2", "2", "2", "2", "2", "2","2"))

diff3<- diff3%>%
  mutate(year=c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"),paired_unit=c("3", "3", "3", "3", "3", "3", "3", "3","3"))

diff4<- diff4%>%
  mutate(year=c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"),paired_unit=c("4", "4", "4", "4", "4", "4", "4", "4","4"))

diff5<- diff5%>%
  mutate(year=c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"),paired_unit=c("5", "5", "5", "5", "5", "5", "5", "5","5"))

diff6<- diff6%>%
  mutate(year=c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"),paired_unit=c("6", "6", "6", "6", "6", "6", "6", "6","6"))

diff7<- diff7%>%
  mutate(year=c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"),paired_unit=c("7", "7", "7", "7", "7", "7", "7", "7","7"))

diff8<- diff8%>%
  mutate(year=c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"),paired_unit=c("8", "8", "8", "8", "8", "8", "8", "8","8"))

#make a dataframe with all delta values 
diff_all <- rbind(diff1, diff2, diff3, diff4, diff5, diff6, diff7, diff8)

colnames(diff_all) <- c("delta", "year", "pair_unit")

##plot the deltas (facet-ameriflux by pair)

 plot1 <- ggplot(ave_delta, aes(x = year, y = delta_ave, group = 1))+
  geom_point()+
  geom_path(linetype = 2)+
  theme_classic()+
   theme(axis.text = element_text(size = 14))+
  geom_errorbar(aes(ymin=delta_ave - delta_se, ymax=delta_ave + delta_se), width=.1)+
   theme(legend.position="none",axis.title.x = element_blank(), axis.title.y = element_blank())
 
  
  plot2 <- ggplot()+
    geom_line(data = diff_all, aes(x = year, y = delta, group = pair_unit, color = pair_unit))+
    theme_classic()+
    theme(axis.text = element_text(size = 14))+
    theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) 
   
  
grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))

ggsave("5mTfigure2.jpg", height = 10, width = 5, units = "in")
##calcuate the mean delta by year 

ave_delta <- diff_all%>%
  na.omit()%>%
  group_by(year)%>%
  summarize(delta_ave=mean(delta), delta_se=std.error(delta), delta_sd=sd(delta))
  

#plot average delta across all pairs by year 
ggplot(ave_delta, aes(x = year, y = delta_ave, group = 1))+
  geom_point()+
  geom_path(linetype = 2)+
  theme_bw()+
  xlab("Year") + ylab("Average Change in Soil Respiration")+
  geom_errorbar(aes(ymin=delta_ave - delta_se, ymax=delta_ave + delta_se), width=.1)


##make dataframes for Chris 

mean_delta_combined <- data.frame(pair_unit = diff_all$pair_unit, year = diff_all$year, mean_delta = diff_all$delta)

mean_delta_by_pair <- ave_delta

write.csv(mean_delta_combined, file = "MyData.csv",row.names=FALSE)

#Plot level Variance 

plot_variance <- all_data %>%
  filter(year=="2009" | year == "2010")%>%
  filter(doy>129)%>%
  filter(doy<279)
  
ggsave("5mTfigure.jpg", height = 5, width = 5, units = "in")