rm(list=ls())
dev.off()

options(scipen = 999)
require(zoo) ## na.approx function

setwd('/Users/nicoleward/Dropbox/CNH-GLM/Sunapee/Experiments/20170320_Sunapee_Sim0/waterbal') 
evap<-read.csv("simulated_evap.csv")
metfile<-read.csv("SunapeeMet.csv")
stage<-read.csv("field_stage.csv")
#outflow<-read.csv("historical dam outflow 82-16-2.csv")

#inflow.back.calc<-read.csv("inflow_waterbal.csv")
hypso<-read.csv("depth_volume_matrix_0p1m_08Dec2016.csv")
hypso2<-read.csv("hypsography_matrix_0p5m_14Dec2016.csv")
inf790<-read.csv("oneInflow_20Mar17.csv")
#inf788<-read.csv("788_totalinflow_temp_05Jan2017.csv")
# inf665<-read.csv("665_totalinflow_temp_05Jan2017.csv")
# inf505<-read.csv("505_totalinflow_temp_05Jan2017.csv")
# infung<-read.csv("ung_totalinflow_temp_05Jan2017.csv")
# inf830<-read.csv("830_totalinflow_temp_05Jan2017.csv")
# inf805<-read.csv("805_totalinflow_temp_05Jan2017.csv")

#plot(df$date,df$flow_out_m3s)
### interpolate outflow ######
library(dplyr)


inf790$time <- as.POSIXct(inf790$time,tz="EST")
inf788$time <- as.POSIXct(inf788$time,tz="EST")
inf665$time <- as.POSIXct(inf665$time,tz="EST")
inf505$time <- as.POSIXct(inf505$time,tz="EST")
infung$time <- as.POSIXct(infung$time,tz="EST")
inf830$time <- as.POSIXct(inf830$time,tz="EST")
inf805$time <- as.POSIXct(inf805$time,tz="EST")
df <- tbl_df(df)    # not necessary, but prints nicely

#list_df <- list(df, df)    # fake list of data.frames
# make a data.frame of sequence to join on
#seq_df <- data_frame(date = seq.POSIXt(as.POSIXct("1982-01-01"), tz="EST",
#                                           as.POSIXct("2009-07-03"), tz="EST",
#                                           by="day"))
#seq_df$date <-as.POSIXct(strptime(seq_df$date,"%Y-%m-%d", tz="EST")) 
#seq_df$value <- df$flow_out_m3s[pmatch(seq_df$date, df$date)]
# na.approx() uses linear interpolation to fill in missing data 
#interp_outflow<-na.approx(seq_df$value)
#fill_flow<-cbind(seq_df,interp_outflow)


seq_st <- data_frame(date = seq.POSIXt(as.POSIXct("1981-12-23"), tz="EST",
                                       as.POSIXct("2013-12-31"), tz="EST",
                                       by="day"))
seq_st$date <-as.POSIXct(strptime(seq_st$date,"%Y-%m-%d", tz="EST")) 
seq_st$value <- stage$stage[pmatch(seq_st$date, stage$datetime)]
interp_stage<-na.approx(seq_st$value)
fill_stage<-cbind(seq_st,interp_stage)

### daily precip #####
daily_P <-  metfile %>% 
  mutate(time= as.POSIXct(time)) %>% 
  group_by(day=format(time, '%d'),
           #year=format(datetime, '%Y'), #if you need year also
           # as grouping variable
           hour=format(as.POSIXct(cut(time, breaks='hour')), '%H'))

daily_P<-daily_P %>% mutate(Rain.hr=Rain/24) 

daily_precip<-  aggregate(daily_P$Rain.hr, by=list(date=daily_P$time), 
                          FUN=sum)  # precip in m per day


#####


names(daily_precip)[names(daily_precip)=="date"] <- "DateTime"
#names(fill_flow)[names(fill_flow)=="date"] <- "DateTime"
names(fill_stage)[names(fill_stage)=="date"] <- "DateTime"
names(daily_precip)[names(daily_precip)=="x"] <- "precip"
daily_precip$DateTime<-as.POSIXct(daily_precip$DateTime, tz="EST")
evap$DateTime<-as.POSIXct(evap$DateTime, tz="EST")
fill_stage$DateTime<-as.POSIXct(fill_stage$DateTime, tz="EST")
#fill_flow$DateTime<-as.POSIXct(fill_flow$DateTime, tz="EST")
fill_stage<- subset(fill_stage, select=c("DateTime", "interp_stage"))
#fill_flow<- subset(fill_flow, select=c("DateTime", "interp_outflow"))

inf505<- subset(inf505, select=c("time", "flow"))
names(inf505)[names(inf505)==c("time","flow")] <- c("DateTime","flow505")

inf665<- subset(inf665, select=c("time", "flow"))
names(inf665)[names(inf665)==c("time","flow")] <- c("DateTime","flow665")

inf788<- subset(inf788, select=c("time", "flow"))
names(inf788)[names(inf788)==c("time","flow")] <- c("DateTime","flow788")

inf790<- subset(inf790, select=c("time", "flow"))
names(inf790)[names(inf790)==c("time","flow")] <- c("DateTime","flow790")

inf805<- subset(inf805, select=c("time", "flow"))
names(inf805)[names(inf805)==c("time","flow")] <- c("DateTime","flow805")

inf830<- subset(inf830, select=c("time", "flow"))
names(inf830)[names(inf830)==c("time","flow")] <- c("DateTime","flow830")

infung<- subset(infung, select=c("time", "flow"))
names(infung)[names(infung)==c("time","flow")] <- c("DateTime","flowung")

join.exp <- evap %>% left_join(daily_precip) %>% left_join(fill_stage)%>% 
  left_join(inf505)%>% left_join(inf665)%>% left_join(inf788)%>% left_join(inf790)%>% 
  left_join(inf805)%>% left_join(inf830)%>% left_join(infung)

join.exp$DateTime <-as.POSIXct(strptime(join.exp$DateTime,"%Y-%m-%d", tz="EST")) 

join.exp$precip[is.na(join.exp$precip)] <- 0
## hypso volume
join.exp<-join.exp %>% mutate(interp_stage = round(interp_stage, 1))
names(hypso)[names(hypso)=="depth_m"] <- "interp_stage"
join.exp<- join.exp %>% left_join(hypso)

##evap units
join.exp<-join.exp %>% mutate(evap = evap*86164.1) ##evap now in m per d
#join.exp<-join.exp %>% mutate(evap = evap*area_m2) ##evap now in m3 per d

#join.exp<-join.exp %>% mutate(precip = ((precip*area_m2))) ## precip now in m3 per d
#precip and evap in m per day
#join.exp<- join.exp %>% mutate(d.volume = (volume_m3 - lag(volume_m3,n = 1))) # delta volume in m3 per s
join.exp<-join.exp %>% mutate(d.stage = (interp_stage - lag(interp_stage,n = 1))) 

##total inflow
join.exp$flow505<-as.numeric(join.exp$flow505)
join.exp$flowung<-as.numeric(join.exp$flowung)
join.exp$flow805<-as.numeric(join.exp$flow805)
join.exp$flow830<-as.numeric(join.exp$flow830)
join.exp$flow665<-as.numeric(join.exp$flow665)
join.exp$flow788<-as.numeric(join.exp$flow788)
join.exp$flow790<-as.numeric(join.exp$flow790)
join.exp<-join.exp %>% mutate(tot.inf = flowung+flow505+flow790+flow788+flow665+flow830+flow805)  

join.exp<-join.exp %>% mutate(tot.inf = tot.inf*86400/16071489) #inflow total in m per day
join.exp<- join.exp %>% mutate(q.out = - d.stage + precip + tot.inf + evap)

#join.exp<-join.exp %>% mutate(q.out.standard = 0.01063605) #m per day based on residence time at 33 m

#join.exp<- join.exp %>% mutate(q.in = d.stage - precip + q.out.standard - evap) # inflow volume in m3 per second
# evap is subtracted because it is a negative value
  
#test <- join.exp %>% filter(q.in < 0)
test <- join.exp %>% filter(q.out < 0)
test<- test %>% mutate(bal.q = -(q.out)) 
bal <- join.exp %>% left_join(test)
test2 <- join.exp %>% filter(q.out >= 0)
names(test2)[names(test2)==("q.out")] <- ("out.pos")
bal <- bal %>% left_join(test2)

#test3<- bal %>% 
  #rowwise will make sure the sum operation will occur on each row
#  rowwise() %>% 
  #then a simple sum(..., na.rm=TRUE) is enough to result in what you need
#  mutate(new.out.q = sum(bal.q,q.out.standard, na.rm=TRUE))
#test3$in.pos[is.na(test3$in.pos)] <- 0
bal$out.pos[is.na(bal$out.pos)] <- 0
bal$bal.q[is.na(bal$bal.q)] <- 0
bal<- bal %>% mutate(res.in = tot.inf+bal.q)

test3<- bal %>% mutate(my.d.stage = tot.inf+bal.q + precip - out.pos + evap)
test3<- test3 %>% mutate(d.d.stage = my.d.stage - d.stage)
#convert back to GLM needed units
#SA = 16071489
test4<-test3 %>% mutate(bal.q.m3s = bal.q*16071489/86400)
test4<-test4 %>% mutate(out.pos.m3s = out.pos*16071489/86400)
##make files for glm
inflow_waterbal<- subset(test4, select=c("DateTime", "bal.q.m3s"))
outflow_waterbal<- subset(test4, select=c("DateTime", "out.pos.m3s"))

names(inflow_waterbal)[names(inflow_waterbal)==c("DateTime","bal.q.m3s")] <- c("time","flow")
names(outflow_waterbal)[names(outflow_waterbal)==c("DateTime","out.pos.m3s")] <- c("time","flow")
inflow_waterbal<-na.omit(inflow_waterbal)
outflow_waterbal<-na.omit(outflow_waterbal)

write.csv(inflow_waterbal,"inflow_bal_6inf.csv", row.names = FALSE, quote = FALSE)
write.csv(outflow_waterbal,"outflow_bal_6inf.csv", row.names = FALSE, quote = FALSE)
