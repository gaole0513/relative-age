library(dplyr)
library(lubridate)
library("survival")
library(readxl)
library(pbapply)
library(tableone)
library(data.table)

#Analysis 1 
# path <- "M:/Cohort Raw Data (do not edit)/DIAMOND/2.convert_update2023/1.mother-child"
# 
# filenames_list <- list.files(path=path, pattern="*.xlsx",full.names=TRUE)
# 
# All <- lapply(filenames_list,function(filename){
#   print(paste("Merging",filename,sep = " "))
#   read_xlsx(filename,sheet = "Data")
# })
# 
# mc_01to23 <- do.call(rbind.data.frame, All)
# colnames(mc_01to23) <- make.names(colnames(mc_01to23)) #708020
# saveRDS(mc_01to23,"M:/Cohort Raw Data (do not edit)/DIAMOND/3.combined_2023/mc_01to23.rds")
mc_01to23 <- readRDS("M:/Cohort Raw Data (do not edit)/DIAMOND/3.combined_2023/mc_01to23.rds")
length(unique(mc_01to23$Baby.s.Reference.Key.))
# path <- "M:/Cohort Raw Data (do not edit)/DIAMOND/2.convert_update2023/3.child/3.death/"
# 
# filenames_list <- list.files(path=path, pattern="*.xlsx",full.names=TRUE)
# 
# All <- lapply(filenames_list,function(filename){
#   print(paste("Merging",filename,sep = " "))
#   read_xlsx(filename,sheet = "Data")
# })
# 
# bb_death <- do.call(rbind.data.frame, All)
# colnames(bb_death) <- make.names(colnames(bb_death)) #708020
# saveRDS(bb_death,"M:/Cohort Raw Data (do not edit)/DIAMOND/3.combined_2023/bb_death.rds")
bb_death1 <- readRDS("M:/Cohort Raw Data (do not edit)/DIAMOND/3.combined_2023/bb_death.rds")
length(unique(bb_death$Reference.Key.))
bb_death <- bb_death1 %>% 
  arrange(Reference.Key.,Date.of.Registered.Death.) %>% 
  filter(duplicated(Reference.Key.)==F) %>% 
  select(Reference.Key.,Date.of.Birth..yyyy.mm.dd..,Date.of.Registered.Death.,Sex.)
length(unique(bb_death$Reference.Key.))#653712

length(unique(bb0117$Baby.s.Reference.Key.))
bb0117 <- merge(mc_01to23 %>% select(-Date.of.Birth..yyyy.mm.dd..,-Date.of.Registered.Death.),
                bb_death %>% distinct(),by.x = "Baby.s.Reference.Key.",by.y = "Reference.Key.",all.x = T) %>% #708020
  mutate(Sex.=ifelse(is.na(Sex.),Baby.s.Info..Sex.,Sex.),
         Date.of.Birth..yyyy.mm.dd..=if_else(is.na(Date.of.Birth..yyyy.mm.dd..),as.Date(Maternity.Episode..Delivery.Date..yyyy.mm.dd..),Date.of.Birth..yyyy.mm.dd..)) %>% 
  filter(!is.na(Baby.s.Reference.Key.), #660112
         Sex.!="U") %>% #660107 
  mutate(Date.of.Birth..yyyy.mm.dd..=as.Date(Date.of.Birth..yyyy.mm.dd..),
         Date.of.Registered.Death.=as.Date(Date.of.Registered.Death.),
         Baby.s.Info..Birth.Weight..gm..=as.numeric(Baby.s.Info..Birth.Weight..gm..),
         Maternity.Episode..Gestation.weeks.1=ifelse(is.na(Maternity.Episode..Gestation.weeks.),39,Maternity.Episode..Gestation.weeks.),
         preterm=ifelse(Maternity.Episode..Gestation.weeks.1<37,1,0),
         Baby.s.Info..Apgar.Score.at.5.min.=if_else(Baby.s.Info..Apgar.Score.at.5.min.==99,as.numeric(NA),as.numeric(Baby.s.Info..Apgar.Score.at.5.min.))) %>% 
  mutate(md=substring(Date.of.Birth..yyyy.mm.dd..,6,10),
         DOB1 = ifelse(md=="02-29",
                       as.character(paste0(year(Date.of.Birth..yyyy.mm.dd..),"-02-28")),
                       as.character(Date.of.Birth..yyyy.mm.dd..)),
         DOB=as.Date(DOB1),
         age6=as.Date(DOB)+years(6),
         age13=as.Date(DOB)+years(13),
         age18=as.Date(DOB)+years(18)) %>% 
  filter(is.na(Date.of.Registered.Death.)|Date.of.Registered.Death.>age6) %>% #658128
  filter(year(DOB)>=2001,
         year(DOB)<=2010)#229124
length(unique(bb0117$Baby.s.Reference.Key.))
bb0117$apgar <- "missing"
bb0117$apgar[which(bb0117$Baby.s.Info..Apgar.Score.at.5.min.>=7)] <- "0reassuring"
bb0117$apgar[which(bb0117$Baby.s.Info..Apgar.Score.at.5.min.>=4&bb0117$Baby.s.Info..Apgar.Score.at.5.min.<=6)] <- "moderately abnormal"
bb0117$apgar[which(bb0117$Baby.s.Info..Apgar.Score.at.5.min.<=3)] <- "illness"




sga <- bb0117 %>%
  mutate(gestation_week1=ifelse(Maternity.Episode..Gestation.weeks.1<=28,28,Maternity.Episode..Gestation.weeks.1)) %>% 
  group_by(gestation_week1) %>% 
  mutate(gw_mean=mean(as.numeric((Baby.s.Info..Birth.Weight..gm..))),
         gw_sd=sd(as.numeric((Baby.s.Info..Birth.Weight..gm..))),
         sga=gw_mean-2*gw_sd) %>% 
  filter(duplicated(Maternity.Episode..Gestation.weeks.1)==F) %>% 
  ungroup() %>% 
  dplyr::select(Maternity.Episode..Gestation.weeks.1,sga)

bb0117<- merge(bb0117,sga,by="Maternity.Episode..Gestation.weeks.1") %>% 
  mutate(lb=ifelse(as.numeric(Baby.s.Info..Birth.Weight..gm..)<sga,1,0))


table(bb0117$District.of.Residence.on.Latest.Selected.Encounter..district..)
bb0117$ses <- NA
bb0117$ses[which(bb0117$District.of.Residence.on.Latest.Selected.Encounter..district.. %in% c("CENTRAL & WESTE","WANCHAI","SAI KUNG excl.","TSEUNG KWAN O","EASTERN"))] <- "HIGH"
bb0117$ses[which(bb0117$District.of.Residence.on.Latest.Selected.Encounter..district.. %in% c("SOUTHERN","KOWLOON CITY","MONGKOK","YAU TSIM","TSUEN WAN","TAI PO"))] <- "Midium1" # upper middle
bb0117$ses[which(bb0117$District.of.Residence.on.Latest.Selected.Encounter..district.. %in% c("SHATIN","OTHERS","ISLANDS excl. N","NORTH LANTAU","YUEN LONG","NORTH"))] <- "Midium2"#lower middle
bb0117$ses[which(bb0117$District.of.Residence.on.Latest.Selected.Encounter..district.. %in% c("WONG TAI SIN","TUEN MUN","SHAM SHUI PO","KWAI TSING","KWUN TONG"))] <- " LOW"


table(bb0117$Baby.s.Info..Mode.of.Delivery.)
bb0117$d_mode <- "Other"
bb0117$d_mode[which(bb0117$Baby.s.Info..Mode.of.Delivery. %in% c("Breech"))] <- "Breech"
bb0117$d_mode[which(bb0117$Baby.s.Info..Mode.of.Delivery. %in% c("Classical CS","LSCS"))] <- "CS"
bb0117$d_mode[which(bb0117$Baby.s.Info..Mode.of.Delivery. %in% c("Forceps"))] <- "Forceps"
bb0117$d_mode[which(bb0117$Baby.s.Info..Mode.of.Delivery. %in% c("NSD"))] <- "NSD"
bb0117$d_mode[which(bb0117$Baby.s.Info..Mode.of.Delivery. %in% c("V/E"))] <- "V/E"



# path <- "M:/Cohort Raw Data (do not edit)/DIAMOND/2.convert_update2023/3.child/1.dx/"
# 
# filenames_list <- list.files(path=path, pattern="*.xlsx",full.names=TRUE)
# 
# All <- lapply(filenames_list,function(filename){
#   print(paste("Merging",filename,sep = " "))
#   read_xlsx(filename,sheet = "Data")
# })
# 
# bb_dx <- do.call(rbind.data.frame, All)
# colnames(bb_dx) <- make.names(colnames(bb_dx)) #708020
# saveRDS(bb_dx,"M:/Cohort Raw Data (do not edit)/DIAMOND/3.combined_2023/bb_dx.rds")

length(unique(bb_dx$Reference.Key.))
bb_dx <- readRDS("M:/Cohort Raw Data (do not edit)/DIAMOND/3.combined_2023/bb_dx.rds")
dx_adhd <- bb_dx %>% 
  filter(grepl("^314",All.Diagnosis.Code..ICD9..)==T) %>% 
  mutate(adhd_date=as.Date(Reference.Date.)) %>% 
  select(Reference.Key.,adhd_date)
length(unique(dx_adhd$Reference.Key.))
# path <- "M:/Cohort Raw Data (do not edit)/DIAMOND/2.convert_update2023/3.child/2.rx/"
# 
# filenames_list <- list.files(path=path, pattern="*.xlsx",full.names=TRUE)
# 
# All <- pblapply(filenames_list,function(filename){
#   read_xlsx(filename,sheet = "Data")
# })
# 
# All1 <- lapply(All, function(x) x[c('Reference Key\n','Dispensing Date (yyyy-mm-dd)\n','Prescription Start Date\n','Therapeutic Classification (BNF, Principal)\n')])
# bb_rx <- do.call(rbind.data.frame, All1)
# colnames(bb_rx) <- make.names(colnames(bb_rx)) #708020
# saveRDS(bb_rx,"M:/Cohort Raw Data (do not edit)/DIAMOND/3.combined_2023/bb_rx.rds")



bb_rx <- readRDS("M:/Cohort Raw Data (do not edit)/DIAMOND/3.combined_2023/bb_rx.rds")
rx_adhd <- bb_rx %>% 
  mutate(rxst=if_else(is.na(Prescription.Start.Date.),
                      as.Date(Dispensing.Date..yyyy.mm.dd..),
                      as.Date(Prescription.Start.Date.))) %>% 
  filter(grepl("^4.4",Therapeutic.Classification..BNF..Principal..)==T) %>% 
  mutate(adhd_date=rxst) %>% 
  select(Reference.Key.,adhd_date)

dx_rx_adhd <- rbind(dx_adhd) %>% 
  arrange(Reference.Key.,adhd_date) %>% 
  filter(duplicated(Reference.Key.)==F)

final1 <- merge(bb0117,dx_rx_adhd,by.x = "Baby.s.Reference.Key.",by.y = "Reference.Key.",all.x = T) %>% 
  filter(is.na(adhd_date)|adhd_date>=age6) %>% #228028
  mutate(edate=pmin(age13-1,
                    Date.of.Registered.Death.,
                    ymd('2023-12-31'),
                    na.rm = TRUE)) %>% 
  # filter(edate>=age6) %>% 
  mutate(event=ifelse(is.na(adhd_date)|adhd_date>edate,0,1)) %>% 
  mutate(event=if_else(is.na(event),0,event),
         fu_days=ifelse(event==1,
                        as.numeric(adhd_date-age6+1),
                        as.numeric(edate-age6+1)),
         birth_mn=month(Date.of.Birth..yyyy.mm.dd..),
         birth_yr=year(Date.of.Birth..yyyy.mm.dd..))#229341
length(unique(final1$Baby.s.Reference.Key.))
table(final1$birth_mn)

# fm <- readRDS("M:/Personal/Gao Le/to_fm/fm_study1.rds") %>% 
#   filter(!is.na(expo))

dat <- final1 %>% 
  filter(birth_mn==12|birth_mn==11|birth_mn==1|birth_mn==2) %>%
  mutate(exp=ifelse(birth_mn==1|birth_mn==2,0,1))
table(dat$exp)
py <- dat %>% 
  group_by(exp) %>% 
  mutate(indiv_sum=length(Baby.s.Reference.Key.),
         event_sum=sum(event),
         py_sum=sum(fu_days)/365.25) %>% 
  filter(duplicated(exp)==F) %>%
  select(exp,indiv_sum,event_sum,py_sum) %>% 
  mutate(inci=event_sum/indiv_sum*100,
         ir=event_sum/py_sum*1000)
table(dat$exp,dat$event)




#cox regression
#crude
result.cox1 <- coxph(Surv(fu_days, event) ~ exp, 
                     data =dat)
summary(result.cox1)
#adjusted
result.cox1 <- coxph(Surv(fu_days, event) ~ exp+Sex.+as.factor(birth_yr)+lb+preterm+Baby.s.Info..With.Birth.Trauma.mentioned..Y.N..+as.factor(apgar)+as.factor(ses)+as.factor(d_mode), 
                     data =dat)
summary(result.cox1)






#Analysis 2

final1 <- merge(bb0117,dx_rx_adhd,by.x = "Baby.s.Reference.Key.",by.y = "Reference.Key.",all.x = T) %>% 
  filter(is.na(adhd_date)|adhd_date>=age13,
         !is.na(Sex.)) %>% 
  filter(is.na(Date.of.Registered.Death.)|Date.of.Registered.Death.>age13) %>% 
  mutate(edate=pmin(age18-1,
                    Date.of.Registered.Death.,
                    ymd('2023-12-31'),
                    na.rm = TRUE)) %>% 
  mutate(event=ifelse(is.na(adhd_date)|adhd_date>edate,0,1),
         fu_days=ifelse(event==1,
                        as.numeric(adhd_date-age13+1),
                        as.numeric(edate-age13+1)),
         birth_mn=month(Date.of.Birth..yyyy.mm.dd..),
         birth_yr=year(Date.of.Birth..yyyy.mm.dd..))#216278
length(unique(final1$Baby.s.Reference.Key.))

dat <- final1 %>% 
  filter(birth_mn==12|birth_mn==11|birth_mn==1|birth_mn==2) %>%
  mutate(exp=ifelse(birth_mn==1|birth_mn==2,0,1))
table(dat$exp)

py <- dat %>% 
  group_by(exp) %>% 
  mutate(indiv_sum=length(Baby.s.Reference.Key.),
         event_sum=sum(event),
         py_sum=sum(fu_days)/365.25) %>% 
  filter(duplicated(exp)==F) %>%
  select(exp,indiv_sum,event_sum,py_sum) %>% 
  mutate(inci=event_sum/indiv_sum*100,
         ir=event_sum/py_sum*1000)
table(dat$exp,dat$event)


#cox regression
#crude
result.cox1 <- coxph(Surv(fu_days, event) ~ exp, 
                     data =dat)
summary(result.cox1)
#adjusted
result.cox1 <- coxph(Surv(fu_days, event) ~ exp+Sex.+as.factor(birth_yr)+lb+preterm+Baby.s.Info..With.Birth.Trauma.mentioned..Y.N..+as.factor(apgar)+as.factor(ses)+as.factor(d_mode), 
                     data =dat)
summary(result.cox1)


