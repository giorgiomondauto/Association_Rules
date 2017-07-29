chronic_disease=asheville_ass_rules$asheville.ICDCode3
freq=sort(table(chronic_disease),decreasing=T)
# 401 High Blood Pressure
# 272 Metabolism Disorder
# 300 Anxiety
# 296 Mood Disorder
# 462 throat pain
# 530 Disease of esophagus
# 477 Allergic rhinitis (related to nose)
# 305 Nondependend abuse of drugs
# 715 Osteoarthrosis
# 461 Acute sinusitis
# 244 Acquired Hypothyrodism
# 309 Adjustment Reaction
# 366 cataract
# 278 abnormal weight
# 268 vitamin of defiency
# 493 asthma
# 250 Diabetes
# 722 Intervertebral disc disorders
# 626 disorders of menstruation
# 627 Menopausa
# 327 organic sleep disorder
# 314 hyperkinetic syndrome of cildhood
# 427 cardiac dysrhythimias  ---> heart disease
# 721 spondylosis and allied disorders
# 473 chronic sinusitis
# 346 migraine

library(data.table)

sankey=data.frame(asheville$MBR_PTY_ID,asheville$AGE,asheville$GDR_TYP_CD,asheville$ICDCode)
sankey_unique <- unique( sankey[ , 1:4 ] )
sankey_unique$asheville.ICDCode <- substr(sankey_unique$asheville.ICDCode, 0, 3)
sankey_unique$asheville.AGE=cut(sankey_unique$asheville.AGE,breaks=c(0,20,40,60,80,110))
write.csv(sankey_unique,file='C:/Users/gmondaut/Desktop/HealthFlow/sankey_unique.csv')


#subset by Gender

#subset by Female
sankey_female=subset(sankey_unique,asheville.GDR_TYP_CD=='F')
sankey_female_id_unique=unique(sankey_female$asheville.MBR_PTY_ID) #8325
x <- 0
for(i in 8326:48870 ){
  sankey_female_id_unique[i] <- x
}

sankey_female$uniqueid=sankey_female_id_unique

x <- list()
for(i in 1:8325) {

  da1=c(sankey_female[sankey_female$asheville.MBR_PTY_ID == sankey_female$uniqueid[i],4])
  x[[i]] <- da1
  
}
rules_b_female <- apriori(x, parameter = list(supp = 0.05, conf = 0.05, target = "rules"))
outcome_female=as(rules_b_female,'data.frame')
DT::datatable(outcome_female)
install.packages('arulesViz')
library(arulesViz)
plot(rules_b_female,interactive=TRUE)

#subset by male
sankey_male=subset(sankey_unique,asheville.GDR_TYP_CD=='M')
sankey_male_id_unique=unique(sankey_male$asheville.MBR_PTY_ID) #6262
x <- 0
for(i in 6263:35121 ){
  sankey_male_id_unique[i] <- x
}

sankey_male$uniqueid=sankey_male_id_unique

x <- list()
for(i in 1:6262) {
  
  da1=c(sankey_male[sankey_male$asheville.MBR_PTY_ID == sankey_male$uniqueid[i],4])
  x[[i]] <- da1
  
}

rules_b_male <- apriori(x, parameter = list(supp = 0.05, conf = 0.05, target = "rules"))
outcome_male=as(rules_b_male,'data.frame')
DT::datatable(outcome_male)
install.packages('arulesViz')
library(arulesViz)
plot(rules_b_male,interactive=TRUE)


#subsets by age
# 0-20
sankey_0_20=subset(sankey_unique,asheville.AGE=='(0,20]') #4299
sankey_0_20_id_unique=unique(sankey_0_20$asheville.MBR_PTY_ID) 
length(sankey_0_20_id_unique) #1541
x <- 0
for(i in 1542:4299 ){
  sankey_0_20_id_unique[i] <- x
}
length(sankey_0_20_id_unique)

sankey_0_20$uniqueid=sankey_0_20_id_unique

x <- list()
for(i in 1:4299) {
  
  da1=c(sankey_male[sankey_male$asheville.MBR_PTY_ID == sankey_male$uniqueid[i],4])
  x[[i]] <- da1
  
}

rules_b_0_20 <- apriori(x, parameter = list(supp = 0.05, conf = 0.05, target = "rules"))
outcome_0_20=as(rules_b_0_20,'data.frame')
DT::datatable(outcome_0_20)
install.packages('arulesViz')
library(arulesViz)
plot(rules_b_male,interactive=TRUE)



# 20-40
sankey_20_40=subset(sankey_unique,asheville.AGE=='(20,40]') #16399
sankey_20_40_id_unique=unique(sankey_20_40$asheville.MBR_PTY_ID) 
length(sankey_20_40_id_unique) #4626
x <- 0
for(i in 4627:16399 ){
  sankey_20_40_id_unique[i] <- x
}
length(sankey_20_40_id_unique)

sankey_20_40$uniqueid=sankey_20_40_id_unique

x <- list()
for(i in 1:4299) {
  
  da1=c(sankey_20_40[sankey_20_40$asheville.MBR_PTY_ID == sankey_20_40$uniqueid[i],4])
  x[[i]] <- da1
  
}

rules_b_20_40 <- apriori(x, parameter = list(supp = 0.01, conf = 0.01, target = "rules"))
outcome_20_40=as(rules_b_20_40,'data.frame')
DT::datatable(outcome_20_40)
install.packages('arulesViz')
library(arulesViz)
plot(rules_b_male,interactive=TRUE)




# 40-60
sankey_40_60=subset(sankey_unique,asheville.AGE=='(40,60]') #29275
sankey_40_60_id_unique=unique(sankey_40_60$asheville.MBR_PTY_ID) 
length(sankey_40_60_id_unique) #5223
x <- 0
for(i in 5224:29275 ){
  sankey_40_60_id_unique[i] <- x
}
length(sankey_40_60_id_unique)

sankey_40_60$uniqueid=sankey_40_60_id_unique

x <- list()
for(i in 1:5223) {
  
  da1=c(sankey_40_60[sankey_40_60$asheville.MBR_PTY_ID == sankey_40_60$uniqueid[i],4])
  x[[i]] <- da1
  
}

rules_b_40_60 <- apriori(x, parameter = list(supp = 0.05, conf = 0.05, target = "rules"))
outcome_40_60=as(rules_b_40_60,'data.frame')
DT::datatable(outcome_40_60)
install.packages('arulesViz')
library(arulesViz)
plot(rules_b_male,interactive=TRUE)



# 60-80
sankey_60_80=subset(sankey_unique,asheville.AGE=='(60,80]') #24094
sankey_60_80_id_unique=unique(sankey_60_80$asheville.MBR_PTY_ID)#2687 

x <- 0
for(i in 2688:24094 ){
  sankey_60_80_id_unique[i] <- x
}
length(sankey_60_80_id_unique)

sankey_60_80$uniqueid=sankey_60_80_id_unique

x <- list()
for(i in 1:2687) {
  
  da1=c(sankey_60_80[sankey_60_80$asheville.MBR_PTY_ID == sankey_60_80$uniqueid[i],4])
  x[[i]] <- da1
  
}

rules_b_60_80 <- apriori(x, parameter = list(supp = 0.05, conf = 0.05, target = "rules"))
outcome_40_60=as(rules_b_60_80,'data.frame')
DT::datatable(outcome_60_80)
install.packages('arulesViz')
library(arulesViz)
plot(rules_b_male,interactive=TRUE)




# 80-110
sankey_80_110=subset(sankey_unique,asheville.AGE=='(80,110]') #9919
sankey_80_110_id_unique=unique(sankey_80_110$asheville.MBR_PTY_ID)#481

x <- 0
for(i in 482:9919 ){
  sankey_80_110_id_unique[i] <- x
}
length(sankey_80_110_id_unique)

sankey_80_110$uniqueid=sankey_80_110_id_unique

x <- list()
for(i in 1:481) {
  
  da1=c(sankey_80_110[sankey_80_110$asheville.MBR_PTY_ID == sankey_80_110$uniqueid[i],4])
  x[[i]] <- da1
  
}

rules_b_80_110 <- apriori(x, parameter = list(supp = 0.05, conf = 0.05, target = "rules"))
outcome_80_110=as(rules_b_80_110,'data.frame')
DT::datatable(outcome_80_110)
install.packages('arulesViz')
library(arulesViz)
plot(rules_b_male,interactive=TRUE)




#subsets by age and gender
#female
sankey_0_20_female=subset(sankey_0_20,asheville.GDR_TYP_CD=='F') #2034
sankey_0_20_female_id_unique=unique(sankey_0_20_female$asheville.MBR_PTY_ID) #742
x <- 0
for(i in 743:2034 ){
  sankey_0_20_female_id_unique[i] <- x
}

sankey_0_20_female$uniqueid=sankey_0_20_female_id_unique

x <- list()
for(i in 1:742) {
  
  da1=c(sankey_0_20_female[sankey_0_20_female$asheville.MBR_PTY_ID == sankey_0_20_female$uniqueid[i],4])
  x[[i]] <- da1
  
}
rules_b_0_20_female <- apriori(x, parameter = list(supp = 0.01, conf = 0.01, target = "rules"))
outcome_0_20_female=as(rules_b_0_20_female,'data.frame')
DT::datatable(outcome_0_20_female)
install.packages('arulesViz')
library(arulesViz)
plot(rules_b_female,interactive=TRUE)


#20_40
sankey_20_40_female=subset(sankey_20_40,asheville.GDR_TYP_CD=='F') #10651
sankey_20_40_female_id_unique=unique(sankey_20_40_female$asheville.MBR_PTY_ID) #2687
x <- 0
for(i in 2688:10651 ){
  sankey_20_40_female_id_unique[i] <- x
}

sankey_20_40_female$uniqueid=sankey_20_40_female_id_unique

x <- list()
for(i in 1:2687) {
  
  da1=c(sankey_20_40_female[sankey_20_40_female$asheville.MBR_PTY_ID == sankey_20_40_female$uniqueid[i],4])
  x[[i]] <- da1
  
}
rules_b_20_40_female <- apriori(x, parameter = list(supp = 0.01, conf = 0.01, target = "rules"))
outcome_20_40_female=as(rules_b_20_40_female,'data.frame')
DT::datatable(outcome_20_40_female)
install.packages('arulesViz')
library(arulesViz)
plot(rules_b_female,interactive=TRUE)


#40_60
sankey_40_60_female=subset(sankey_40_60,asheville.GDR_TYP_CD=='F') #16357
sankey_40_60_female_id_unique=unique(sankey_40_60_female$asheville.MBR_PTY_ID) #2910
x <- 0
for(i in 2911:16357 ){
  sankey_40_60_female_id_unique[i] <- x
}

sankey_40_60_female$uniqueid=sankey_40_60_female_id_unique

x <- list()
for(i in 1:2910) {
  
  da1=c(sankey_40_60_female[sankey_40_60_female$asheville.MBR_PTY_ID == sankey_40_60_female$uniqueid[i],4])
  x[[i]] <- da1
  
}
rules_b_40_60_female <- apriori(x, parameter = list(supp = 0.01, conf = 0.01, target = "rules"))
outcome_40_60_female=as(rules_b_40_60_female,'data.frame')
DT::datatable(outcome_40_60_female)
install.packages('arulesViz')
library(arulesViz)
plot(rules_b_female,interactive=TRUE)


#60_80
sankey_60_80_female=subset(sankey_60_80,asheville.GDR_TYP_CD=='F') #13578
sankey_60_80_female_id_unique=unique(sankey_60_80_female$asheville.MBR_PTY_ID) #1509
x <- 0
for(i in 1510:13578 ){
  sankey_60_80_female_id_unique[i] <- x
}

sankey_60_80_female$uniqueid=sankey_60_80_female_id_unique

x <- list()
for(i in 1:1509) {
  
  da1=c(sankey_60_80_female[sankey_60_80_female$asheville.MBR_PTY_ID == sankey_60_80_female$uniqueid[i],4])
  x[[i]] <- da1
  
}
rules_b_60_80_female <- apriori(x, parameter = list(supp = 0.05, conf = 0.05, target = "rules"))
outcome_60_80_female=as(rules_b_60_80_female,'data.frame')
DT::datatable(outcome_60_80_female)
install.packages('arulesViz')
library(arulesViz)
plot(rules_b_female,interactive=TRUE)



#80_110
sankey_80_110_female=subset(sankey_80_110,asheville.GDR_TYP_CD=='F') #6066
sankey_80_110_female_id_unique=unique(sankey_80_110_female$asheville.MBR_PTY_ID) #300
x <- 0
for(i in 301:6066 ){
  sankey_80_110_female_id_unique[i] <- x
}

sankey_80_110_female$uniqueid=sankey_80_110_female_id_unique

x <- list()
for(i in 1:300) {
  
  da1=c(sankey_80_110_female[sankey_80_110_female$asheville.MBR_PTY_ID == sankey_80_110_female$uniqueid[i],4])
  x[[i]] <- da1
  
}
rules_b_80_110_female <- apriori(x, parameter = list(supp = 0.05, conf = 0.05, target = "rules"))
outcome_80_110_female=as(rules_b_80_110_female,'data.frame')
DT::datatable(outcome_80_110_female)
install.packages('arulesViz')
library(arulesViz)
plot(rules_b_female,interactive=TRUE)










#male
sankey_0_20_male=subset(sankey_0_20,asheville.GDR_TYP_CD=='M') #2265
sankey_0_20_male_id_unique=unique(sankey_0_20_male$asheville.MBR_PTY_ID) #807
x <- 0
for(i in 808:2265 ){
  sankey_0_20_male_id_unique[i] <- x
}

sankey_0_20_male$uniqueid=sankey_0_20_male_id_unique

x <- list()
for(i in 1:807) {
  
  da1=c(sankey_0_20_male[sankey_0_20_male$asheville.MBR_PTY_ID == sankey_0_20_male$uniqueid[i],4])
  x[[i]] <- da1
  
}
rules_b_0_20_male <- apriori(x, parameter = list(supp = 0.01, conf = 0.01, target = "rules"))
outcome_0_20_male=as(rules_b_0_20_male,'data.frame')
DT::datatable(outcome_0_20_male)
install.packages('arulesViz')
library(arulesViz)
plot(rules_b_female,interactive=TRUE)


#20_40
sankey_20_40_male=subset(sankey_20_40,asheville.GDR_TYP_CD=='M') #5746
sankey_20_40_male_id_unique=unique(sankey_20_40_male$asheville.MBR_PTY_ID) #1771
x <- 0
for(i in 1772:5746 ){
  sankey_20_40_male_id_unique[i] <- x
}

sankey_20_40_male$uniqueid=sankey_20_40_male_id_unique

x <- list()
for(i in 1:1771) {
  
  da1=c(sankey_20_40_male[sankey_20_40_male$asheville.MBR_PTY_ID == sankey_20_40_male$uniqueid[i],4])
  x[[i]] <- da1
  
}
rules_b_20_40_male <- apriori(x, parameter = list(supp = 0.01, conf = 0.01, target = "rules"))
outcome_20_40_male=as(rules_b_20_40_male,'data.frame')
DT::datatable(outcome_20_40_male)
install.packages('arulesViz')
library(arulesViz)
plot(rules_b_female,interactive=TRUE)


#40_60
sankey_40_60_male=subset(sankey_40_60,asheville.GDR_TYP_CD=='M') #12738
sankey_40_60_male_id_unique=unique(sankey_40_60_male$asheville.MBR_PTY_ID) #2320
x <- 0
for(i in 2321:12738 ){
  sankey_40_60_male_id_unique[i] <- x
}

sankey_40_60_male$uniqueid=sankey_40_60_male_id_unique

x <- list()
for(i in 1:2320) {
  
  da1=c(sankey_40_60_male[sankey_40_60_male$asheville.MBR_PTY_ID == sankey_40_60_male$uniqueid[i],4])
  x[[i]] <- da1
  
}
rules_b_40_60_male <- apriori(x, parameter = list(supp = 0.01, conf = 0.01, target = "rules"))
outcome_40_60_female=as(rules_b_40_60_male,'data.frame')
DT::datatable(outcome_40_60_male)
install.packages('arulesViz')
library(arulesViz)
plot(rules_b_female,interactive=TRUE)


#60_80
sankey_60_80_male=subset(sankey_60_80,asheville.GDR_TYP_CD=='M') #10516
sankey_60_80_male_id_unique=unique(sankey_60_80_male$asheville.MBR_PTY_ID) #1182
x <- 0
for(i in 1183:10516 ){
  sankey_60_80_male_id_unique[i] <- x
}

sankey_60_80_male$uniqueid=sankey_60_80_male_id_unique

x <- list()
for(i in 1:1182) {
  
  da1=c(sankey_60_80_male[sankey_60_80_male$asheville.MBR_PTY_ID == sankey_60_80_male$uniqueid[i],4])
  x[[i]] <- da1
  
}
rules_b_60_80_male <- apriori(x, parameter = list(supp = 0.05, conf = 0.05, target = "rules"))
outcome_60_80_male=as(rules_b_60_80_male,'data.frame')
DT::datatable(outcome_60_80_male)
install.packages('arulesViz')
library(arulesViz)
plot(rules_b_female,interactive=TRUE)



#80_110
sankey_80_110_male=subset(sankey_80_110,asheville.GDR_TYP_CD=='M') #3853
sankey_80_110_male_id_unique=unique(sankey_80_110_male$asheville.MBR_PTY_ID) #181
x <- 0
for(i in 182:3853 ){
  sankey_80_110_male_id_unique[i] <- x
}

sankey_80_110_male$uniqueid=sankey_80_110_male_id_unique

x <- list()
for(i in 1:181) {
  
  da1=c(sankey_80_110_male[sankey_80_110_male$asheville.MBR_PTY_ID == sankey_80_110_male$uniqueid[i],4])
  x[[i]] <- da1
  
}
rules_b_80_110_male <- apriori(x, parameter = list(supp = 0.05, conf = 0.05, target = "rules"))
outcome_80_110_male=as(rules_b_80_110_male,'data.frame')
DT::datatable(outcome_80_110_male)
install.packages('arulesViz')
library(arulesViz)
plot(rules_b_female,interactive=TRUE)



###################################################################

