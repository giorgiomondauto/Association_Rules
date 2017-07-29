#asheville is a dataset related to the city of Asheville in North Carolina
asheville=read.csv('C:/Users/gmondaut/Desktop/asheville/asheville_chronic_disease.csv')
age=asheville$AGE
summary(age)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00   41.00   58.00   54.81   67.00  110.00 
age_groups=cut(age,breaks=c(0,20,40,60,80,100,120),include.highest=TRUE)
summary(age_groups)
#(0,20]   (20,40]   (40,60]   (60,80]  (80,100] (100,120]      NA's 
#75915    229079    431101    351984    139391      3634        47 
asheville['AGE_GROUPS']=age_groups

#mapping tobacco dimension
TOBACCO=asheville$TBCC_USE_IND
TOBACCO=mapvalues(TOBACCO,from=c('','?','T'),to=c('U','U','Y'))
asheville$TBCC_USE_IND=TOBACCO
asheville$GDR_TYP_CD=mapvalues(asheville$GDR_TYP_CD,from=c('U'),to=c('F'))

#tobacco = Smoker, icd code= code identifying disease. 
#Association Rules for sex_age_icd_tobacco
sex_age_icd_tobacco=data.frame(asheville$GDR_TYP_CD,asheville$AGE_GROUPS,asheville$ICDCode,asheville$TBCC_USE_IND)

#Performing Apriori Algorithm
data1=data.table(sex_age_icd_tobacco)
testdata1 <- as( data1, 'transactions' )
basket_rules1 <- apriori(testdata1,parameter = list(sup = 0.01, conf = 0.01, target = "rules"))
outcome1=as(basket_rules1,'data.frame')
plot(outcome1)
#to get interactive association rules table
DT::datatable(outcome1)

#plot association rules
library('arulesViz')
plot(basket_rules1, method="matrix", measure="lift")
plot(basket_rules1, method="matrix3D", measure="lift")
plot(basket_rules1, method="matrix", measure="lift", control=list(reorder=TRUE))
plot(basket_rules1, method="matrix", measure=c("lift", "confidence"),interactive=TRUE)
plot(basket_rules1, method="graph", control=list(type="itemsets"))

plot(basket_rules1, method="paracoord",interactive = TRUE)
saveAsGraph(head(sort(basket_rules1, by="lift"),1000), file="rules.graphml")


--------------------------------------------------
##Association Rules for icd_codes

uniqmem= read.csv('C:/Users/gmondaut/Desktop/asheville/icd_codes_association_rule.csv')
x=list()
uniqmem['icdcode']=as.character(uniqmem$asheville.ICDCode3) 

for(i in 1:14556) {
  var <- uniqmem$realUniqID[i]
  
  da1=uniqmem[uniqmem$asheville.MBR_PTY_ID ==  (var), 10]
  x[[i]] <- da1
  
}
#performing apriori algorithm
rules_b <- apriori(x, parameter = list(supp = 0.01, conf = 0.01, target = "rules"))
dataframe=as(rules_b,'data.frame')
DT::datatable(dataframe) 
# sort by lift
rules_b_sort=head(sort(rules_b, by="lift"), 20)
#plot association rules
plot(rules_b_sort, method="paracoord",interactive = TRUE)
plot(rules_b_sort,method='graph')

