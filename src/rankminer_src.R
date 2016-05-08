setwd("D:/Projects/RankMiner Predictive_Analytics/data")
agent <- read.csv('project_agent_data.csv',header = TRUE, stringsAsFactors = FALSE)
call <- read.csv('project_call_data.csv', header = TRUE, stringsAsFactors = FALSE)
feature <- read.csv('project_feature_data.csv',header =TRUE, stringsAsFactors = FALSE)
attach(agent)
attach(call)
attach(feature)
str(agent)
str(call)
str(feature)

agent[agent == ""] <- NA
call[call == ""] <- NA
feature[feature == ""] <- NA

agent$payroll_id_src1[is.na(agent$payroll_id_src1)] <- agent$payroll_id_src2[is.na(agent$payroll_id_src1)]
agent$payroll_id_src2[is.na(agent$payroll_id_src2)] <- agent$payroll_id_src1[is.na(agent$payroll_id_src2)]

agent$hire_date_src1[is.na(agent$hire_date_src1)] <- agent$hire_date_src2[is.na(agent$hire_date_src1)]
agent$hire_date_src2[is.na(agent$hire_date_src2)] <- agent$hire_date_src1[is.na(agent$hire_date_src2)]

agent$term_date_src1[is.na(agent$term_date_src2)] <- agent$term_date_src1[is.na(agent$term_date_src2)]
agent$term_date_src1[is.na(agent$term_date_src1)] <- agent$term_date_src2[is.na(agent$term_date_src1)]

agent$work_shift_src1[is.na(agent$work_shift_src1)] <- agent$work_shift_src2[is.na(agent$work_shift_src1)]
agent$work_shift_src2[is.na(agent$work_shift_src2)] <- agent$work_shift_src1[is.na(agent$work_shift_src2)]

# Check if src1 and src2 are different
agent$payroll_id_src1[agent$payroll_id_src1!=agent$payroll_id_src2]
agent$payroll_id_src2[agent$payroll_id_src1!=agent$payroll_id_src2]

agent$hire_date_src1[agent$hire_date_src1!=agent$hire_date_src2 ]
agent$hire_date_src2[agent$hire_date_src1!=agent$hire_date_src2]

agent$term_date_src1[agent$term_date_src1!=agent$term_date_src2 & !is.na(agent$term_date_src1) & !is.na(agent$term_date_src2)]
agent$term_date_src2[agent$term_date_src1!=agent$term_date_src2  & !is.na(agent$term_date_src1) & !is.na(agent$term_date_src2)]

agent$work_shift_src1[agent$work_shift_src1!=agent$work_shift_src2 & !is.na(agent$work_shift_src1) & !is.na(agent$work_shift_src2)]
agent$work_shift_src2[agent$work_shift_src1!=agent$work_shift_src2 & !is.na(agent$work_shift_src1) & !is.na(agent$work_shift_src2)]

length(agent$work_shift_src2[is.na(agent$work_shift_src2)])

#Delete src2
agent$payroll_id_src2 = NULL
agent$hire_date_src2 = NULL
agent$term_date_src2 = NULL
agent$work_shift_src2 = NULL

#agent - Setting all characters to uppercase

agent = data.frame(lapply(agent, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))

#call - Setting all characters to uppercase
call = data.frame(lapply(call, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))


names(agent)[names(agent)=="payroll_id_src1"] <- "payroll_id"
names(agent)[names(agent)=="hire_date_src1"] <- "hire_date"
names(agent)[names(agent)=="term_date_src1"] <- "term_date"
names(agent)[names(agent)=="work_shift_src1"] <- "work_shift"
names(agent)[names(agent)=="group_src1"] <- "group"

agent$terminated <- 0
agent$terminated[!is.na(agent$term_date) | !is.na(agent$term_code) | !is.na(agent$term_type) | !is.na(agent$term_reason)] <- 1
length(agent$terminated[agent$terminated ==1])

#analysis
table(agent$terminated)
table(agent$terminated,agent$term_type)
table(agent$terminated,agent$term_reason)
round(prop.table(table(agent$terminated[agent$terminated==1], agent$term_type[agent$terminated==1]), 1)*100)


agent_unique_l <- lapply(agent[,c("group","work_shift","term_code","term_type","term_reason","Jul_group","Aug_group","Sep_group","Oct_group","Nov_group","Dec_group")], unique)
agent_unique_max.ln <- max(sapply(agent_unique_l, length))
agent_unique_l <- lapply(agent_unique_l, function(v) { c(v, rep(NA, agent_unique_max.ln-length(v)))})
agent_unique <- as.data.frame(agent_unique_l)
agent_unique <- apply(agent_unique,2,sort,decreasing=F)

agent_unique_l[1]

names(call)
names(call)[names(call)=="ACCOUNT"] <- "account"
names(call)[names(call)=="AUDIO.FILE.NAME"] <- "audio_file_name"
names(call)[names(call)=="SKILL.NAME"] <- "skill_name"
names(call)[names(call)=="CALL.START.TIME"] <- "call_start"
names(call)[names(call)=="CALL.END.TIME"] <- "call_end"
names(call)[names(call)=="AGENT.ID"] <- "agent_id"
names(call)[names(call)=="CALL.DIRECTION"] <- "call_direction"
names(call)[names(call)=="CALL.DURATION.HMS"] <- "call_duration"
names(call)[names(call)=="FILESIZE.KB"] <- "audio_file_size"
names(call)[names(call)=="REC.STATUS"] <- "call_end_status"

call_unique_l <- apply(call[,c("skill_name","call_direction","call_end_status")],2,sort,decreasing=F)
call_unique_l <- lapply(call_unique_l, unique)
call_unique_max.ln <- max(sapply(call_unique_l, length))
call_unique_l <- lapply(call_unique_l, function(v) { c(v, rep(NA, call_unique_max.ln-length(v)))})
call_unique <- as.data.frame(call_unique_l)

#Some outlier - 102474 to be removed from Dec_Group
agent[agent$Dec_group==102474]=""

#Converting NA's in term_type to UNKNOWN for reasons unknown
#agent$term_type[which(!is.na(agent$term_date) & is.na(agent$term_type))]="UN"

#Tenure
agent$tenure <- NA
agent$tenure <- as.Date(agent$term_date) - as.Date(agent$hire_date)

#Termination Month
agent$term_month <- NA
agent$term_month <- format(as.Date(agent$term_date), "%m")

#Hire Month
agent$hire_month <- NA
agent$hire_month <- ifelse(as.numeric(format(as.Date(agent$hire_date), "%Y")) == 2015, as.numeric(format(as.Date(agent$hire_date), "%m")),as.numeric(7))


#Checking whether any person has a term date missing but term_type,code or reason present.
agent$term_code[is.na(agent$term_date)]
agent$term_type[is.na(agent$term_date)]
agent$term_reason[is.na(agent$term_date)]

#User groups with #N/A values converted to Other
agent$Jul_group[which(agent$Jul_group=="#N/A")] = "OTHER"
agent$Aug_group[which(agent$Aug_group=="#N/A")] = "OTHER"
agent$Sep_group[which(agent$Sep_group=="#N/A")] = "OTHER"
agent$Oct_group[which(agent$Oct_group=="#N/A")] = "OTHER"
agent$Nov_group[which(agent$Nov_group=="#N/A")] = "OTHER"
agent$Dec_group[which(agent$Dec_group=="#N/A")] = "OTHER"

agent$usergroup = NULL

#Consolidating.. 
unique(agent$Jul_group)



#-------------Data Cleaning--------------

agent$Jul_group = as.character(agent$Jul_group)
unique(agent$Jul_group)
agent$Jul_group[which(agent$Jul_group=="INB DSH")]= "DISH"
agent$Jul_group[which(agent$Jul_group=="DSH")]= "DISH" 
agent$Jul_group[which(agent$Jul_group=="PTM")]= "TMOBILE"
agent$Jul_group[which(agent$Jul_group=="VZ")]= "VERIZON"  
agent$Jul_group[which(agent$Jul_group=="INB SPR")]= "SPRINT"  
agent$Jul_group[which(agent$Jul_group=="SPR")]= "SPRINT"  
agent$Jul_group[which(agent$Jul_group=="G1A")]= "CREDIT CARD"  
agent$Jul_group[which(agent$Jul_group=="G3A")]= "TELECOM"  
agent$Jul_group[which(agent$Jul_group=="DTV")]= "DIRECTV"    
agent$Jul_group[which(agent$Jul_group=="COM")]= "COMMERCIAL"    
agent$Jul_group[which(agent$Jul_group=="G 13 LEGAL")]= "LEGAL"  
agent$Jul_group[which(agent$Jul_group=="INB A")]= "INBOUND"    
agent$Jul_group[which(agent$Jul_group=="INB B")]= "INBOUND"   

#replace(agent$Jul_group, agent$Jul_group %in% c("INB DSH","DSH") , "DISH")

agent$Aug_group = as.character(agent$Aug_group)
unique(agent$Aug_group)
agent$Aug_group[which(agent$Aug_group=="INB DSH")]= "DISH"
agent$Aug_group[which(agent$Aug_group=="DSH")]= "DISH" 
agent$Aug_group[which(agent$Aug_group=="PTM")]= "TMOBILE"
agent$Aug_group[which(agent$Aug_group=="VZ")]= "VERIZON"  
agent$Aug_group[which(agent$Aug_group=="INB SPR")]= "SPRINT"  
agent$Aug_group[which(agent$Aug_group=="SPR")]= "SPRINT"  
agent$Aug_group[which(agent$Aug_group=="G1A")]= "CREDIT CARD"  
agent$Aug_group[which(agent$Aug_group=="G3A")]= "TELECOM"  
agent$Aug_group[which(agent$Aug_group=="DTV")]= "DIRECTV"    
agent$Aug_group[which(agent$Aug_group=="COM")]= "COMMERCIAL"    
agent$Aug_group[which(agent$Aug_group=="G 13 LEGAL")]= "LEGAL"  
agent$Aug_group[which(agent$Aug_group=="INB A")]= "INBOUND"  
agent$Aug_group[which(agent$Aug_group=="INB B")]= "INBOUND"  

agent$Sep_group = as.character(agent$Sep_group)
unique(agent$Sep_group)
agent$Sep_group[which(agent$Sep_group=="INB DSH")]= "DISH"
agent$Sep_group[which(agent$Sep_group=="DSH")]= "DISH" 
agent$Sep_group[which(agent$Sep_group=="PTM")]= "TMOBILE"
agent$Sep_group[which(agent$Sep_group=="VZ")]= "VERIZON"  
agent$Sep_group[which(agent$Sep_group=="INB SPR")]= "SPRINT"  
agent$Sep_group[which(agent$Sep_group=="SPR")]= "SPRINT"  
agent$Sep_group[which(agent$Sep_group=="G1A")]= "CREDIT CARD"  
agent$Sep_group[which(agent$Sep_group=="G3A")]= "TELECOM"  
agent$Sep_group[which(agent$Sep_group=="DTV")]= "DIRECTV"    
agent$Sep_group[which(agent$Sep_group=="COM")]= "COMMERCIAL"    
agent$Sep_group[which(agent$Sep_group=="G2A")]= "AUTO"  
agent$Sep_group[which(agent$Sep_group=="INB SPAN")]= "INB A"
agent$Sep_group[which(agent$Sep_group=="INB A")]= "INBOUND"
agent$Sep_group[which(agent$Sep_group=="INB B")]= "INBOUND"

agent$Oct_group = as.character(agent$Oct_group)
unique(agent$Oct_group)
agent$Oct_group[which(agent$Oct_group=="INB DSH")]= "DISH"
agent$Oct_group[which(agent$Oct_group=="DSH")]= "DISH" 
agent$Oct_group[which(agent$Oct_group=="PTM")]= "TMOBILE"
agent$Oct_group[which(agent$Oct_group=="VZ")]= "VERIZON"  
agent$Oct_group[which(agent$Oct_group=="INB SPR")]= "SPRINT"  
agent$Oct_group[which(agent$Oct_group=="SPR")]= "SPRINT"  
agent$Oct_group[which(agent$Oct_group=="G1A")]= "CREDIT CARD"  
agent$Oct_group[which(agent$Oct_group=="G3A")]= "TELECOM"  
agent$Oct_group[which(agent$Oct_group=="DTV")]= "DIRECTV"    
agent$Oct_group[which(agent$Oct_group=="COM")]= "COMMERCIAL"    
agent$Oct_group[which(agent$Oct_group=="INB SPAN")]= "INB A"
agent$Oct_group[which(agent$Oct_group=="INB A")]= "INBOUND"
agent$Oct_group[which(agent$Oct_group=="INB B")]= "INBOUND"

agent$Nov_group = as.character(agent$Nov_group)
unique(agent$Nov_group)
agent$Nov_group[which(agent$Nov_group=="INB DSH")]= "DISH"
agent$Nov_group[which(agent$Nov_group=="DSH")]= "DISH" 
agent$Nov_group[which(agent$Nov_group=="PTM")]= "TMOBILE"
agent$Nov_group[which(agent$Nov_group=="VZ")]= "VERIZON"  
agent$Nov_group[which(agent$Nov_group=="INB SPR")]= "SPRINT"  
agent$Nov_group[which(agent$Nov_group=="SPR")]= "SPRINT"  
agent$Nov_group[which(agent$Nov_group=="G1A")]= "CREDIT CARD"  
agent$Nov_group[which(agent$Nov_group=="G3A")]= "TELECOM"  
agent$Nov_group[which(agent$Nov_group=="DTV")]= "DIRECTV"    
agent$Nov_group[which(agent$Nov_group=="COM")]= "COMMERCIAL"    
agent$Nov_group[which(agent$Nov_group=="INB SPAN")]= "INB A"
agent$Nov_group[which(agent$Nov_group=="INB A")]= "INBOUND"
agent$Nov_group[which(agent$Nov_group=="INB B")]= "INBOUND"

agent$Dec_group = as.character(agent$Dec_group)
unique(agent$Dec_group)
agent$Dec_group[which(agent$Dec_group=="INB DSH")]= "DISH"
agent$Dec_group[which(agent$Dec_group=="DSH")]= "DISH" 
agent$Dec_group[which(agent$Dec_group=="PTM")]= "TMOBILE"
agent$Dec_group[which(agent$Dec_group=="VZ")]= "VERIZON"  
agent$Dec_group[which(agent$Dec_group=="INB SPR")]= "SPRINT"  
agent$Dec_group[which(agent$Dec_group=="SPR")]= "SPRINT"  
agent$Dec_group[which(agent$Dec_group=="G1A")]= "CREDIT CARD"  
agent$Dec_group[which(agent$Dec_group=="G3A")]= "TELECOM"  
agent$Dec_group[which(agent$Dec_group=="DTV")]= "DIRECTV"    
agent$Dec_group[which(agent$Dec_group=="COM")]= "COMMERCIAL"    
agent$Dec_group[which(agent$Dec_group=="INB SPAN")]= "INB A"  
agent$Dec_group[which(agent$Dec_group=="INB A")]= "INBOUND"
agent$Dec_group[which(agent$Dec_group=="INB B")]= "INBOUND"

agent["JulyFunctionalGroup"] <- NA

agent$JulyFunctionalGroup[which(agent$Jul_group=="TMOBILE")]= "CELL CARRIER" 
agent$JulyFunctionalGroup[which(agent$Jul_group=="SPRINT")]= "CELL CARRIER"
agent$JulyFunctionalGroup[which(agent$Jul_group=="VERIZON")]= "CELL CARRIER"
agent$JulyFunctionalGroup[which(agent$Jul_group=="TELECOM")]= "CELL CARRIER"
agent$JulyFunctionalGroup[which(agent$Jul_group=="ATT")]= "CELL CARRIER"
agent$JulyFunctionalGroup[which(agent$Jul_group=="DISH")]= "TV PROVIDER"
agent$JulyFunctionalGroup[which(agent$Jul_group=="DIRECTV")]= "TV PROVIDER"
agent$JulyFunctionalGroup[which(agent$Jul_group=="LEGAL")]= "LEGAL"
agent$JulyFunctionalGroup[which(agent$Jul_group=="COMMERCIAL")]= "COMMERCIAL"
agent$JulyFunctionalGroup[which(agent$Jul_group=="AUTO")]= "AUTO"
agent$JulyFunctionalGroup[which(agent$Jul_group=="CREDIT CARD")]= "CREDIT CARD"
agent$JulyFunctionalGroup[which(agent$Jul_group=="INB A")]= "INBOUND"
agent$JulyFunctionalGroup[which(agent$Jul_group=="INB B")]= "INBOUND"

agent["AugFunctionalGroup"] <- NA
agent$AugFunctionalGroup[which(agent$Aug_group=="TMOBILE")]= "CELL CARRIER" 
agent$AugFunctionalGroup[which(agent$Aug_group=="SPRINT")]= "CELL CARRIER"
agent$AugFunctionalGroup[which(agent$Aug_group=="VERIZON")]= "CELL CARRIER"
agent$AugFunctionalGroup[which(agent$Aug_group=="TELECOM")]= "CELL CARRIER"
agent$AugFunctionalGroup[which(agent$Aug_group=="ATT")]= "CELL CARRIER"
agent$AugFunctionalGroup[which(agent$Aug_group=="DISH")]= "TV PROVIDER"
agent$AugFunctionalGroup[which(agent$Aug_group=="DIRECTV")]= "TV PROVIDER"
agent$AugFunctionalGroup[which(agent$Aug_group=="LEGAL")]= "LEGAL"
agent$AugFunctionalGroup[which(agent$Aug_group=="COMMERCIAL")]= "COMMERCIAL"
agent$AugFunctionalGroup[which(agent$Aug_group=="AUTO")]= "AUTO"
agent$AugFunctionalGroup[which(agent$Aug_group=="CREDIT CARD")]= "CREDIT CARD"
agent$AugFunctionalGroup[which(agent$Aug_group=="INB A")]= "INBOUND"
agent$AugFunctionalGroup[which(agent$Aug_group=="INB B")]= "INBOUND"

agent["SeptFunctionalGroup"] <- NA
agent$SeptFunctionalGroup[which(agent$Sep_group=="TMOBILE")]= "CELL CARRIER" 
agent$SeptFunctionalGroup[which(agent$Sep_group=="SPRINT")]= "CELL CARRIER"
agent$SeptFunctionalGroup[which(agent$Sep_group=="VERIZON")]= "CELL CARRIER"
agent$SeptFunctionalGroup[which(agent$Sep_group=="TELECOM")]= "CELL CARRIER"
agent$SeptFunctionalGroup[which(agent$Sep_group=="ATT")]= "CELL CARRIER"
agent$SeptFunctionalGroup[which(agent$Sep_group=="DISH")]= "TV PROVIDER"
agent$SeptFunctionalGroup[which(agent$Sep_group=="DIRECTV")]= "TV PROVIDER"
agent$SeptFunctionalGroup[which(agent$Sep_group=="LEGAL")]= "LEGAL"
agent$SeptFunctionalGroup[which(agent$Sep_group=="COMMERCIAL")]= "COMMERCIAL"
agent$SeptFunctionalGroup[which(agent$Sep_group=="AUTO")]= "AUTO"
agent$SeptFunctionalGroup[which(agent$Sep_group=="CREDIT CARD")]= "CREDIT CARD"
agent$SeptFunctionalGroup[which(agent$Sep_group=="INB A")]= "INBOUND"
agent$SeptFunctionalGroup[which(agent$Sep_group=="INB B")]= "INBOUND"

agent["OctFunctionalGroup"] <- NA
agent$OctFunctionalGroup[which(agent$Oct_group=="TMOBILE")]= "CELL CARRIER" 
agent$OctFunctionalGroup[which(agent$Oct_group=="SPRINT")]= "CELL CARRIER"
agent$OctFunctionalGroup[which(agent$Oct_group=="VERIZON")]= "CELL CARRIER"
agent$OctFunctionalGroup[which(agent$Oct_group=="TELECOM")]= "CELL CARRIER"
agent$OctFunctionalGroup[which(agent$Oct_group=="ATT")]= "CELL CARRIER"
agent$OctFunctionalGroup[which(agent$Oct_group=="DISH")]= "TV PROVIDER"
agent$OctFunctionalGroup[which(agent$Oct_group=="DIRECTV")]= "TV PROVIDER"
agent$OctFunctionalGroup[which(agent$Oct_group=="LEGAL")]= "LEGAL"
agent$OctFunctionalGroup[which(agent$Oct_group=="COMMERCIAL")]= "COMMERCIAL"
agent$OctFunctionalGroup[which(agent$Oct_group=="AUTO")]= "AUTO"
agent$OctFunctionalGroup[which(agent$Oct_group=="CREDIT CARD")]= "CREDIT CARD"
agent$OctFunctionalGroup[which(agent$Oct_group=="INB A")]= "INBOUND"
agent$OctFunctionalGroup[which(agent$Oct_group=="INB B")]= "INBOUND"

agent["NovFunctionalGroup"] <- NA
agent$NovFunctionalGroup[which(agent$Nov_group=="TMOBILE")]= "CELL CARRIER" 
agent$NovFunctionalGroup[which(agent$Nov_group=="SPRINT")]= "CELL CARRIER"
agent$NovFunctionalGroup[which(agent$Nov_group=="VERIZON")]= "CELL CARRIER"
agent$NovFunctionalGroup[which(agent$Nov_group=="TELECOM")]= "CELL CARRIER"
agent$NovFunctionalGroup[which(agent$Nov_group=="ATT")]= "CELL CARRIER"
agent$NovFunctionalGroup[which(agent$Nov_group=="DISH")]= "TV PROVIDER"
agent$NovFunctionalGroup[which(agent$Nov_group=="DIRECTV")]= "TV PROVIDER"

#Creating List of User Groups
for(i in 1:nrow(agent)){
  agent$usergroup[i]=list(c(as.character(agent$Jul_group[i]),as.character(agent$Aug_group[i]),as.character(agent$Sep_group[i]),as.character(agent$Oct_group[i]),as.character(agent$Nov_group[i]),as.character(agent$Dec_group[i])))
}

#agent$usergroup = NULL

length(agent$usergroup)
View(agent$usergroup)


agent$grpchange = NULL

#How many groups has the user changed?
for(i in 1:nrow(agent)) {
  if(NA %in% agent$usergroup[[i]]){
    agent$grpchange[i]=length(unique(agent$usergroup[[i]]))-1
  }
  else {  agent$grpchange[i]=length(unique(agent$usergroup[[i]]))}
}

length(unique(agent$usergroup[]))


#Last Group the agent worked under 
tmp = 0
for(i in 1:298)
{
  tmp = agent$term_month[i]-6
  print(tmp)
  if(!is.na(tmp) & tmp == 0){tmp = 1}
  
  if(is.na(tmp)){
    agent$lastgrp[i]= NA
  }
  else{  agent$lastgrp[i] = agent$usergroup[[i]][[tmp]] }
}

#Fixing group_src1 values
old_values = c("INB DSH" ,"DSH","PTM","VZ","INB SPR","SPR","G1A","G3A","DTV","COM","INB SPAN","INB A","INB B")
new_values = c("DISH","DISH","TMOBILE","VERIZON","SPRINT","SPRINT","CREDIT CARD","TELECOM","DIRECTV","COMMERCIAL","INBOUND","INBOUND","INBOUND")

grp_old = as.character(unique(agent$group))
grp_old = grp_old[!is.na(grp_old)]

grp_new = c("TELECOM","CREDIT CARD","VERIZON","TMOBILE","SPRINT","SPRINT","INBOUND","DISH","DISH","TELECOM","DIRECTV","INBOUND","AUTO","FM","COMMERICAL","LEGAL","TELECOM","FM","TELECOM","UNKNOWN")


lapply(agent[,c("Jul_group")],function(v) {
  if(!is.na(v) & v=="G 18 ATT") { v = "CELL CARRIER"} }
) 


#-------------Data Preparation--------------
dataset1 <- agent

dataset1$payroll_id <- NULL 
dataset1$hire_date <- NULL
dataset1$term_date <- NULL
dataset1$group <- NULL
dataset1$term_code <- NULL
dataset1$term_reason <- NULL
dataset1$work_shift <- NULL


dataset1$nom <-0
dataset1$nom <- ifelse(!is.na(dataset1$Jul_group),dataset1$nom+1,dataset1$nom)
dataset1$nom <- ifelse(!is.na(dataset1$Aug_group),dataset1$nom+1,dataset1$nom)
dataset1$nom <- ifelse(!is.na(dataset1$Sep_group),dataset1$nom+1,dataset1$nom)
dataset1$nom <- ifelse(!is.na(dataset1$Oct_group),dataset1$nom+1,dataset1$nom)
dataset1$nom <- ifelse(!is.na(dataset1$Nov_group),dataset1$nom+1,dataset1$nom)
dataset1$nom <- ifelse(!is.na(dataset1$Dec_group),dataset1$nom+1,dataset1$nom)

#term_type
dataset1$term_type_bin <- NA
dataset1$term_type_bin[dataset1$term_type == "VOLUNTARY"] <- 1
dataset1$term_type_bin[dataset1$term_type == "INVOLUNTARY"] <- 0
dataset1$term_type <- NULL
names(dataset1)[names(dataset1)=="term_type_bin"] <- "term_type"


#productivity
dataset1[,"Dec_hrs_worked"] <- as.numeric(dataset1[,"Dec_hrs_worked"])
dataset1$productivity <- NA
dataset1$productivity <- rowMeans(dataset1[,c("Jul_hrs_worked","Aug_hrs_worked","Sep_hrs_worked","Oct_hrs_worked","Nov_hrs_worked","Dec_hrs_worked")],na.rm = TRUE)
dataset1$productivity <- dataset1$productivity/dataset1$nom


#rev_generated
dataset1[,"Oct_revenue_generated"] <- as.numeric(dataset1[,"Oct_revenue_generated"])
dataset1[,"Sep_revenue_generated"] <- as.numeric(dataset1[,"Sep_revenue_generated"])
dataset1$rev_generated <- NA
dataset1$rev_generated <- rowSums(dataset1[,c("Jul_revenue_generated","Aug_revenue_generated","Sep_revenue_generated","Oct_revenue_generated","Nov_revenue_generated","Dec_revenue_generated")],na.rm = TRUE)-rowSums(dataset1[,c("Jul_commission","Aug_commission","Sep_commission","Oct_commission","Nov_commission","Dec_commission")],na.rm = TRUE)
dataset1$rev_generated <- round(dataset1$rev_generated/dataset1$nom,3)

#commision
dataset1$commision <- 0
dataset1$commision <- ifelse(!is.na(dataset1$Jul_commission),dataset1$commision+1,dataset1$commision)
dataset1$commision <- ifelse(!is.na(dataset1$Aug_commission),dataset1$commision+1,dataset1$commision)
dataset1$commision <- ifelse(!is.na(dataset1$Sep_commission),dataset1$commision+1,dataset1$commision)
dataset1$commision <- ifelse(!is.na(dataset1$Oct_commission),dataset1$commision+1,dataset1$commision)
dataset1$commision <- ifelse(!is.na(dataset1$Nov_commission),dataset1$commision+1,dataset1$commision)
dataset1$commision <- ifelse(!is.na(dataset1$Dec_commission),dataset1$commision+1,dataset1$commision)
dataset1$commision <- dataset1$nom

#salary
dataset1$salary <- 0
dataset1$salary <- dataset1[,"Jul_hourly_rate"]*dataset1[,"Jul_hrs_worked"]+dataset1[,"Aug_hourly_rate"]*dataset1[,"Aug_hrs_worked"]+dataset1[,"Sep_hourly_rate"]*dataset1[,"Sep_hrs_worked"]+dataset1[,"Oct_hourly_rate"]*dataset1[,"Oct_hrs_worked"]+dataset1[,"Nov_hourly_rate"]*dataset1[,"Nov_hrs_worked"]+dataset1[,"Dec_hourly_rate"]*dataset1[,"Dec_hrs_worked"]

sum(2,4)


#median call duration
library(data.table)
cz <- tapply(call$CALL.DURATION.HMS., call$agent_id, median)
cz = data.frame(names(cz),cz)
names(cz) = c("agent_id","Call_Median_Duration")
agent2 = (merge(agent1,cz, by = "agent_id"))


#no of calls
cy <- data.table(call)
cy <- tapply(call$account, call$agent_id, length)
cy = data.frame(names(cy),cy)
names(cy) = c("agent_id","Total_CallData_Calls")
agent1 = (merge(agent,cy, by = "agent_id"))


#call exit status

#skill group difference

#feature
names(feature)
feature$CallSpeech <- rowMeans(feature[,3:26])
NE1 <- rowSums(feature[,25:34])
NE2 <- rowSums(feature[,45:54])
NE3 <- rowSums(feature[,65:74])
NE4 <- rowSums(feature[,85:94])
NE5 <- rowSums(feature[,105:114])
NE6 <- rowSums(feature[,125:134])
NE7 <- rowSums(feature[,145:154])
NETotal <- NE1 + NE2 + NE3 + NE4 + NE5 + NE6 + NE7
feature$NegativeEmotions <- NETotal/(70)
PE1 <- rowSums(feature[,35:44])
PE2 <- rowSums(feature[,55:64])
PE3 <- rowSums(feature[,75:84])
PE4 <- rowSums(feature[,95:104])
PE5 <- rowSums(feature[,115:124])
PE6 <- rowSums(feature[,135:144])
PE7 <- rowSums(feature[,155:164])
PETotal <- PE1 + PE2 + PE3 + PE4 + PE5 + PE6 + PE7
feature$PositiveEmotions <- PETotal/(70)
feature$PositiveNegativeProportion <- rowMeans(feature[, 165:176])

#target




str(dataset1)
dataset1$last_group <- NA


#Write
#write.csv(agent,"testing4.csv")