setwd('D:/Projects/RankMiner Predictive_Analytics/data')
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
#agent_unique_max.ln <- max(sapply(agent_unique_l, length))
#agent_unique_l <- lapply(agent_unique_l, function(v) { c(v, rep(NA, agent_unique_max.ln-length(v)))})
#agent_unique <- as.data.frame(agent_unique_l)
#agent_unique <- apply(agent_unique,2,sort,decreasing=F)

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
agent$Dec_group = as.character(agent$Dec_group)
agent$Dec_group[agent$Dec_group==102474]=NA

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
agent[,c("Jul_group","Aug_group","Sep_group","Oct_group","Nov_group","Dec_group")] =
  sapply(agent[,c("Jul_group","Aug_group","Sep_group","Oct_group","Nov_group","Dec_group")],function(v){
    v = as.character(v)
    v[which(v=="#N/A")] = "OTHER"
    v
  })


#Consolidating.. 
#unique(agent$Jul_group)

##-------------Data Cleaning--------------
myreplace <- function(v,old,new){
  v= as.character(v)
  if(length(old)==length(new)){
    for(i in 1:length(old))
    {v[which(v==old[i])]= new[i]
    
    }
  }
  else{
    print("Check Old and New Values - they are unequal")
  }
  v
}

#Creating Vectors for new and old values to categorize the monthly group data
old_values = c("INB DSH" ,"DSH","PTM","VZ","INB SPR","SPR","G1A","G3A","DTV","COM","INB SPAN","INB A","INB B")
new_values = c("DISH","DISH","TMOBILE","VERIZON","SPRINT","SPRINT","CREDIT CARD","TELECOM","DIRECTV","COMMERCIAL","INBOUND","INBOUND","INBOUND")

agent[,c("Jul_group","Aug_group","Sep_group","Oct_group","Nov_group","Dec_group")]  =
  sapply(agent[,c("Jul_group","Aug_group","Sep_group","Oct_group","Nov_group","Dec_group")],myreplace,old=old_values,new=new_values)


#Introducing Functional Groupings

old_functional_group = c("TMOBILE","SPRINT","VERIZON","TELECOM","ATT","DISH","DIRECTV","LEGAL","COMMERCIAL","AUTO","CREDIT CARD","INBOUND")

new_functional_group = c("CELL CARRIER","CELL CARRIER","CELL CARRIER","CELL CARRIER","CELL CARRIER","TV PROVIDER","TV PROVIDER","LEGAL","COMMERCIAL","AUTO","CREDIT CARD","INBOUND")

agent[c("JulFunctionalGroup","AugFunctionalGroup","SepFunctionalGroup","OctFunctionalGroup","NovFunctionalGroup","DecFunctionalGroup")] <- NA
agent[,c("JulFunctionalGroup","AugFunctionalGroup","SepFunctionalGroup","OctFunctionalGroup","NovFunctionalGroup","DecFunctionalGroup")] =
    sapply(agent[,c("Jul_group","Aug_group","Sep_group","Oct_group","Nov_group","Dec_group")],myreplace,old=old_functional_group,new=new_functional_group)

###----------------------------------------------------------------------------
#Creating List of User Groups
###----------------------------------------------------------------------------
for(i in 1:nrow(agent)){
  agent$usergroup[i]=list(c(as.character(agent$Jul_group[i]),as.character(agent$Aug_group[i]),as.character(agent$Sep_group[i]),as.character(agent$Oct_group[i]),as.character(agent$Nov_group[i]),as.character(agent$Dec_group[i])))
}

###----------------------
#Group Changes
###------------------------
agent$grpchange = NA
for(i in 1:nrow(agent)) {
  if(NA %in% agent$usergroup[[i]]){
    agent$grpchange[i]=length(unique(agent$usergroup[[i]]))-1
  }
  else {  agent$grpchange[i]=length(unique(agent$usergroup[[i]]))}
}

###---------------------------
##Last Group the agent worked under 
###----------------------------

agent$lastgrp = NA
agent$lastgrp = sapply(agent$usergroup,function(v){i = max(which(!is.na(v)));v[i] })

###-----------------------
#Fixing grp_src_1 (group) values 
###-----------------------
#Fixing group_src1 values - introducing new_group
grp_old = as.character(unique(agent$group))
grp_old = grp_old[!is.na(grp_old)]
grp_old
grp_new = c("TELECOM","CREDIT CARD","VERIZON","TMOBILE","SPRINT","SPRINT","INBOUND","DISH","DISH","TELECOM","DIRECTV","INBOUND","AUTO","FM","COMMERICAL","LEGAL","TELECOM","FM","TELECOM","UNKNOWN")

agent$new_group <- NA
agent$new_group=sapply(agent$group,myreplace, old=grp_old,new=grp_new)

###------------------------
#Adding Synthetic Variables (Vishal)
###---------------------------
dataset1 <- agent

dataset1$payroll_id <- NULL 
dataset1$hire_date <- NULL
dataset1$term_date <- NULL
dataset1$group <- NULL
dataset1$term_code <- NULL
dataset1$term_reason <- NULL
dataset1$work_shift <- NULL

#Change This


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

#commision - Change This
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
names(call)

#median call duration
install.packages("data.table")
library(data.table)
cz <- tapply(call$CALL.DURATION.HMS.,call$agent_id, median)
cz = data.frame(names(cz),cz)
names(cz) = c("agent_id","Call_Median_Duration")
agent = (merge(x = agent,y = cz, by = "agent_id", all.x = TRUE))


#no of calls
cy <- data.table(call)
cy <- tapply(call$account, call$agent_id, length)
cy = data.frame(names(cy),cy)
names(cy) = c("agent_id","Total_CallData_Calls")
agent = (merge(x = agent,y = cy, by = "agent_id", all.x = TRUE))

#names(agent)

###----------------------------------------
#------------------------------------
#CALL DATA : REC-STATUS

new_statuses = c(0,0,1,0,0,1,1,1,0,1,1,1)


call$negoutcome <- 0
#Run the below command wisely, it takes time
call$negoutcome <- as.numeric(sapply(call$call_end_status,myreplace,old=unique(call$call_end_status),new=new_statuses))


#Calculating Number of Negative calls per agent.
agent$negativecalls = 0

#Agents Not in CALL DATASET - logical 
log_agent = agent$agent_id %in% call$agent_id

agent$negativecalls[log_agent] = tapply(call$negoutcome, call$agent_id, sum)

#-Check the above works fine  --- sum(call$negoutcome[call$agent_id == "ABR"])

#Calculating Ratio of Negative calls/total calls
agent$negativity = 0
agent$negativity = agent$negativecalls / agent$Total_CallData_Calls

###------------------------------------------------
#SKILL GROUPINGS: Comparing Skill Groups in Call data and Groups in Agent Data
###-----------------------------------------------

##Skill Groups
call$skillname <- NA
call$skill_name = as.character(call$skill_name)
call$skillname = substr(call$skill_name,0, (nchar(call$skill_name)-5))
#call$skillname[10:40]

old_skill_group = c("GENERAL",NA,"AUT","AT AND ","AT AND T ","DI","TMOBILE TIERTARY ","SPRI","GENERAL ","VERIZO","CREDIT CAR","DIRECT","T-MOBILE_FAMIL","SPRIN","TMOBILE TIERTA","TMOBIL","AT AND","SPRINT ","DIS","TMOBI","TMOBILE ","VERIZ","AU","VERIZON ","LEGA","CLOS","CREDIT CA","COMMERCIA","UTILI","TMOBILE TIERTAR","T-MOBILE_FAMI","JC","DIRECTT","COMMERCI","AT AND T B_M","GENERAL 2_M")
new_skill_group = c("OTHER",NA,"AUTO","ATT","ATT","DISH","TMOBILE","SPRINT","OTHER","VERIZON","CREDIT CARD","DIRECTTV","TMOBILE","SPRINT","TMOBILE","TMOBILE","ATT","SPRINT","DISH","TMOBILE","TMOBILE","VERIZON","AUTO","VERIZON","LEGAL","OTHER","CREDIT CARD","COMMERCIAL","OTHER","TMOBILE","TMOBILE","OTHER","DIRECTTV","COMMERCIAL","ATT","OTHER")
call[c("skillgroup")] <- NA
call[,c("skillgroup")] =sapply(call[,c("skillname")],myreplace,old=old_skill_group,new=new_skill_group)

#Creating List of Skill Groups

#library(plyr)
#install.packages('dplyr')
#library(dplyr)
#R2 <- ddply(call, "agent_id")
#data<-call[,c("agent_id","skillgroup")]
#data[is.na(data$skillgroup),"skillgroup"]<-"NA"

#R1 <- ddply(call[,c("agent_id","skillgroup")], "agent_id")
#R1[1:50,]


###----------------------------------------------------
#Creating Skill
###---------------------------------------------------
agent$skills = NA
agent$skills[log_agent] = tapply(call$skillgroup, call$agent_id, unique)

agent$skilloutofgroup = 0
agent$skilloutofgroup[log_agent] = mapply(function(v,w){subset(v,!(v %in% w)) },agent$skills[log_agent],agent$usergroup[log_agent])

agent$skillgroupdiff = 0
agent$skillgroupdiff = sapply(agent$skilloutofgroup,length)


####Ignore below for now
call$audio <- substr(call$audio_file_name,1,34)
feature$audio <- (substr(feature$AUDIO.FILE.NAME, 1,34))
e = call[which(call$audio %in% feature$audio),c(14,6)]
z = feature[which(feature$audio %in% call$audio),c(2,183)]
z = na.omit(Z)
target_agent = merge(E,Z, by.x = "audio", by.y = "audio")
d = data.frame(tapply(target_agent$target_value,list(target_agent$agent_id,target_agent$target_value),length))
d <- cbind(rownames(D), D)
rownames(D) <- NULL
colnames(D) <- c("agent_id","target_0","target_1")
agent$target_value = tapply(target_agent$target_value, target_agent$agent_id, unique)
agent_terminated_targetvalue = agent[,c(1,40,61)]

#####-----------

new_feature = feature[which(feature$audio %in% call$audio),]
write.csv(agent[-c(50,58,59)],"agentv1.csv")

names(new_feature)

###----
##
###-----------
library(caTools)



###---------------------------------------
#FEATURE DATASET
###-----------------------------------------
#feature

names(feature)

#feature_agg <- read.csv('project_feature_data_agg.csv',header = TRUE, stringsAsFactors = FALSE)
call$audio <- substr(call$audio_file_name,1,34)
feature$audio <- (substr(feature$AUDIO.FILE.NAME, 1,34))
e = call[which(call$audio %in% feature$audio),c(11,6)]
z = feature[which(feature$audio %in% call$audio),c(2,183)]
z = na.omit(z)
target_agent = merge(x = e,y = z, by.x = "audio", by.y = "audio")
d = data.frame(tapply(target_agent$target_value,list(target_agent$agent_id,target_agent$target_value),length))
d <- cbind(rownames(d), d)
rownames(d) <- NULL
colnames(d) <- c("agent_id","target_0","target_1")
f  = tapply(target_agent$target_value, target_agent$agent_id, unique)
f = data.frame(names(f),f)
names(f) = c("agent_id","target_value")
agent = (merge(x = agent,y = f, by = "agent_id", all.x = TRUE))






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



  
###-------------------------------------------------------------
#

y = feature[which(feature$audio %in% call$audio),]
new_feature = merge(x = e,y = y, by.x = "audio", by.y = "audio")

names(new_feature)
nrow(new_feature)

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

length(unique(feature_agg$target_value))

nrow(feature_agg_1)
colnames(feature_agg_1)
nrow(feature_agg)
feature_agg <- new_feature
feature_agg$AUDIO.FILE.NAME <- NULL
feature_agg$audio <- NULL
feature_agg$target_value[is.na(feature_agg$target_value)] <- 1 


agent$terminated[agent$terminated!=new_feature$target_value && agent$agent_id == feature_agg_1$agent_id]



feature_agg_1 <- aggregate(. ~ agent_id, data = feature_agg, FUN = mean)
feature_agg_2 <- aggregate(. ~ agent_id, data = feature_agg, FUN = mode)

log.feature_agg_1 <- log(feature_agg_1[,2:182])


feature_agg_1
feature_agg_1$target_value <- NA
feature_agg_1$target_value <- aggregate(feature_agg$target_value ~ agent_id, data = feature_agg_1, FUN = count)
nrow(feature_agg$target_value)


