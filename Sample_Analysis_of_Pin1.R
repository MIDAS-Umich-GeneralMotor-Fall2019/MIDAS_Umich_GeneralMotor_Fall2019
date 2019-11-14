library(tidyverse)

setwd("C:/Users/jueju/Desktop/seperate_all_info/Pin_all_info")
test_Pin1 = read.csv("Pin1_all_info.csv")

###############################################
######## Roundness Only as an example #########
###############################################

###The whole Pin1 - Roundness
round = test_Pin1 %>% select(grep("Roundness", names(test_Pin1)))

new_round = round[complete.cases(round), ]
kk = nrow(new_round)
c = new_round %>% mutate(num = 1:kk) %>% gather("operation", "value",-num) %>% 
  dplyr::mutate(num = as.factor(num))

#plot line (each observation has values for the selected operations)
ggplot(c, aes(x = operation, y = value, group = num, color = num)) + 
  geom_point() + geom_line() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plot boxplot (each observation has values for the selected operations)
ggplot(c, aes(x = operation, y = value))+ geom_boxplot(alpha = 0.5) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###Divide by two operations(120,240)
#120
round120 = round %>% select(grep("OP120", names(round)))

new_round120 = round120[complete.cases(round120), ]
k = nrow(new_round120)
c120 = new_round120 %>% mutate(num = 1:k) %>% gather("operation", "value",-num) %>% 
  dplyr::mutate(num = as.factor(num))

ggplot(c120, aes(x = operation, y = log(value)))+ geom_boxplot(alpha = 0.5) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #+
  #scale_y_continuous(breaks = seq(0, 1, by=0.25), limits = c(0,1))

#Remove outlier
unique(c120$operation)
cc120 = c120 %>% filter(operation == "OP120..2102.Roundness.P1.ctr")
outliers = boxplot(cc120$value)$out
!which(cc120$value %in% outliers)
test = cc120[-which(cc120$value %in% outliers),]
ggplot(test, aes(x = operation, y = log(value)))+ geom_boxplot(alpha = 0.5)

#240
round240 = round %>% select(grep("OP240", names(round)))

new_round240 = round240[complete.cases(round120), ]
k = nrow(new_round240)
c240 = new_round240 %>% mutate(num = 1:k) %>% gather("operation", "value",-num) %>% 
  dplyr::mutate(num = as.factor(num))

ggplot(c240, aes(x = operation, y = log(value)))+ geom_boxplot(alpha = 0.5) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(-7, -2, by=1), limits = c(-7,-2))

##################Detect Outlier#################
a = new_round
length(a)
names(a) = as.character(1:length(a))

c = a %>% mutate(num = 1:kk) %>% gather("operation", "value",-num) %>% 
  mutate(nn = as.numeric(operation), nnum = as.character(num)) 

library(olsrr)
model <- lm(value ~ nn, c)
ols_plot_resid_stud(model)

#################Compare Variance###################

pin1_roundness = test_Pin1%>%select(contains("Roundness"))
ratio <- c()
name <- c()
var_col <- c()

for(i in 1:length(pin1_roundness)){
  var_col[i] = var(pin1_roundness[i], na.rm = T)
  name[i] = names(pin1_roundness[i])
  
}

data <- as.data.frame(var_col)
for(i in 1: length(data$var_col)-1){
  ratio[i] = abs(data$var_col[i+1]-data$var_col[i])
  print(data$var_col[i+1])
}
#ratio = ratio[complete.cases(ratio)]
ratio

max = 0
operation = 0 
for(i in 1: length(ratio)){
  if(ratio[i] > 0){
    if(ratio[i] > max){
      max = ratio[i]
      operation = i
    }
  }
}
max
operation

names(pin1_roundness)[operation]


############ Compare variance after removing outliers ###########

pin1_roundness = test_Pin1%>%select(contains("Roundness"))

ratio <- c()
name <- c()
var_col <- c()

for(i in 1:length(pin1_roundness)){
  
  outliers = boxplot(pin1_roundness[i])$out
  which(as.vector(pin1_roundness[[i]]) %in% outliers)
  no_outlier = pin1_roundness[i][-which(pin1_roundness[[i]] %in% outliers),]
  
  var_col[i] = var(no_outlier,na.rm = T)
  name[i] = names(pin1_roundness[i])
  
}

data <- as.data.frame(var_col)
for(i in 1: length(data$var_col)-1){
  ratio[i] = abs(data$var_col[i+1]-data$var_col[i])
  print(data$var_col[i+1])
}

ratio

max = 0
operation = 0 
for(i in 1: length(ratio)){
  if(ratio[i] > 0){
    if(ratio[i] > max){
      max = ratio[i]
      operation = i
    }
  }
}
max
operation

names(pin1_roundness)[operation]

#Trend of Variance
variance = tibble(
  op = name,
  variance = var_col
)

ggplot(variance, aes(x = op, y = variance)) + 
  geom_point(size = 5) + geom_line() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#########################################################################
#################Relationship among Different Operations#################
#########################################################################

####################################
######### Taper and Roundness ######
####################################

Taper = test_Pin1 %>% select(grep("Taper", names(test_Pin1)))

round120 = round %>% select(grep("OP120", names(round)))
Taper250 = Taper %>% select(grep("OP110", names(Taper)))

R_T = cbind(round120, Taper250)

new_round = R_T[complete.cases(R_T), ]
names(new_round)
model = lm(`OP250..49.P1.Taper`~., new_round)
summary(model)

##second column 
#unique(c120$operation)
#cc120 = c120 %>% filter(operation == "OP120..2102.Roundness.P1.ctr")
#outliers = boxplot(cc120$value)$out

#which(cc120$value %in% outliers)
#test = cc120[-which(cc120$value %in% outliers),]


###### Remove Outliers for the second column #####
names(R_T)
Rcol2 = R_T %>% select(`OP120..2102.Roundness.P1.ctr`) %>% mutate(num = 1:380555)
T250_Rcol2 = cbind(Taper250, Rcol2)

outliers = boxplot(T250_Rcol2$OP120..2102.Roundness.P1.ctr)$out
no_outlier = T250_Rcol2[-which(T250_Rcol2$OP120..2102.Roundness.P1.ctr %in% outliers),]
remove_outlier = no[complete.cases(no_outlier), ]

model1 = lm(`OP250..49.P1.Taper`~`OP120..2102.Roundness.P1.ctr`, remove_outlier)
summary(model1)

remove_outlier %>% ggplot(aes(`OP120..2102.Roundness.P1.ctr`, `OP250..49.P1.Taper`)) + 
  geom_point() + #scale_x_continuous(breaks = seq(54.2, 54.4, by=.01), limits = c(54.2,54.4)) +
  geom_smooth(method = "lm")

#####################################
######## Lobing and Diameter ########
#####################################

Diameter = test_Pin1 %>% select(grep("Diameter", names(test_Pin1)))
Lobing = test_Pin1 %>% select(`OP120..2091.Lobing..Median..P1.ctr`)

D_L = cbind(Diameter, Lobing)

new_round = D_L[complete.cases(D_L), ]
names(new_round)
model = lm(`OP120..2091.Lobing..Median..P1.ctr`~., new_round)
summary(model)

names(new_round)


new_round %>% ggplot(aes(`OP250..33.Diameter.P1.section.3`, `OP120..2091.Lobing..Median..P1.ctr`)) + 
  geom_point() + #scale_x_continuous(breaks = seq(54.2, 54.4, by=.01), limits = c(54.2,54.4)) +
  geom_smooth(method = "lm")

unique(new_round$OP250..31.Diameter.P1.section.1)

##### Separate into Two Groups #####

new_round55 = new_round %>% filter(`OP250..33.Diameter.P1.section.3`<= 55)
new_round57 = new_round %>% filter(`OP250..33.Diameter.P1.section.3`> 57)

new_round57 %>% ggplot(aes(`OP250..31.Diameter.P1.section.1`, `OP120..2091.Lobing..Median..P1.ctr`)) + 
  geom_point() + #scale_x_continuous(breaks = seq(54.2, 54.4, by=.01), limits = c(54.2,54.4)) +
  geom_smooth(method = "lm")

model = lm(`OP120..2091.Lobing..Median..P1.ctr`~., new_round57)
summary(model)

###########################################
######## QuenchVolume and Diameter ########
###########################################

Diameter = test_Pin1 %>% select(grep("Diameter", names(test_Pin1)))
QuenchVolume = test_Pin1 %>% select(contains("QuenchVolume"))
names(QuenchVolume)

D_Q = cbind(Diameter, QuenchVolume)

new_round = D_Q[complete.cases(D_Q), ]
names(new_round)
model = lm(`OP90..010.12.Pin.1.QuenchVolume`~., new_round)
summary(model)

names(new_round)

##### Separate into Two Groups #####

new_round55 = new_round %>% filter(`OP250..33.Diameter.P1.section.3`<= 55)
new_round57 = new_round %>% filter(`OP250..33.Diameter.P1.section.3`> 57)

new_round57 %>% ggplot(aes(`OP250..31.Diameter.P1.section.1`, `OP90..010.12.Pin.1.QuenchVolume`)) + 
  geom_point() + scale_x_continuous(breaks = seq(57.28, 57.34, by=.01), limits = c(57.28,57.34)) +
  geom_smooth(method = "lm")

model = lm(`OP90..010.12.Pin.1.QuenchVolume`~., new_round57)
summary(model)

###### Separate QuenchVolume #####
names(new_round)

new_QV60 = new_round %>% filter(`OP90..010.12.Pin.1.QuenchVolume`<= 66, `OP90..010.12.Pin.1.QuenchVolume`>=40)
new_QV80 = new_round %>% filter(`OP90..010.12.Pin.1.QuenchVolume`>=70)

# 57 vs 60 (with the most values)
new_round57 = new_QV60 %>% filter(`OP250..33.Diameter.P1.section.3`> 57)

new_round57 %>% ggplot(aes(`OP90..010.12.Pin.1.QuenchVolume`,`OP250..31.Diameter.P1.section.1`)) + 
  geom_point() + scale_y_continuous(breaks = seq(57.28, 57.34, by=.01), limits = c(57.28,57.34)) +
  geom_smooth(method = "lm")

model = lm(`OP90..010.12.Pin.1.QuenchVolume`~., new_round57)
summary(model)
 
#Plot boxplot
nnew_round57 = new_round57 %>% mutate(Quench_factor = as.factor(`OP90..010.12.Pin.1.QuenchVolume`))
nnew_round57 %>% ggplot +
  geom_boxplot(aes(Quench_factor, `OP250..31.Diameter.P1.section.1`))

#Variance comparison-quench
a = nnew_round57 %>% group_by(Quench_factor) %>% summarize(variance = var(`OP250..31.Diameter.P1.section.1`))
a = a[complete.cases(a), ] %>%  mutate(quenchV_value = as.numeric(as.character(Quench_factor)))
max(a$quenchV_value)
a %>% ggplot(aes(quenchV_value, variance)) + geom_point(size = 5) + geom_path() +
  scale_x_continuous(breaks = seq(45,64, by=1))


# 55 vs 60
new_round55 = new_QV60 %>% filter(`OP250..33.Diameter.P1.section.3`< 55)

new_round55 %>% ggplot(aes(`OP250..31.Diameter.P1.section.1`, `OP90..010.12.Pin.1.QuenchVolume`)) + 
  geom_point() + #scale_x_continuous(breaks = seq(5.28, 57.34, by=.01), limits = c(57.28,57.34)) +
  geom_smooth(method = "lm")

model = lm(`OP90..010.12.Pin.1.QuenchVolume`~., new_round55)
summary(model)

# 57 vs 80 
new_round57 = new_QV80 %>% filter(`OP250..33.Diameter.P1.section.3`> 57)
names(new_round57)
unique(new_round57$`OP90..010.12.Pin.1.QuenchVolume`)

new_round57 %>% ggplot(aes(`OP90..010.12.Pin.1.QuenchVolume`, `OP250..31.Diameter.P1.section.1`)) + 
  geom_point() + scale_y_continuous(breaks = seq(57.28, 57.34, by=.01), limits = c(57.28,57.34)) +
  geom_smooth(method = "lm")

model = lm(`OP90..010.12.Pin.1.QuenchVolume`~., new_round57)
summary(model)

#variance comparison
nnew_round57 = new_round57 %>% mutate(Quench_factor = as.factor(`OP90..010.12.Pin.1.QuenchVolume`))
a = nnew_round57 %>% group_by(Quench_factor) %>% summarize(variance = var(`OP250..31.Diameter.P1.section.1`))
a = a[complete.cases(a), ] %>%  mutate(quenchV_value = as.numeric(as.character(Quench_factor)))
min(a$quenchV_value)
a %>% ggplot(aes(quenchV_value, variance)) + geom_point(size = 5) + geom_path() +
  scale_x_continuous(breaks = seq(75,83, by=1))

##################################################
################# Diameter and Ra ################
##################################################

#Ra_Avg.All
Ra = test_Pin1 %>% select(grep("Ra_Avg.All", names(test_Pin1)))

Diameter = test_Pin1 %>% select(grep("Diameter", names(test_Pin1)))
Ra120 = Ra %>% select(grep("OP120", names(Ra)))

R_D = cbind(Ra120, Diameter)

new_round = R_D[complete.cases(R_D), ]
names(new_round)

names(new_round)
#model = lm(`OP250..49.P1.Taper`~., new_round)
#summary(model)

new_round55 = new_round %>% filter(`OP250..33.Diameter.P1.section.3`<= 55)
new_round57 = new_round %>% filter(`OP250..33.Diameter.P1.section.3`> 57)

new_round57 %>% ggplot(aes(`OP250..31.Diameter.P1.section.1`, `OP120..201000001.P1.Ra_Avg.All`)) + 
  geom_point() + #scale_x_continuous(breaks = seq(54.2, 54.4, by=.01), limits = c(54.2,54.4)) +
  geom_smooth(method = "lm")

#######################################################
################# QuenchTime vs diameter ##############
#######################################################

Diameter = test_Pin1 %>% select(grep("Diameter", names(test_Pin1)))
QuenchTime = test_Pin1 %>% select(contains("QuenchTime"))
names(QuenchTime)

D_T = cbind(Diameter, QuenchTime)

new_round = D_T[complete.cases(D_T), ]
names(new_round)
#model = lm(`OP90..010.12.Pin.1.QuenchVolume`~., new_round)
#summary(model)


##### Separate into Two Groups #####

new_round55 = new_round %>% filter(`OP250..33.Diameter.P1.section.3`<= 55)
new_round57 = new_round %>% filter(`OP250..33.Diameter.P1.section.3`> 57)

new_round57 %>% ggplot(aes(`OP250..31.Diameter.P1.section.1`, `OP90..010.13.Pin.1.QuenchTime`)) + 
  geom_point() +
  geom_smooth(method = "lm")

#model = lm(`OP90..010.12.Pin.1.QuenchVolume`~., new_round57)
#summary(model)

##### Separate QuenchVolume ####
names(new_round)

# 5_7 vs 57 (with the most values)
new_round5_7 = new_round57 %>% filter(`OP90..010.13.Pin.1.QuenchTime`> 5, `OP90..010.13.Pin.1.QuenchTime`< 7)

new_round5_7 %>% ggplot(aes(`OP90..010.13.Pin.1.QuenchTime`,`OP250..31.Diameter.P1.section.1`)) + 
  geom_point() + 
  geom_smooth(method = "lm")

#model = lm(`OP90..010.12.Pin.1.QuenchVolume`~., new_round57)
#summary(model)

#Plot boxplot
nnew_round57 = new_round5_7 %>% mutate(Quench_factor = as.factor(`OP90..010.13.Pin.1.QuenchTime`))
nnew_round57 %>% ggplot +
  geom_boxplot(aes(Quench_factor, `OP250..31.Diameter.P1.section.1`))

#Variance comparison-quench
a = nnew_round57 %>% group_by(Quench_factor) %>% summarize(variance = var(`OP250..31.Diameter.P1.section.1`))
a = a[complete.cases(a), ] %>%  mutate(quenchV_value = as.numeric(as.character(Quench_factor)))
max(a$quenchV_value)
min(a$quenchV_value)
a %>% ggplot(aes(quenchV_value, variance)) + geom_point(size = 5) + geom_path() +
  scale_x_continuous(breaks = seq(5.1,6.1, by=0.05))

#######################################################
################# Diameter and Ovality ################
#######################################################

Ovality = test_Pin1 %>% select(grep("Ovality", names(test_Pin1)))
names(Ovality)
colSums(!is.na(Ovality))

Diameter = test_Pin1 %>% select(grep("Diameter", names(test_Pin1)))

O_D = cbind(Ovality, Diameter)

new_round = O_D[complete.cases(O_D), ]
names(new_round)

#model = lm(`OP250..49.P1.Taper`~., new_round)
#summary(model)

new_round55 = new_round %>% filter(`OP250..33.Diameter.P1.section.3`<= 55)
new_round57 = new_round %>% filter(`OP250..33.Diameter.P1.section.3`> 57)

new_round57 %>% ggplot(aes(`OP250..31.Diameter.P1.section.1`, `OP250..61.P1.Ovality`)) + 
  geom_hex() + scale_x_continuous(breaks = seq(57.27, 57.35, by=.01), limits = c(57.27, 57.35)) +
  geom_smooth(method = "lm")

model = lm(`OP250..61.P1.Ovality`~`OP250..31.Diameter.P1.section.1`, new_round57)
summary(model)

##################################################
################# Diameter and Ra ################
##################################################

Ra = test_Pin1 %>% select(grep("Ra_Avg.R", names(test_Pin1)))
names(Ra)
Diameter = test_Pin1 %>% select(grep("Diameter", names(test_Pin1)))
Ra120 = Ra %>% select(grep("OP240", names(Ra)))

R_D = cbind(Ra120, Diameter)

new_round = R_D[complete.cases(R_D), ]
names(new_round)

names(new_round)
#model = lm(`OP250..49.P1.Taper`~., new_round)
#summary(model)

new_round55 = new_round %>% filter(`OP250..33.Diameter.P1.section.3`<= 55)
new_round57 = new_round %>% filter(`OP250..33.Diameter.P1.section.3`> 57)

new_round57 %>% ggplot(aes(`OP250..31.Diameter.P1.section.1`, `OP240..201000101.P1.Ra_Avg.R`)) + 
  geom_point() + #scale_x_continuous(breaks = seq(54.2, 54.4, by=.01), limits = c(54.2,54.4)) +
  geom_smooth(method = "lm")

####################################################
#################### Ovality and Ra ################
####################################################

Ovality = test_Pin1 %>% select(grep("Ovality", names(test_Pin1)))
Ra = test_Pin1 %>% select(grep("Ra_Avg.R", names(test_Pin1)))
Ra120 = Ra %>% select(grep("OP120", names(Ra)))
R_D = cbind(Ovality, Ra120)

new_round = R_D[complete.cases(R_D), ]
names(new_round)
sort(unique(new_round$OP250..61.P1.Ovality))

new_round %>% ggplot() + 
  geom_violin(aes(`OP120..201000101.P1.Ra_Avg.R`,`OP250..61.P1.Ovality`)) +
  geom_point(aes(`OP120..201000101.P1.Ra_Avg.R`,`OP250..61.P1.Ovality`))  +
  scale_x_continuous(breaks = seq(0.1875, 0.35, by=0.01), limits = c(0.1875, 0.35))
  #geom_smooth(aes(`OP250..61.P1.Ovality`,`OP120..201000101.P1.Ra_Avg.R`),method = "lm")


################################################################
################# Diameter and Center Deviation ################
################################################################

Ra = test_Pin1 %>% select(grep("Center.Deviation", names(test_Pin1)))
names(Ra120)
Ra120 = Ra %>% select(grep("OP250", names(Ra)))
Diameter = test_Pin1 %>% select(grep("Diameter", names(test_Pin1)))


R_D = cbind(Ra120, Diameter)
names(R_D)

new_round = R_D[complete.cases(R_D), ]
names(new_round)

names(new_round)
#model = lm(`OP250..49.P1.Taper`~., new_round)
#summary(model)

new_round55 = new_round %>% filter(`OP250..33.Diameter.P1.section.3`<= 55)
new_round57 = new_round %>% filter(`OP250..33.Diameter.P1.section.3`> 57)

new_round57 %>% ggplot(aes(`OP250..31.Diameter.P1.section.1`, `OP250..55.P1.Center.Deviation`)) + 
  geom_hex() + scale_x_continuous(breaks = seq(57.25, 57.35, by=.01), limits = c(57.25, 57.35)) +
  geom_smooth(method = "lm")

model = lm(`OP250..55.P1.Center.Deviation`~., new_round57)
summary(model)

#################################################################
################# Roundness and Center Deviation ################
#################################################################

Ra = test_Pin1 %>% select(grep("Center.Deviation", names(test_Pin1)))
names(Ra)
Round = test_Pin1 %>% select(grep("Roundness", names(test_Pin1)))
names(Round)
Ra120 = Ra %>% select(grep("OP120", names(Ra)))
Round120 = Round %>% select(grep("OP240", names(Round)))
Round120pst = Round120 %>% select(grep("pst", names(Round120)))
names(Round120pst)

R_D = cbind(Ra120, Round120pst)

new_round = R_D[complete.cases(R_D), ]
names(new_round)

names(new_round)
#model = lm(`OP250..49.P1.Taper`~., new_round)
#summary(model)

new_round %>% ggplot(aes(`OP240..2101.Roundness.P1.pst`, `OP120..2601.Center.Deviation.P1.pst`)) + 
  geom_point() + #scale_x_continuous(breaks = seq(54.2, 54.4, by=.01), limits = c(54.2,54.4)) +
  geom_smooth(method = "lm")
