##################################################################################                                                                      #
#  CATS case study illustration                                                  #
#  Simulation of Complete and missing data                                      #
#  Rushani Wijesuriya 24th of June 2021                                         #
#                                                                                #
##################################################################################

rm(list=ls())

library(ReIns) #for generating random numbers from log-normal distribution 
library(splitstackshape) #to exapand the rows
library(boot)#for inverting the logit function 
library(dplyr)
library(DataCombine) #for the slide function 
library(missMethods)

#set seed
set.seed(3682500)

#set working directory (to where the script if saved)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#############parameter values#############

#the number of school clusters at wave 1
I<-40

#the number of students in each school at wave 1(do we fix this or vary)
J<-30

#child's age
a <- 7
b <- 10

#parameters for log school frequencies 
log.mu <- 3.27
log.sigma <- 0.57
log.min <- 2.10
log.max <- 4.20

##child's gender
lambda <- 0.5

##chid's teacher rating at wave 1
eta0 <- 3.00
eta1 <- 0.20
eta2 <- 0.10
eta3 <- 0.02

#depressive symptom scores at waves 1,2 and 3 (exposure)
delta0 <- 6.0
delta1 <- 0.01
delta2 <- -0.04
delta3 <- -0.20
delta4 <- -0.10
delta5 <- -0.30

#teacher ratings waves 2,3 and 4 (outcome)
gamma0 <- 2.60
gamma1 <- -0.02
gamma2 <- -0.20
gamma3 <- 0.02
gamma4 <- 0.70
gamma5 <- 0.01
gamma6 <- 0.01

#SDQ values at waves 1,2 and 3 (auxiliary)
beta0 <- 16.0
beta1 <- 0.40
beta2 <- 0.05

##parameters for the random effects

#exposure-depressive symptoms
sd_depL3 <- 0.25
sd_depL2 <- 0.80
sd_depL1 <- 1.50
  
#outcome-Teacher ratings
sd_tratingL3 <- sqrt(0.15)
sd_tratingL2 <- sqrt(0.35)
sd_tratingL1 <-sqrt(0.5)
  
#Auxiliary-SDQ
sd_sdqL3 <- 0.8
sd_sdqL2 <- 4.0
sd_sdqL1 <- 2.8

#porporition of students who move (at wave 2,3 and 4)

propw2 <- 0.05
propw3 <- 0.05
propw4 <- 0.05


#missing data generation in prev_dep
iota01 <- -6.5
iota02 <- -7.0
iota03 <- -7.5
iota1 <- 0.7
iota2 <- 0.4

#new schools added at each wave
nw2 <- 50
nw3 <- 50
nw4 <- 50


########### Complete data generation #####################

  
##generate the school clusters at wave 1
D <- data.frame(school.1=c(1:I))

#generate school level RE for wave 1
D$L3_RE_dep.1 <- rnorm(40,0,sd_depL3)
D$L3_RE_trating.1 <- rnorm(40,0,sd_tratingL3)
D$L3_RE_sdq.1 <- rnorm(40,0,sd_sdqL3)

#Generate the class sizes
D$freq <-round(exp(rtlnorm(40,log.mu,log.sigma,endpoint = log.max)))

#obtain the total number of students
total <- sum(D$freq)

#recale to 1200
N <- I*J
D$class_size <- round(D$freq*(N/total))

#add/remove students from the last cluster to total the no of students to 1200

if ((sum(D$class_size)-N)<0){D$class_size[I] <- D$class_size[I]+(N-sum(D$class_size))} 
if((sum(D$class_size)-N)>0){D$class_size[I] <- D$class_size[I]-(sum(D$class_size)-N)} 

#replicate the rows for adding students
D <- expandRows(D, "class_size")

#generate the indivdiuals
D$c_id <- c(1:N)

D <- D[order(D$school.1),]

D$freq=NULL

#Generate wave 2 school cluster memberships (add 50 schools and assign 5% of students to these schools)

# randomly split data 
random.split <- function(p){
  picked <- sample(seq_len(nrow(D)),size = floor(p*nrow(D)))
  obj <- list(change <- D[picked,], constant <- D[-picked,])
  return(obj)
}

W2 <- random.split(propw2)
changeW2 <- W2[[1]]

#assign the students who moved to 50 new schools
changeW2$school.2 <- rep((I+1):(I+nw2), length=nrow(changeW2))

#generate school level RE for wave 2 for those who moved
changeW2$L3_RE_dep.2 <- rep(rnorm(nw2,0,sd_depL3), length=nrow(changeW2))
changeW2$L3_RE_trating.2 <- rep(rnorm(nw2,0,sd_tratingL3),length=nrow(changeW2))
changeW2$L3_RE_sdq.2 <- rep(rnorm(nw2,0,sd_sdqL3),length=nrow(changeW2))


#generate(repeat wave 1)wave 2 information for those who didnt move
constant <- W2[[2]]
constant <- constant %>% 
  mutate(school.2 = school.1, L3_RE_dep.2=L3_RE_dep.1,L3_RE_trating.2=L3_RE_trating.1,L3_RE_sdq.2=L3_RE_sdq.1)

#merge the data 
D <- rbind(constant,changeW2)

#Generate wave 3 school cluster memberships (add 50 schools and assign 8% of students to these schools)

W3<- random.split(propw3)
changeW3 <- W3[[1]]

#assign the students who moved to 50 new schools
changeW3$school.3 <- rep((I+nw2+1):(I+nw2+nw3), length=nrow(changeW3))

#generate school level RE for wave 3 for those who moved
changeW3$L3_RE_dep.3 <- rep(rnorm(nw3,0,sd_depL3), length=nrow(changeW3))
changeW3$L3_RE_trating.3 <- rep(rnorm(nw3,0,sd_tratingL3),length=nrow(changeW3))
changeW3$L3_RE_sdq.3 <- rep(rnorm(nw3,0,sd_sdqL3),length=nrow(changeW3))


#generate(repeat)wave 3 information for those who didnt move
constant <- W3[[2]]
constant <- constant %>% 
  mutate(school.3 = school.2, L3_RE_dep.3=L3_RE_dep.2,L3_RE_trating.3=L3_RE_trating.2,L3_RE_sdq.3=L3_RE_sdq.2)


#merge the data 
D <- rbind(constant,changeW3)


#Generate wave 4 school cluster memberships (add 50 schools and assign 5% of students to these schools)

W4<- random.split(propw4)
changeW4 <- W4[[1]]

#assign the students who moved to 50 new schools
changeW4$school.4<- rep((I+nw2+nw3+1):(I+nw2+nw3+nw4), length=nrow(changeW4))

#generate school level RE for wave 4 for those who moved
changeW4$L3_RE_dep.4 <- rep(rnorm(nw4,0,sd_depL3), length=nrow(changeW4))
changeW4$L3_RE_trating.4 <- rep(rnorm(nw4,0,sd_tratingL3),length=nrow(changeW4))
changeW4$L3_RE_sdq.4 <- rep(rnorm(nw4,0,sd_sdqL3),length=nrow(changeW4))


#generate(repeat)wave 3 information for those who didnt move
constant <- W4[[2]]
constant <- constant %>% 
  mutate(school.4 = school.3, L3_RE_dep.4=L3_RE_dep.3,L3_RE_trating.4=L3_RE_trating.3,L3_RE_sdq.4=L3_RE_sdq.3)

#merge the data 
D <- rbind(constant,changeW4)

#rearrange columns

D <- D[,c( "c_id","school.1", "L3_RE_dep.1","L3_RE_trating.1","L3_RE_sdq.1","school.2","L3_RE_dep.2",
           "L3_RE_trating.2","L3_RE_sdq.2","school.3","L3_RE_dep.3","L3_RE_trating.3","L3_RE_sdq.3","school.4",        
           "L3_RE_dep.4","L3_RE_trating.4","L3_RE_sdq.4")]

D <- D[order(D$c_id),]

#child's age at wave 1
D$c_age <- runif((I*J),a,b)

#Simulate sex (M,F) groups (males=1, females=0)
D$uran=runif((I*J),0,1)

D$c_sex=ifelse(D$uran<=lambda,1,0)
 
##simulate SES values
D$c_ses <- rnorm(N,0,1)

#Simulate teacher ratings at wave 1
e_teacherw1 <- rnorm(N,0,1)
D$t_score.W1=eta0+eta1*D$c_sex+eta2*D$c_ses+eta3*D$c_age+e_teacherw1

D$uran=NULL

#generate individual level REs
D$L2_RE_dep <- rnorm(N,0,sd_depL2)
D$L2_RE_trating <- rnorm(N,0,sd_tratingL2)
D$L2_RE_sdq <- rnorm(N,0,sd_sdqL2)

#reshape to long format
D_long=reshape(D,varying =list(c("school.1","school.2","school.3","school.4"),
                                c("L3_RE_dep.1","L3_RE_dep.2","L3_RE_dep.3","L3_RE_dep.4"),
                                c("L3_RE_trating.1","L3_RE_trating.2","L3_RE_trating.3","L3_RE_trating.4"),
                               c("L3_RE_sdq.1","L3_RE_sdq.2","L3_RE_sdq.3","L3_RE_sdq.4")),idvar="c_id", 
             v.names=c("school","L3_RE_dep","L3_RE_trating","L3_RE_sdq"), times=c(1,2,3,4),direction= "long")

D_long <- D_long[order(D_long$c_id),]

#D_long <- D_long %>%filter(time!=1) 

#generate the exposure (depressive symptoms at waves 1,2 and 3)
e_dep <- rnorm(4800,0,sd_depL1)
D_long$dep <- delta0+delta1*D_long$c_age+delta2*D_long$c_sex+delta3*D_long$t_score.W1+
  delta4*D_long$c_ses+delta5*D_long$time+D_long$L3_RE_dep+D_long$L2_RE_dep+e_dep

#generate previous wave depression
D_long<- slide(D_long, Var = "dep", GroupVar = "c_id",
               slideBy = -1)

colnames(D_long)[colnames(D_long)=="dep-1"] <- "prev_dep"


#remove the original variable
D_long$dep=NULL

#generate the outcome (teacher ratings at waves 2,3 and 4)
e_trating <- rnorm(4800,0,sd_tratingL1)
D_long$t_rating <- gamma0+gamma1*D_long$prev_dep+gamma2*D_long$c_age+gamma3*D_long$c_sex+gamma4*D_long$t_score.W1+gamma5*D_long$c_ses+
  gamma6*D_long$time+D_long$L3_RE_trating+ D_long$L2_RE_trating+e_trating

#generate the auxiliary variable (SDQ at waves 1,2 and 3)
e_sdq <- rnorm(4800,0,sd_sdqL1)
D_long$prev_sdq <- beta0+beta1*D_long$prev_dep+beta2*D_long$time+D_long$L3_RE_sdq+ D_long$L2_RE_sdq+e_sdq

#drop variables
D_long <- D_long %>% select(!contains("L3")& !contains("L2"))

##########MIssing data generation######################

#reshape to wide format
D_wide <- reshape(D_long,v.names=c("school","t_rating","prev_sdq","prev_dep"),timevar = "time",idvar="c_id",direction= "wide")

#generate missing data in prev_dep 2,3 and 4 (probability of response, 1=observed, 0=missing) 
D_wide$r_prevdep.2 <- as.numeric(runif(1200,0,1)< inv.logit(iota01+iota1*D_wide$t_rating.2+iota2*D_wide$prev_sdq.2))
D_wide$r_prevdep.3 <- as.numeric(runif(1200,0,1)< inv.logit(iota02+iota1*D_wide$t_rating.3+iota2*D_wide$prev_sdq.3))
D_wide$r_prevdep.4 <- as.numeric(runif(1200,0,1)< inv.logit(iota03+iota1*D_wide$t_rating.4+iota2*D_wide$prev_sdq.4))


#check missing data proportions
1-(sum(D_wide$r_prevdep.2)/nrow(D_wide))
1-(sum(D_wide$r_prevdep.3)/nrow(D_wide))
1-(sum(D_wide$r_prevdep.4)/nrow(D_wide))

#generate missing data in Teacher scores at waves 1,2,3,4 
D_wide <- delete_MCAR(D_wide, 0.1, "t_score.W1")
D_wide <- delete_MCAR(D_wide, 0.1, "t_rating.2")
D_wide <- delete_MCAR(D_wide, 0.1, "t_rating.3")
D_wide <- delete_MCAR(D_wide, 0.1, "t_rating.4")

#generate missing data in prev_SDQ values at waves 2,3,4
D_wide <- delete_MCAR(D_wide, 0.1, "prev_sdq.2")
D_wide <- delete_MCAR(D_wide, 0.3, "prev_sdq.3")
D_wide <- delete_MCAR(D_wide, 0.3, "prev_sdq.4")


D_wide <- D_wide %>% select(!c("t_rating.1","prev_sdq.1","prev_dep.1"))

colnames(D_wide)[colnames(D_wide)=="school.1"] <- "school.W1"


#reshape to long format
D_long <- reshape(D_wide,varying =list(c("school.2","school.3","school.4"),
                                       c("prev_dep.2","prev_dep.3","prev_dep.4"),
                                       c("r_prevdep.2","r_prevdep.3","r_prevdep.4"),
                                       c("t_rating.2","t_rating.3","t_rating.4"),
                                       c("prev_sdq.2","prev_sdq.3","prev_sdq.4")),idvar="c_id", 
                  v.names=c("school","prev_dep","r_prevdep","t_rating","prev_sdq"), times=c(2,3,4),direction= "long")

#assign NA values 
D_long$prev_dep <- ifelse(D_long$r_prevdep==0,NA,D_long$prev_dep)


D_long$r_prevdep=NULL
#Export data
write.csv(D_long,paste0("CATS_dataL.csv"),row.names = F)
