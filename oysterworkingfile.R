options("install.lock"=FALSE)


hist(Oyster_Predation_Data$No.ofSpats)#not normal

boxplot(Oyster_Predation_Data$No.ofSpats~Oyster_Predation_Data$BagID)
##bunch of outliers
boxplot(Oyster_Predation_Data$No.ofSpats~Oyster_Predation_Data$Site)
##bunch of outliers
boxplot(Oyster_Predation_Data$No.ofSpats~Oyster_Predation_Data$Iteration)

##How long were the predation spat bags in the water for both iterations??
  

pred<- lm(Oyster_Predation_Data$No.ofSpats ~ Oyster_Predation_Data$Iteration + Oyster_Predation_Data$Site + Oyster_Predation_Data$BagID)
summary(pred)  
 
TukeyHSD(aov(Oyster_Predation_Data$No.ofSpats ~ Oyster_Predation_Data$Site))
##next step how to get one rate for spat mortality?

summary(aov(Oyster_Predation_Data$No.ofSpats~ as.character(Oyster_Predation_Data$Iteration)))


#OBCTotalarea = 40,468.6 m²/10 acres, OBC subpplot = 0.5 acre/ 2023.43m²
#LHTotalArea = 80,792.689 m²/20acres, LH subplot = 0.5 acre/ 2023.43m²
#quadrat area is 0.5m²
#OBC area sampled = 19 * 0.5 = 9.5m²
#LH area sampled = 17 * 0.5= 8.5m²


# Multi-year projection code 
# Set key parameters 

nYears <- 20 # set the number of years to project
Transition <- matrix(  
  c(
    0.7,  0 ,  0 ,  0, 0 , 0,
    .27, .49,  0 ,  0, 0 , 0,
    0  , .11, .22,  0, 0 , 0,
    0  , 0  , 0  ,0.7, 0 , 0,
    0  , 0  , 0  ,.27,.49, 0,
    0  , 0  , 0  , 0 ,.11, .22
  )
  ,nrow=6, ncol=6, byrow=T
)

InitAbund <- c(238, 3333,	22377, 4047, 21299, 60064 ) # initial abundance vector
AgeStructured <- FALSE # set to TRUE for Leslie matrix and FALSE for Lefkovitch 


Fecundity <- matrix(     
  c(
    398 , 7639 ,  39071, 0 , 0, 0,
    0   ,     0,      0, 0 , 0, 0,
    0   ,     0,      0, 0 , 0, 0,
    0   ,     0,      0,398, 7639, 39071,
    0   ,     0,      0, 0 , 0, 0, 
    0   ,     0,      0, 0 , 0, 0
    
  )
  ,nrow=6, ncol=6,byrow=T
)

Fecundity_larval_mort<- Fecundity *0.22 #assuming a 22% larval survival over 2 weeks; calculated later in the script

connectivitymat <- t(matrix(     
  c(
    0.096, 0 , 0, 0.008, 0, 0, 
    0, 0,  0,  0, 0, 0,
    0, 0,  0,  0, 0, 0,
    0.006, 0,  0, 0.008, 0, 0,
    0,0,0,0,0,0,
    0, 0,0,0,0,0
  )
  ,nrow=6,ncol=6,byrow=T
))


##NOTE: theuerkauf model: {TransitionMatrix + [(P of remaining in the same size class(connectivitymatrix * fecunditymatrix)]} * initial abundance
##NOTE: for fecundity, use both range estimates, i. e., from Mann et al 2014 and Mroch 2012
 
fc<- Fecundity*connectivitymat
fct<- fc + Transition
fct

#adjusting for larval mortality
fc2<- Fecundity_larval_mort*connectivitymat
fc2t<- fc2 + Transition
fc2t
  
dumping_matrix<- matrix(0, nrow=nrow(fct), ncol = nYears+1)
dumping_matrix[,1] <- c(10000,1000,1000,1000,1000,1000) #random numbers for three sites for year 2023
dumping_matrix[,2] <- c(10000,1000,1000,1000,1000,1000)# random numbers for year 2204


#FOR loop for multi-year projection  
allYears <- matrix(0,nrow=nrow(fct),ncol=nYears+1)     # build a storage array for all stages and all years
allYears[,1] <- InitAbund  # set the year 0 abundance                                    
for(t in 2:(nYears+1)){   # loop through all years
  allYears[,t] <-  fc2t %*% allYears[,t-1]# allYears[,t] <-  fct %*% allYears[,t-1] +dumping_matrix[,t-1]
}

allYearslog<- log10(allYears)
plot(1,1,pch="",ylim=c(0,max(allYearslog)),xlim=c(0,nYears+1),xlab="Years",ylab="Log Abundance",xaxt="n")  # set up blank plot
cols <- rainbow(ncol(fct))    # set up colors to use
for(s in 1:ncol(fc2t)){
  points(allYearslog[s,],col=cols[s],type="l",lwd=2)     # plot out each life stage abundance, one at a time
}
axis(1,at=seq(1,nYears+1),labels = seq(0,nYears))   # label the axis
if(AgeStructured){
  leg <-  paste("Age",seq(1,(ncol(fc2t))))
}else{
  leg <- paste("Stage",seq(1,ncol(fc2t))) 
}
legend("topleft",col=cols,lwd=rep(2,ncol(fc2t)),legend= c("Juvenile LH", "Subadult LH", "Adult LH", "Juvenile OBC", "Subadult OBC", "Adult OBC"),bty="n
       ")  # put a legend on the plot

library(popbio)
lambda(fc2t) 



#notes from the defense:
#incorporate additional habitats in the model. 
#take into account dumping data 
#improve connectivity matrix using MIKE  
#better fecundity estimates from lit or local hatcheries
#use substrate data to estimate the probability of larval survival in a particular reef


#last meeting 12/3
daily_larval_mort<- 0.10
starting_larva<- 40000
time_until_Settle<- 14


existing_larva<- starting_larva
for(day in 1:time_until_Settle){
  existing_larva<- existing_larva* (1-daily_larval_mort)
}
print(existing_larva)

survival_larva<- existing_larvae/starting_larvae
print(survival_larva)


##calculate K to limit growth
#i can't figure out how to calculate K without the maximum density. I only have area and observed densities, which I don't think are close to the maximum. 









