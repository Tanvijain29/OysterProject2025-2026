options("install.lock"=FALSE)

#OBCTotalarea = 40,468.6 m²/10 acres, OBC subpplot = 0.5 acre/ 2023.43m²
#LHTotalArea = 80,792.689 m²/20acres, LH subplot = 0.5 acre/ 2023.43m²
#quadrat area is 0.25m²
#OBC area sampled = 19 * 0.25 = 4.75m²
#LH area sampled = 17 * 0.25= 4.25m²
#very rough estimate but western bay area with sand and gravel substrate is about 211 acres 

Transition <- matrix(  
  c(
    0.007,  0 ,  0 ,  0, 0 , 0,
    .27, .49,  0 ,  0, 0 , 0,
    0  , .11, .22,  0, 0 , 0,
    0  , 0  , 0  ,0.007, 0 , 0,
    0  , 0  , 0  ,.27,.49, 0,
    0  , 0  , 0  , 0 ,.11, .22
  )
  ,nrow=6, ncol=6, byrow=T
)

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

connectivitymat <- t(matrix(     
  c(
    0.096, 0 , 0, 0.008, 0, 0, 
    0, 0,  0,  0, 0, 0,
    0, 0,  0,  0, 0, 0,
    0.006, 0,  0, 0.008, 0, 0,
    0,   0,0,  0, 0,0,
    0,   0,0,  0, 0,0
  )
  ,nrow=6,ncol=6,byrow=T
))


#estimating pre-settlement larval mortality
daily_larval_mort<- 0.20
starting_larva<- 40000
time_until_Settle<- 14

existing_larva<- starting_larva
for(day in 1:time_until_Settle){
  existing_larva<- existing_larva*(1-daily_larval_mort)
}
print(existing_larva)
survival_larva<- existing_larva/starting_larva
print(survival_larva)

Fecundity_larval_mort<- Fecundity*survival_larva #assuming a 4% larval survival (20% daily mortality in 14 days) over 2 weeks


#NOTE: theuerkauf model: {TransitionMatrix + [(P of remaining in the same size class(connectivitymatrix * fecunditymatrix)]} * initial abundance
#NOTE: for fecundity, use both range estimates, i. e., from Mann et al 2014 and Mroch 2012
 

#adjusting for pre-settlement larval mortality

fc2<- Fecundity_larval_mort*connectivitymat
fc2
fc2t<- fc2 + Transition
fc2t

InitAbund <- c(238, 3333,	22377, 4047, 21299, 60064 ) # initial empirical abundance from 2025 LH and then OBC
InitAbund2 <- c(0, 0, 0, 0, 0, 0) #supposing 0 oysters in 2022
#InitAbund2 <- c(1, 1, 1, 1, 1, 1) #supposing 0 oysters in 2022


#LH_23_spat<- 5775000
#OB_23_spat<- 2000000
#LH_24_spat<- 6679195
#LH_24_Adult<- 329707
#OB_24_spat<- 218320

nYears <- 10# set the number of years to project

dumping_matrix<- matrix(0, nrow = nrow(fc2t),ncol=nYears+1 )
dumping_matrix[,1]<- c(5775000, 0,      0, 2000000, 0, 0 )#2023 lh first 
dumping_matrix[,2]<- c(6679195, 0,  329707, 218320, 0, 0    )#2024
dumping_matrix[,3]<- c(3000000, 0,  30000, 3000000, 0, 75000)#2025
dumping_matrix[,4]<- c(3000000, 0,  30000, 3000000, 0, 75000)#2026
dumping_matrix[,5]<- c(3000000, 0,  30000, 3000000, 0, 75000)#2027
dumping_matrix[,6]<- c(3000000, 0,  30000, 3000000, 0, 75000)#2028
dumping_matrix[,7]<- c(3000000, 0,  30000, 3000000, 0, 75000)#2029
dumping_matrix[,8]<- c(3000000, 0,  30000, 3000000, 0, 75000)#2030
dumping_matrix[,9]<- c(3000000, 0,  30000, 3000000, 0, 75000)#2031
dumping_matrix[,10]<- c(3000000, 0, 30000, 3000000, 0, 75000)#2032

AgeStructured <- FALSE # set to TRUE for Leslie matrix and FALSE for Lefkovitch 


#--------------------------------------------------------------------------


#DUMPING PROJECTION

allYearsdump <- matrix(0, nrow=nrow(fc2t), ncol=nYears+1)# build a storage array for all stages and all years
allYearsdump[,1] <- InitAbund2  # set the year 0 abundance                                    
for(t in 2:(nYears+1)){   # loop through all years
  #allYearsdump[,t] <-  fc2t %*% (allYearsdump[,t-1] + dumping_matrix[,t-1])

temp1<- allYearsdump[,t-1] + dumping_matrix[,t-1]
temp2<- Fecundity_larval_mort %*% temp1# new juveniles produced
temp3<- connectivitymat %*% temp2
temp4 <- temp1 + temp3
allYearsdump[,t] <- Transition %*% temp4 
}

#GRAPHING 
allYearsdumplog<- log10(allYearsdump + 0.1)
plot(1, 1 , pch="" , ylim=c(0, max(allYearsdumplog)), xlim=c(0,nYears+1),xlab="Years",ylab="log Abundance",xaxt="n", main = "With Dumping")  # set up blank plot
cols <- c("red", "darkblue", "lightblue", "purple", "maroon", "black")    # set up colors to use
for(s in 1:ncol(fc2t)){
  points(allYearsdumplog[s,],col=cols[s],type="l",lwd=2)    # plot out each life stage abundance, one at a time
}
axis(1,at=seq(1,nYears+1),labels = seq(0,nYears))   # label the axis
if(AgeStructured){
  leg <-  paste("Age",seq(1,(ncol(fc2t))))
}else{
  leg <- paste("Stage",seq(1,ncol(fc2t))) 
}
legend("bottomright",col=cols,lwd=rep(2,ncol(fc2t)),legend= c("Juvenile LH", "Subadult LH", "Adult LH", "Juvenile OBC", "Subadult OBC", "Adult OBC"),bty="n
       ")  # put a legend on the plot

#-------------------------------------------------------------------------

#NO DUMPING PROJECTION
allYears <- matrix(0, nrow=nrow(fc2t), ncol=nYears+1)# build a storage array for all stages and all years
allYears[,1] <- InitAbund  # set the year 0 abundance                                    
for(t in 2:(nYears+1)){   # loop through all years
  allYears[,t] <-  fc2t %*% allYears[,t-1] }
  #temp1<- allYears[,t-1] 
  #temp2<- Fecundity_larval_mort %*% temp1# new juveniles produced
  #temp3<- connectivitymat %*% temp2
  #temp4<- temp1+ temp3
  #allYears[,t]<- Transition %*% temp4 }
  

allYearslog<- log10(allYears +0.1)
plot(1,1,pch="",ylim=c(0,max(allYearslog)),xlim=c(0,nYears+1),xlab="Years",ylab="Log Abundance",xaxt="n", main = "No dumping")  # set up blank plot
cols <- c("red", "darkblue", "lightblue", "purple", "maroon", "black")   # set up colors to use
for(s in 1:ncol(fc2t)){
  points(allYearslog[s,],col=cols[s],type="l",lwd=2)    # plot out each life stage abundance, one at a time
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
#^incorporate additional habitats in the model. 
#^take into account dumping data 
#^improve connectivity matrix using MIKE  
#better fecundity estimates from lit or local hatcheries
#use substrate data to estimate the probability of larval survival in a particular reef



#2/3/2026 Meeting with Dr. Freeman
#Need better connectivity numbers through MIKE model.
#Use multiple estimates for connectivity. I already have low estimates. 

##FOR NEXT WEEK
#include a 3rd site within the bay that is also contributing to the larval pool; the general 'habitat' as opposed to the 'outside'


#2/9/2025-pre-meeting notes
#really need 2025 dumping data


#2/9/2026
#initial estimates for the 3rd site are close to zero.
#waiting to hear back from Rob about dumping data for 2025
#They plan to add 75,000 adult oysters every year, at least in OBC.
#For the 3rd habitat, assume a certain percent that survive based on substrate, for example if 94 spat make it to the 3rd
#habitat, then 47 of them settle if 50% die because of landing on sand. 
#use substrate data from Dr. Wallce's HSM to estimate 3rd site settlement/survival


