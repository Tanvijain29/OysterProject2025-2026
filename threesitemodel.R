options("install.lock"=FALSE)

#survival and growth matrix
Transition <- matrix(     
  c(
    0.007, 0   ,  0,        0,    0,    0,   0,        0,  0,
    0.27 , 0.49,  0,        0,    0,    0,   0,        0,  0, 
    0    , 0.11,  0.22,     0,    0,    0,   0,        0,  0, 
    0    , 0   ,  0,    0.007,    0,    0,   0,        0,  0,
    0    , 0   ,  0,     0.27, 0.49,    0,   0,        0,  0, 
    0    , 0   ,  0,        0, 0.11, 0.22,   0,        0,  0, 
    0    , 0   ,  0,        0,    0,    0,   0.007,    0,  0,
    0    , 0   ,  0,        0,    0,    0,   0.27 , 0.49,  0,
    0    , 0   ,  0,        0,    0,    0,   0,     0.11,  0.22
  )
  ,nrow=9,ncol=9,byrow=T
)

#per capita fecundity
Fecundity <- matrix(     
  c(
    398, 7639, 39071,    0,    0,    0,   0,    0,  0,
    0  , 0   ,     0,    0,    0,    0,   0,    0,  0, 
    0  , 0   ,     0,    0,    0,    0,   0,    0,  0, 
    0  , 0   ,     0,  398, 7639, 39071,  0,    0,  0,
    0  , 0   ,     0,    0,    0,    0,   0,    0,  0, 
    0  , 0   ,     0,    0,    0,    0,   0,    0,  0, 
    0  , 0   ,     0,    0,    0,    0, 398, 7639, 39071,
    0  , 0   ,     0,    0,    0,    0,   0,    0,  0,
    0  , 0   ,     0,    0,    0,    0,   0,    0,  0
  )
  ,nrow=9,ncol=9,byrow=T
)

#larval connectivity among three sites.
connectivitymat <- t(matrix(     
  c(
    0.096, 0 , 0, 0.008, 0, 0, 0.002, 0, 0,
    0,     0,  0,     0, 0, 0,     0, 0, 0, 
    0,     0,  0,     0, 0, 0,     0, 0, 0, 
    0.006, 0,  0, 0.008, 0, 0, 0.188, 0, 0,
    0,     0,  0,     0, 0, 0,     0, 0, 0,
    0,     0,  0,     0, 0, 0,     0, 0, 0, 
    0,     0,  0, 0.038, 0, 0,   0.5, 0, 0,
    0,     0,  0,     0, 0, 0,     0, 0, 0,
    0,     0,  0,     0, 0, 0,     0, 0, 0
  )
  ,nrow=9,ncol=9,byrow=T
))

nYears <- 20# set the number of years to project

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

#adjusting for pre-settlement larval mortality

fc2<- Fecundity_larval_mort*connectivitymat
fc2
fc2t<- fc2 + Transition
fc2t


InitAbund <- c(238, 3333,	22377, 4047, 21299, 60064, 0, 0, 0 ) # initial empirical abundance from 2025 LH and then OBC
#InitAbund2 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0) #supposing 0 oysters in 2022
InitAbund2 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1) #supposing 0 oysters in 2022

#oysters deployed
dumping_matrix<- matrix(0, nrow = nrow(fc2t),ncol=nYears+1 )
dumping_matrix[,1]<- c(2035000, 0,  0, 5810000, 0, 0, 0, 0, 0)#2023 lh first 
dumping_matrix[,2]<- c(247198,  0,  0, 6714155, 0, 329707, 0, 0, 0)#2024
dumping_matrix[,3]<- c(22500,   0,  0, 5881028, 0, 129533, 0, 0, 0)#2025
dumping_matrix[,4]<- c(3000000, 0,  0, 3000000, 0, 75000, 0, 0, 0)#2026
dumping_matrix[,5]<- c(3000000, 0,  0, 3000000, 0, 75000, 0, 0,0)#2027
dumping_matrix[,6]<- c(3000000, 0,  0, 3000000, 0, 75000, 0, 0, 0)#2028
dumping_matrix[,7]<- c(3000000, 0,  0, 3000000, 0, 75000, 0, 0, 0)#2029
dumping_matrix[,8]<- c(3000000, 0,  0, 3000000, 0, 75000, 0, 0, 0)#2030
dumping_matrix[,9]<- c(3000000, 0,  0, 3000000, 0, 75000, 0, 0, 0)#2031
dumping_matrix[,10]<- c(3000000,0,  0, 3000000, 0, 75000, 0, 0, 0)#2032

AgeStructured <- FALSE # set to TRUE for Leslie matrix and FALSE for Lefkovitch 

K_sites_50 <- c(4046860, 2023430, 42694373) 
K_sites_75<- c(6070290, 3035145, 64041560)
K_sites_100<- c(8093720, 4046860, 85388746)

#-------------------------------------------------------------------------------------------

#PROJECTION
allYearsdump <- matrix(0, nrow=nrow(fc2t), ncol=nYears+1)# build a storage array for all stages and all years
allYearsdump[,1] <- InitAbund2  # set the year 0 or 1 abundance   

for(t in 2:(nYears+1)){   # loop through all years
  temp1<- allYearsdump[,t-1] + dumping_matrix[,t-1]

  lh_total_pop = sum(temp1[1:3])
  obc_total_pop= sum(temp1[4:6])
  wsr_total_pop= sum(temp1[7:9])
  Current_N <- c(lh_total_pop, obc_total_pop, wsr_total_pop)
  
  # B. Calculate the Density Scalar (0 to 1)
  # This scalar will reduce both recruitment AND existing survival
  Density_Scalar <- pmax(0.1, (K_sites_50 - Current_N) / K_sites_50) #assuming a conservative K of 50 oysters/m sq.
  # Note: I used pmax(0.1) so survival doesn't hit zero instantly, 
  # simulating a "minimum" survival even at high density.
  
  # C. RECRUITMENT: Limit new larvae (same as before)
  temp2 <- Fecundity_larval_mort %*% temp1 
  temp3 <- connectivitymat %*% temp2       
  temp3[1] <- temp3[1] * Density_Scalar[1] 
  temp3[4] <- temp3[4] * Density_Scalar[2] 
  temp3[7] <- temp3[7] * Density_Scalar[3] 
  
  # D. SURVIVAL: Modify the Transition Matrix for this year
  # We create a temporary matrix 'Yearly_Transition'
  Yearly_Transition <- (Transition * 0.25)
  Yearly_Transition[1:3, 1:3] <- Yearly_Transition[1:3, 1:3] * Density_Scalar[1]wh
  Yearly_Transition[4:6, 4:6] <- Yearly_Transition[4:6, 4:6] * Density_Scalar[2]
  Yearly_Transition[7:9, 7:9] <- Yearly_Transition[7:9, 7:9] * Density_Scalar[3]
  
  # E. Final Projection
  temp4 <- temp1 + as.vector(temp3)
  allYearsdump[,t] <- Yearly_Transition %*% temp4 }


#Graphing
allYearsdumplog<- log10(allYearsdump + 0.1)
plot(1, 1 , pch="" , ylim=c(0, max(allYearsdumplog)), xlim=c(0,nYears+1),xlab="Years",ylab="Log Abundance",xaxt="n", main = "With Dumping")  # set up blank plot
cols <- c("red", "darkblue", "lightblue", "purple", "yellow", "black", "green", "darkgreen", "orange")   # set up colors to use
for(s in 1:ncol(fc2t)){
  points(allYearsdumplog[s,],col=cols[s],type="l",lwd=2)    # plot out each life stage abundance, one at a time
}
axis(1,at=seq(1,nYears+1),labels = seq(0,nYears))   # label the axis
if(AgeStructured){
  leg <-  paste("Age",seq(1,(ncol(fc2t))))
}else{
  leg <- paste("Stage",seq(1,ncol(fc2t)))   }
points(x = rep(4, length(InitAbund)), y=log10(InitAbund + 0.1), col = cols , pch = 15)

#legend("topleft",col=cols,lwd=rep(2,ncol(fc2t)),legend= c("Juvenile LH", "Subadult LH", "Adult LH", "Juvenile OBC", "Subadult OBC", "Adult OBC", "Juvenile W", "Subadult W", "Adult W"),bty="n")# put a legend on the plot

#----------------------------------------------------------------------------------------------

#No dumping PROJECTION

allYears <- matrix(0, nrow=nrow(fc2t), ncol=nYears+1)# build a storage array for all stages and all years
allYears[,1] <- InitAbund2 # set the year 0 abundance                                    
for(t in 2:(nYears+1)){ 
  
  temp1<- allYears[,t-1]
  lh_total_pop = sum(temp1[1:3])
  obc_total_pop= sum(temp1[4:6])
  wsr_total_pop= sum(temp1[7:9])
  Current_N <- c(lh_total_pop, obc_total_pop, wsr_total_pop)

  Density_Scalar <- pmax(0.1, (K_sites_50 - Current_N) / K_sites_50)

  temp2 <- Fecundity_larval_mort %*% temp1 
  temp3 <- connectivitymat %*% temp2       
  temp3[1] <- temp3[1] * Density_Scalar[1] 
  temp3[4] <- temp3[4] * Density_Scalar[2] 
  temp3[7] <- temp3[7] * Density_Scalar[3] 
  
  Yearly_Transition <- Transition * 0.25
  Yearly_Transition[1:3, 1:3] <- Yearly_Transition[1:3, 1:3] * Density_Scalar[1]
  Yearly_Transition[4:6, 4:6] <- Yearly_Transition[4:6, 4:6] * Density_Scalar[2]
  Yearly_Transition[7:9, 7:9] <- Yearly_Transition[7:9, 7:9] * Density_Scalar[3]
  
  temp4 <- temp1 + as.vector(temp3)
  allYears[,t] <- Yearly_Transition %*% temp4 }


allYearslog<- log10(allYears +0.1)
plot(1,1,pch="",ylim=c(0,max(allYearslog)),xlim=c(0,nYears+1),xlab="Years",ylab=" Log Abundance",xaxt="n", main = "No dumping")  # set up blank plot
cols <- c("red", "darkblue", "lightblue", "purple", "yellow", "black", "green", "darkgreen", "orange") # set up colors to use
for(s in 1:ncol(fc2t)){
  points(allYearslog[s,],col=cols[s],type="l",lwd=2)    # plot out each life stage abundance, one at a time
}
axis(1,at=seq(1,nYears+1),labels = seq(0,nYears))   # label the axis
if(AgeStructured){
  leg <-  paste("Age",seq(1,(ncol(fc2t))))
}else{s
  leg <- paste("Stage",seq(1,ncol(fc2t))) 
}

legend("topleft",col=cols,lwd=rep(2,ncol(fc2t)),legend= c("Juvenile LH", "Subadult LH", "Adult LH", "Juvenile OBC", "Subadult OBC", "Adult OBC", "Juvenile W", "Subadult W", "Adult W"),bty="n
       ")  # put a legend on the plot

points(x = rep(4, length(InitAbund)), y=log10(InitAbund + 0.1), col = cols , pch = 15)

---------------------------------------------------------

#sum total for each site, stages pooled.
#no dumping
site1_total1 <- colSums(allYears[1:3, ])
site2_total1 <- colSums(allYears[4:6, ])
site3_total1 <- colSums(allYears[7:9, ])

site_totals_matrix1 <- rbind(site1_total1, site2_total1, site3_total1)
log_site_totals1 <- log(site_totals_matrix1 + 0.1)

initabundpooled<- c(25948, 85410, 0)

#dumping

site1_totald<- colSums(allYearsdump[1:3, ])
site2_totald<- colSums(allYearsdump[4:6, ])
site3_totald <- colSums(allYearsdump [7:9, ])

site_totals_matrixd <- rbind(site1_totald, site2_totald, site3_totald)
log_site_totald <- log(site_totals_matrixd + 0.1)


plot(1,1,pch="",ylim=c(0,max(log_site_totals1)),xlim=c(0,nYears+1),
     xlab="Years", ylab="Log Abundance", xaxt="n", 
     main = "Oyster Abundance: With and Without Restoration")  # set up blank plot

cols <- c("coral4","orange", "black" ) # set up colors to use
for(s in 1:3){
  points(log_site_totals1[s,],col=cols[s],type="l",lwd=2, lty = 2)# plot out each life stage abundance, one at a time
}

for(s in 1:3) {
  points(log_site_totald[s, ], col = cols[s], type = "l", lwd = 2, lty = 1)
}

axis(1,at=seq(1,nYears+1),labels = seq(0,nYears))   


if(AgeStructured) {
  leg <- paste("Age", seq(1, (ncol(fc2t))))
} else {
  leg <- paste("Stage", seq(1, ncol(fc2t))) 
}

# Add the initial abundance points 
#points(x = rep(4, length(initabundpooled)), 
#       y = log10(initabundpooled + 0.1), 
#       col = cols, pch = 15)

points(x = rep(4, length(initabundpooled)), # Matches your image where points are at Year 3
       y = log10(initabundpooled + 0.1), 
       col = "black",   # Border
       bg = "white",    # Fill
       pch = 22,        # Square with border/fill
       cex = 1.2)

#text(x = 6.7, y = 4.8, label = "Empirical Estimates", cex = 0.9)

#legend(x = 7, y = 4, col = c(cols, "black", "black"), legend = c("Laurel Hollow", "Oyster Bay Cove", "West Bay", "Restoration (Solid)", "No Restoration (Dashed)"), 
#       lty = c(1, 1, 1, 1, 2), lwd = 2, bty = "n", ncol = 2)

legend_labels <- c("Laurel Hollow", "Oyster Bay Cove", "West Bay", "Dashed Lines: Same Sites No Restoration", "Empirical Estimates")

# Colors: Match your site colors; use black for the scenario/symbols
legend_cols <- c("brown", "orange", "black", "black", "black")

# Line types: 1 for sites & restoration; 2 for no restoration; NA for the symbol
legend_ltys <- c(1, 1, 1, 2, NA)

# Symbols: NA for the lines; 22 for the empirical estimate symbol
legend_pchs <- c(NA, NA, NA, NA, 22)

# Fill for symbol 22
legend_bg <- c(NA, NA, NA, NA, "white")

legend(x = 8, y = 6, 
       legend = legend_labels, 
       col = legend_cols, 
       pt.bg = legend_bg,
       lty = legend_ltys, 
       pch = legend_pchs,
       lwd = 2, 
       ncol = 1, 
       bty = "n")


#so figures right now are with K = 50, 0.25 of the Transition matrix, with dumping for 10 years only 

#pop_sum = as.matrix(c(lh_total_pop, obc_total_pop, wsr_total_pop), nrow = 3, ncol = 1)
#k_adjustment<- (1 - (pop_sum/K_sites_50))
#k_adjustment_9<- c(rep(k_adjustment[1],3), rep(k_adjustment[2],3), rep(k_adjustment[3],3))


#allYearsdump[,t] <-  fc2t %*% (allYearsdump[,t-1] + dumping_matrix[,t-1])}