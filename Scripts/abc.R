options("install.lock"=FALSE)
library(abc)
library(popbio)
library(dplyr)
library(tidyverse)
library(readr)

nYears <- 10
InitAbund <- c(1, 1655, 54448, 1, 1, 1770, 1, 1, 1, 1, 1, 1, 1, 1, 1)
K_sites_50 <- c(4046860, 2023430, 42739220, 10837171, 65402346)

T_block <- matrix(c(
  0.0012,       0,      0,
  0.3797,  0.3630,      0,
  0.3639,  0.3619,    0.0275
), nrow=3, byrow=TRUE)

F_block <- matrix(c(
  398, 7639, 39071,
  0,   0,    0,
  0,   0,    0
), nrow=3, byrow=TRUE)

Base_Transition <- kronecker(diag(5), T_block)
Base_Fecundity  <- kronecker(diag(5), F_block)


connect_block <- matrix(c(
  0.622, 0.006, 0.002, 0.0, 0.0, 
  0.000, 0.226, 0.000, 0.0, 0.0, 
  0.000, 0.098, 0.220, 0.0, 0.0, 
  0.002, 0.024, 0.030, 0.0, 0.0, 
  0.032, 0.008, 0.004, 0.0, 0.0  
), nrow=5, byrow=TRUE)

juveniles_only <- matrix(0, nrow = 3, ncol = 3)
juveniles_only[1,1] <- 1
connectivity <- kronecker(connect_block, juveniles_only)

# Dumping matrix 
dumping_matrix <- matrix(0, nrow = 15, ncol = nYears + 1)
dumping_matrix[1:9, 1] <- c(2035000 * 0.10, 0, 0, 5810000 * 0.10, 0, 0, 0, 0, 0) 
dumping_matrix[1:9, 2] <- c(247198  * 0.10, 0, 0, 6714155 * 0.10, 0, 329707, 0, 0, 0)
dumping_matrix[1:9, 3] <- c(22500   * 0.10, 0, 0, 5881028 * 0.10, 0, 129533, 0, 0, 0)
dumping_matrix[1:9, 4:10] <- c(3000000 * 0.10, 0, 0, 3000000 * 0.10, 0, 75000, 0, 0, 0)

run_projection <- function(initial_population, trans_mat, fec_mat, dump_mat, years = nYears) {
  proj_mat <- matrix(0, nrow = 15, ncol = years + 1)
  proj_mat[, 1] <- initial_population
  recruits <- c(1, 4, 7, 10, 13)
  
  for(t in 2:(years + 1)) {
    current_N <- proj_mat[, t - 1] + dump_mat[, t - 1]
    
    Site_Totals <- colSums(matrix(current_N, nrow = 3))
    Density_Scalar <- pmax(0.1, (K_sites_50 - Site_Totals) / K_sites_50)
    
    larvae <- fec_mat %*% current_N
    settlers <- (connectivity %*% larvae) * 0.2
    settlers[recruits] <- settlers[recruits] * Density_Scalar
    
    proj_mat[, t] <- (trans_mat %*% current_N) + as.vector(settlers)
  }
  return(proj_mat)
}


iterations <- 10000

results <- data.frame(
  iteration = 1:iterations,
  tmat = numeric(iterations),
  fmat = numeric(iterations),
  dlm = numeric(iterations),
  LH_2022_class1 = numeric(iterations),
  LH_2022_class2 = numeric(iterations),
  LH_2022_class3 = numeric(iterations),
  LH_2023_class1 = numeric(iterations),
  LH_2023_class2 = numeric(iterations),
  LH_2023_class3 = numeric(iterations),
  LH_2024_class1 = numeric(iterations),
  LH_2024_class2 = numeric(iterations),
  LH_2024_class3 = numeric(iterations),
  LH_2025_class1 = numeric(iterations),
  LH_2025_class2 = numeric(iterations),
  LH_2025_class3 = numeric(iterations),
  OBC_2022_class1 = numeric(iterations),
  OBC_2022_class2 = numeric(iterations),
  OBC_2022_class3 = numeric(iterations),
  OBC_2025_class1 = numeric(iterations),
  OBC_2025_class2 = numeric(iterations),
  OBC_2025_class3 = numeric(iterations)
)


for (iter in 1:iterations) {
  
  #Draw random parameters
  tmat_parameter <- runif(1, min = 0.8, max = 2.0) 
  fmat_parameter <- runif(1, min = 0.65, max = 1.3)
  dlm <- runif(1, min = 0.1, max = 0.35)
  
  #Apply parameters 
  surviving_larva <- (1 - dlm)^21
  Iter_Transition <- Base_Transition * tmat_parameter
  Iter_Fecundity <- Base_Fecundity * fmat_parameter * surviving_larva
  
  #Run the simulation
  allYearsdump <- run_projection(initial_population = InitAbund, 
                                 trans_mat = Iter_Transition, 
                                 fec_mat = Iter_Fecundity, 
                                 dump_mat = dumping_matrix)
  
  #Store the results 
  results$tmat[iter] <- tmat_parameter
  results$fmat[iter] <- fmat_parameter
  results$dlm[iter] <- dlm
  results$LH_2022_class1[iter] <- allYearsdump[1, 1]
  results$LH_2022_class2[iter] <- allYearsdump[2, 1]
  results$LH_2022_class3[iter] <- allYearsdump[3, 1]
  results$LH_2023_class1[iter] <- allYearsdump[1, 2]
  results$LH_2023_class2[iter] <- allYearsdump[2, 2]
  results$LH_2023_class3[iter] <- allYearsdump[3, 2]
  results$LH_2024_class1[iter] <- allYearsdump[1, 3]
  results$LH_2024_class2[iter] <- allYearsdump[2, 3]
  results$LH_2024_class3[iter] <- allYearsdump[3, 3]
  results$LH_2025_class1[iter] <- allYearsdump[1, 4]
  results$LH_2025_class2[iter] <- allYearsdump[2, 4]
  results$LH_2025_class3[iter] <- allYearsdump[3, 4]
  results$OBC_2022_class1[iter] <- allYearsdump[4, 1]
  results$OBC_2022_class2[iter] <- allYearsdump[5, 1]
  results$OBC_2022_class3[iter] <- allYearsdump[6, 1]
  results$OBC_2025_class1[iter] <- allYearsdump[4, 4]
  results$OBC_2025_class2[iter] <- allYearsdump[5, 4]
  results$OBC_2025_class3[iter] <- allYearsdump[6, 4]
}


write.csv(results, "simulation_result.csv", row.names = FALSE)

rfile<- read_csv("simulation_result.csv")


#first, pull parameter values from a prior distribution (used uniform distribution here)
#then, use those values to simulate data sets corresponding to observed data set
#then, calculate the distance between simulated and observed data sets
#a threshold of tolerance is chosen so that only certain simulations/values are selected based on their
#closeness to observed values
#use the selected parameter values to make a posterior distribution using one of the algorithms
#provided by the abc package (neural net method)


observed <- c(
  LH_2022_class1 = log10(0+ 0.1),
  LH_2022_class2 = log10(1655+ 0.1) ,
  LH_2022_class3 = log10(54448+ 0.1),
  LH_2023_class1 = log10(0+ 0.1),
  LH_2023_class2 = log10(5982+ 0.1) ,
  LH_2023_class3 = log10(36597+ 0.1),
  LH_2024_class1 = log10(140942+ 0.1),
  LH_2024_class2 = log10(5023+ 0.1) ,
  LH_2024_class3 = log10(56935+ 0.1),
  LH_2025_class1 = log10(19044 + 0.1),
  LH_2025_class2 = log10(266616 + 0.1),
  LH_2025_class3 = log10(1790138 + 0.1),
  OBC_2022_class1 = log10(0 + 0.1),
  OBC_2022_class2 = log10(0 + 0.1),
  OBC_2022_class3 = log10(1770 + 0.1),
  OBC_2025_class1 = log10(161874 + 0.1),
  OBC_2025_class2 = log10(851970 + 0.1),
  OBC_2025_class3 = log10(2402556 + 0.1)
)


simulated <- data.frame(
  LH_2022_class1 = log10(rfile$LH_2022_class1+ 0.1),
  LH_2022_class2 = log10(rfile$LH_2022_class2 + 0.1),
  LH_2022_class3 = log10(rfile$LH_2022_class3 + 0.1),
  LH_2023_class1 = log10(rfile$LH_2023_class1+ 0.1),
  LH_2023_class2 = log10(rfile$LH_2023_class2 + 0.1),
  LH_2023_class3 = log10(rfile$LH_2023_class3 + 0.1),
  LH_2024_class1 = log10(rfile$LH_2024_class1+ 0.1),
  LH_2024_class2 = log10(rfile$LH_2024_class2 + 0.1),
  LH_2024_class3 = log10(rfile$LH_2024_class3 + 0.1),
  LH_2025_class1 = log10(rfile$LH_2025_class1+ 0.1),
  LH_2025_class2 = log10(rfile$LH_2025_class2 + 0.1),
  LH_2025_class3 = log10(rfile$LH_2025_class3 + 0.1),
  OBC_2022_class1 = log10(rfile$OBC_2022_class1 + 0.1),
  OBC_2022_class2 = log10(rfile$OBC_2022_class2 + 0.1),
  OBC_2022_class3 = log10(rfile$OBC_2022_class3 + 0.1),
  OBC_2025_class1 = log10(rfile$OBC_2025_class1 + 0.1),
  OBC_2025_class2 = log10(rfile$OBC_2025_class2 + 0.1),
  OBC_2025_class3 = log10(rfile$OBC_2025_class3 + 0.1)
)


params<- rfile[, c("tmat", "fmat", "dlm")]

res<- abc(target = observed, param = params,  sumstat = simulated, tol = 0.01, method = "neuralnet")
summary(res)
#hist(res)
#plot(res, param = params)


posterior_data <- as.data.frame(res$adj.values)
posterior_long <- posterior_data %>% pivot_longer(cols = c(tmat, fmat, dlm), names_to = "Parameter", values_to = "Value")
param_labels <- c(
  dlm = "Daily Larval Mortality",
  fmat = "Fecundity Scalar",
  tmat = "Transition Scalar"
)
p1 <- ggplot(subset(posterior_long, Parameter == "dlm"), aes(x = Value)) +
  geom_density(color = "#CC6677", linewidth = 1) +
  scale_x_continuous(limits = c(0.28, 0.33)) + # Forces the axis to expand
  labs(title = "Daily Larval Mortality", x = "Parameter Value", y = "Density") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 2. Plot for Fecundity Scalar
p2 <- ggplot(subset(posterior_long, Parameter == "fmat"), aes(x = Value)) +
  geom_density(color = "#228833", linewidth = 1) +
  scale_x_continuous(limits = c(0.5, 1.5)) + 
  labs(title = "Fecundity Scalar", x = "Parameter Value", y = "Density") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 3. Plot for Transition Scalar
p3 <- ggplot(subset(posterior_long, Parameter == "tmat"), aes(x = Value)) +
  geom_density(color = "#4477AA", linewidth = 1) +
  scale_x_continuous(limits = c(0.6, 0.9)) + 
  labs(title = "Transition Scalar", x = "Parameter Value", y = "Density") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

library(patchwork)
p1+p2+p3












