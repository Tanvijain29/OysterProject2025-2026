options("install.lock"=FALSE)
library(popbio)
library(tidyverse)
library(GGally)
library(car)
library(readxl)
library(rlang)
library(scales)

#Base Projection Matrix

#Borrowing transition rates from Theuerkauf et al. 2021 for sanctuary reef type 
T_block <- matrix(c(
    0.0012,        0,     0,
    0.3797,   0.3630,     0,
    0.3639,   0.3619, 0.0275
), nrow=3, byrow=TRUE)

#Borrowing from the same paper
F_block <- matrix(c(
    398, 7639, 39071,
    0,   0,    0,
    0,   0,    0
), nrow=3, byrow=TRUE)

#create 15x15 matrices for 5 sites and 3 life stages

Transition <- kronecker(diag(5), T_block)
Fecundity  <- kronecker(diag(5), F_block)

#connectivity matrix derived from Wallace MIKE model
connect_block <- matrix(c(
#from LH    OBC   Inner   0 COP CSH
    0.622, 0.006, 0.002, 0.0, 0.0, # to LH
    0.000, 0.226, 0.000, 0.0, 0.0, # to OBC
    0.000, 0.098, 0.22, 0.0, 0.0, # to IB
    0.002, 0.024, 0.030, 0.0, 0.0, # to Cove Point
    0.032, 0.008, 0.004, 0.0, 0.0  # to CSH
), nrow=5, byrow=TRUE)


#create a small 3x3 matrix with 1 in the 1x1 position. When multiplied by the con_block, only juveniles can move.
juveniles_only <-matrix(0, nrow = 3, ncol = 3)
juveniles_only[1,1] <- 1

#fill the diagonals juveniles with connectivity numbers
connectivity<- kronecker(connect_block, juveniles_only)

#Simulation Time
nYears <- 10

Transition<- Transition * 0.82

#Daily Larval Mortality for 21 day period from ABC model
dlm <- 0.30
surviving_larva <-(1-dlm)^21

#Fecundity scalar from ABC model
Fecundity <- Fecundity * 0.98
Fecundity_after_larval_mort <- Fecundity  * surviving_larva

#post-settlement survival is 20%
fc <- (connectivity %*%  Fecundity_after_larval_mort ) * 0.2

#main projection matrix A
fct <- fc + Transition

#Empirically collected abundances in 2022. Data only for LH and OBC.others initialize at 1 oyster/stage/site
InitAbund <- c(1, 1655, 54448, 1, 1, 1770, 1, 1, 1, 1, 1, 1, 1, 1, 1) #LH, OBC, Inner Bay, Cove Point, CSH

#Empirical Data from oyster deployment 2022-2025
#Assuming that minimum amount of oysters (3 mil spats, 75k adults) continue getting deployed till 2032
#LH gets only spats
#OBC also gets adults
#Other sites do not get anything
#Added a mortality rate to my new spats

dumping_matrix <- matrix(0, nrow = 15, ncol=nYears+1 )
dumping_matrix[1:9, 1] <-    c(2035000 * 0.10, 0, 0, 5810000 * 0.10, 0,   0   , 0, 0, 0) #2023
dumping_matrix[1:9, 2] <-    c(247198  * 0.10, 0, 0, 6714155 * 0.10, 0, 329707, 0, 0, 0)
dumping_matrix[1:9, 3] <-    c(22500   * 0.10, 0, 0, 5881028 * 0.10, 0, 129533, 0, 0, 0)
dumping_matrix[1:9, 4:10] <- c(3000000 * 0.10, 0, 0, 3000000 * 0.10, 0, 75000 , 0, 0, 0)

#Maximum carrying capacity for each site assuming reef capacity at 50 oysters/m^2
K_sites_50 <- c(4046860, 2023430, 42739220, 10837171, 65402346)
K_sites_100 <- c(8093710, 4046860, 85388700, 21650680, 130718100)
#LH 20 acres, OBC 10 acres, Inner Bay 211 acres, CP acres 53.5 acres , CSH acres 323.2 acres

#-------------------------------------------------------------------------------------------

#PROJECTION CODE

run_projection <- function(initial_population, dump_mat = NULL, years = nYears) {
  
  proj_mat <- matrix(0, nrow = nrow(fct), ncol = years + 1)
  proj_mat[, 1] <- initial_population
  recruits <- c(1, 4, 7, 10, 13)
  
  for(t in 2:(years + 1)) {
    current_N <- proj_mat[, t - 1]
    
    #Add restoration oysters 
    if(!is.null(dump_mat)) {
      current_N <- current_N + dump_mat[, t - 1]
    }
    
    #Calculate site totals and the density scalar
    Site_Totals <- colSums(matrix(current_N, nrow = 3))
    Density_Scalar <- pmax(0.1, (K_sites_50 - Site_Totals) / K_sites_50)
    
    #Recruitment
    larvae <- Fecundity_after_larval_mort %*% current_N
    settlers <- (connectivity %*% larvae) * 0.2 #post-settlement survival
    
    #Apply density limit to the new recruits
    settlers[recruits] <- settlers[recruits] * Density_Scalar
    
    #Project to the next year
    proj_mat[, t] <- (Transition %*% current_N) + as.vector(settlers)
  }
  return(proj_mat)
}

#No restoration projection
allYears <- run_projection(initial_population = InitAbund)
allYearslog<- log10(allYears +0.1)

#With restoration projection
allYearsdump <- run_projection(initial_population = InitAbund, dump_mat = dumping_matrix)
allYearsdumplog<- log10(allYearsdump + 0.1) #log 10 of the population. 0.1 added to stabilize the math 


#-----------------------------------
#PLOTTING

site_names <- c("Laurel Hollow", "Oyster Bay Cove", "Inner Bay", "Cove Point", "Cold Spring Harbor")
no_restoration <- rowsum(allYears, group = rep(site_names, each = 3))
with_restoration <- rowsum(allYearsdump, group = rep(site_names, each = 3))

no_restoration_df <- as.data.frame(no_restoration) %>%
  setNames(2022:(2022 + ncol(allYears) - 1)) %>%
  rownames_to_column("Site") %>%
  pivot_longer(-Site, names_to = "Year", values_to = "Abundance") %>%
  mutate(Year = as.numeric(Year), Scenario = "Without Restoration")

with_restoration_df <- as.data.frame(with_restoration) %>%
  setNames(2022:(2022 + ncol(allYearsdump) - 1)) %>%
  rownames_to_column("Site") %>%
  pivot_longer(-Site, names_to = "Year", values_to = "Abundance") %>%
  mutate(Year = as.numeric(Year), Scenario = "With Restoration")



plot_data <- rbind(no_restoration_df, with_restoration_df)
plot_data$Site <- factor(plot_data$Site, levels = site_names)

ggplot(plot_data, aes(x = Year, y = Abundance + 0.01 , color = Scenario, linetype = Scenario, shape = Scenario)) +
  geom_line(linewidth = 0.8) +
  geom_point(data = subset(plot_data, Year %in% c(2022, 2024, 2026, 2028, 2030, 2032)), size = 2.0)+
  facet_wrap(~ Site, scales = "fixed", ncol = 3) +
  scale_color_manual(values = c("With Restoration" = "black", "Without Restoration" = "black")) +
  scale_linetype_manual(values = c("With Restoration" = "solid", "Without Restoration" = "dashed")) +
  scale_shape_manual(values = c("With Restoration" = 15, "Without Restoration" = 0)) +
  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
  scale_y_log10(
    breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000),
    labels = c("1", "10", "100", "1K", "10K", "100K", "1M")) +
  # ylim(0, 2000000)+
  labs(
    title = paste0("Eastern Oyster ", nYears, "-Year Population Projection "),
    x = "Year",
    y = "Total Oyster Population"
  ) +
  theme_bw() +
  theme(
    plot.margin = margin(15, 15, 15, 15),
    panel.spacing.x = unit(1.2, "lines"),
    panel.spacing.y = unit(1.8, "lines"),
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    strip.text = element_text(size = 15, face = "bold", color = "black"),
    axis.title = element_text(size = 15, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text = element_text(size = 13, color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 13),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14, face = "bold"),
    legend.key.width = unit(2.5, "cm"),
    legend.key.height = unit(0.8, "cm"),
    legend.margin = margin(t = 15),
    legend.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", color = NA)
  )


#----------------------------
#Model evaluation  

#Population growth rate
lambda(fct) 

#site-level elasticity
elas<- elasticity(fct)
site_names <- c("Laurel Hollow", "Oyster Bay Cove", "Inner Bay", "Cove Point", "Cold Spring Harbor")
site_elasticity <- colSums(matrix(colSums(elas), nrow = 3))
names(site_elasticity) <- site_names
print(round(site_elasticity, 4))


#life stage elasticity
stage_names <- c("Recruits", "Subadults", "Adults")
stage_elasticity <- rowSums(matrix(rowSums(elas), nrow = 3))
names(stage_elasticity) <- stage_names
print(round(stage_elasticity, 4))

#manual perturbation
test_vital_rates <- function(tmat_scalar, fmat_scalar, dlm_rate, retention_scalar, post_settle_rate) {
  
  T_block <- matrix(c(
    0.0012,        0,       0,
    0.3797,   0.3630,       0,
    0.3639,   0.3619,  0.0275
  ), nrow=3, byrow=TRUE)
  
  F_block <- matrix(c(
    398, 7639, 39071,
    0,   0,    0,
    0,   0,    0
  ), nrow=3, byrow=TRUE)
  
  Transition <- kronecker(diag(5), T_block) * tmat_scalar
  Fecundity  <- kronecker(diag(5), F_block) * fmat_scalar 
  
  connect_block <- matrix(c(
    0.622, 0.006, 0.002, 0.0, 0.0, 
    0.000, 0.226, 0.000, 0.0, 0.0, 
    0.000, 0.098, 0.220, 0.0, 0.0, 
    0.002, 0.024, 0.030, 0.0, 0.0, 
    0.032, 0.008, 0.004, 0.0, 0.0  
  ), nrow=5, byrow=TRUE)
  
  diag(connect_block) <- diag(connect_block) * retention_scalar
  
  juveniles_only <- matrix(0, nrow = 3, ncol = 3)
  juveniles_only[1,1] <- 1
  connectivity <- kronecker(connect_block, juveniles_only)
  
  surviving_larva <- (1 - dlm_rate)^21
  Fecundity_final <- Fecundity * surviving_larva
  
  fct <- ((connectivity %*% Fecundity_final) * post_settle_rate) + Transition
  
  return(lambda(fct))
}


base_tmat <- 0.82
base_fmat <- 0.98
base_dlm <- 0.30
base_retention_scalar <- 1.0  
base_post_settle <- 0.20 

lambda_base <- test_vital_rates(base_tmat, base_fmat, base_dlm, base_retention_scalar, base_post_settle)

# Perturb each parameter by  1 percent. 
lambda_dlm    <- test_vital_rates(base_tmat, base_fmat, base_dlm * 0.99, base_retention_scalar, base_post_settle)
lambda_reten  <- test_vital_rates(base_tmat, base_fmat, base_dlm, base_retention_scalar * 1.01, base_post_settle)
lambda_ps     <- test_vital_rates(base_tmat, base_fmat, base_dlm, base_retention_scalar, base_post_settle * 1.01)

# Calculate the percent change in lambda
change_dlm    <- ((lambda_dlm - lambda_base) / lambda_base) * 100
change_reten  <- ((lambda_reten - lambda_base) / lambda_base) * 100
change_ps     <- ((lambda_ps - lambda_base) / lambda_base) * 100

results_table <- data.frame(
  Parameter = c("Daily Larval Mortality ", "Larval Retention", "Post-Settlement Survival"),
  Percent_Change_in_Lambda = c(change_dlm, change_reten, change_ps)
)

print(results_table)


#lambda vs dlm curve

dlm_sequence <- seq(0.10, 0.50, by = 0.01)
lambda_results <- numeric(length(dlm_sequence))

for (i in seq_along(dlm_sequence)) {
  lambda_results[i] <- test_vital_rates(
    tmat_scalar = base_tmat, 
    fmat_scalar = base_fmat, 
    dlm_rate = dlm_sequence[i], 
    retention_scalar = base_retention_scalar, 
    post_settle_rate = base_post_settle
  )
}
df<- data.frame(
  DLM = dlm_sequence,
  Lambda = lambda_results
)

ggplot(df, aes(x = DLM, y = Lambda)) +
  geom_line(color = "black", linewidth = 1.0) +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray50", linewidth = 1) +
    labs(
    x = "Daily Larval Mortality Rate",
    y = "Population Growth Rate (\u03BB)"
  ) +
  theme_classic()

#Model fit
validation_data <- data.frame(
  year = c("2022", "2022", "2022", "2023", "2023", "2023", "2024", "2024", "2024", "2025", "2025", "2025", "2022", "2022", "2022", "2025", "2025", "2025"),
  Observed = c(0, 1655, 54448, 0, 5982, 36597, 140942, 5023, 56935, 19044, 266616, 1790138, 0, 0, 1770, 161874, 851970, 2402556 ), #lh 2022, 2023, 2024 2025, obc 2022, obc 2025
  Modeled =  c(1, 1655, 54448, 141917, 63853.4, 62443.2 , 202250.6, 70889.7, 70081.2, 220264.65, 84773.1,  83640, 1, 1, 1770, 131673.3, 315827, 314669.5), 
  Site = c(rep("Laurel Hollow", 12), rep("Oyster Bay Cove", 6) ))


log_model <- lm(log10(Modeled + 1) ~ log10(Observed + 1), data = validation_data)

r_sq <- summary(log_model)$r.squared
intercept <- coef(log_model)[1]
slope <- coef(log_model)[2]

ggplot(validation_data, aes(x = Observed + 1, y = Modeled + 1)) +
  geom_smooth(method = "lm", formula = y ~ x, color = "black", se = FALSE, linewidth = 1) +
  geom_point(aes(fill = year, shape = Site), size = 3, color = "black", alpha = 0.8, position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c("Laurel Hollow" = 22, "Oyster Bay Cove" = 24)) +
  scale_fill_manual(values = c("2022" = "lightpink", "2023" = "dodgerblue", "2024" = "darkblue", "2025" = "black")) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  guides(fill = guide_legend(override.aes = list(shape = 21)),
         shape = guide_legend(override.aes = list(fill = "white"))) +
  annotate("text", x = 2, y = 800000, label = "R² = 0.63" , hjust = -0.2, vjust = 1.5, size = 5) +
  annotate("text", x = 2, y = 500000, label = "y = 0.66x + 1.63" , hjust = -0.2, vjust = 1.5, size = 5) +
  labs(
    x = "Observed Population Size (Log)",
    y = "Modeled Population Size (Log)"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.85, 0.20), 
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.background = element_blank(),
    legend.key = element_blank()
  )

#Plotting each life stage separately 

#cols <- c("red", "darkblue", "lightblue", "deepskyblue", "aquamarine", "black", "brown", "darkgreen", "darksalmon","pink","grey","darkorange1","maroon","deeppink", "cyan") 

#plot(1,1,pch="",ylim=c(0,max(allYearslog)),xlim=c(0,nYears+1),xlab="Years",ylab=" Log Abundance",xaxt="n", main = "No dumping")  
#for(s in 1:ncol(fct)){
#  points(allYearslog[s,],col=cols[s],type="l",lwd=2) }   
#axis(1,at=seq(1,nYears+1),labels = seq(0,nYears))  
#points(x = rep(4, length(InitAbund2)), y=log10(InitAbund2 + 0.1), col = cols , pch = 15)
#legend("topleft",col=cols,lwd=rep(2,ncol(fc2t)),legend= c("Juvenile LH", "Subadult LH", "Adult LH", "Juvenile OBC", "Subadult OBC", "Adult OBC", "Juvenile W", "Subadult W", "Adult W", "J CP", "SA CP", "A CP", "J CSH", "SA CSH", "A CSH"),
#bty="n")


#plot(1, 1 , pch="" , ylim=c(0, max(allYearsdumplog)), xlim=c(0,nYears+1),xlab="Years",ylab="Log Abundance",xaxt="n", main = "With Dumping")  # set up blank plot
#for(s in 1:ncol(fct)){
#  points(allYearsdumplog[s,],col=cols[s],type="l",lwd=2)}
#axis(1,at=seq(1,nYears+1),labels = seq(0,nYears))   
#points(x = rep(4, length(InitAbund2)), y=log10(InitAbund2 + 0.1), col = cols , pch = 15)
#legend("topleft",col=cols,lwd=rep(2,ncol(fc2t)),legend= c("Juvenile LH", "Subadult LH", "Adult LH", "Juvenile OBC", "Subadult OBC", "Adult OBC", "Juvenile W", "Subadult W", "Adult W", "J CP", "SA CP", "A CP", "J CSH", "SA CSH", "A CSH"),bty="n")# put a legend on the plot