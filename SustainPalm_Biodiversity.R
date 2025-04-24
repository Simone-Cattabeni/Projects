library(vegan)    
library(dplyr)
library(ggplot2)     
library(glmmTMB)   
library(emmeans)
library(pairwiseAdonis)
library(lme4)

#Performing nmds on pitfalls data
data_nmds <- read.csv("data_nmds.csv", header = TRUE, sep = ";")
comm_matrix <- read.csv("comm_matrix.csv", header = TRUE, sep = ";")

nmds <- metaMDS(comm_matrix[,2:26], distance = "bray", k = 2)
print(nmds)
stressplot(nmds)
plot(nmds)

habitat_type <- data_nmds %>% 
  distinct(Palm_point, Treatment) 

#Extract NMDS scores for sites and species
nmds_SiteScores <-
  as.data.frame(scores(nmds)$sites) %>%
  rownames_to_column(var = "Palm_point") %>% 
  left_join(habitat_type)

nmds_SpeciesScores <- 
  as.data.frame(scores(nmds, "species"))
nmds_SpeciesScores$species <- rownames(nmds_SpeciesScores) 

#Visualization
Habitat_Centroid <- 
  nmds_SiteScores %>% 
  group_by(Treatment) %>% 
  summarise(axis1 = mean(NMDS1),
            axis2 = mean(NMDS2)) %>% 
  ungroup()

habitat.hull <- 
  nmds_SiteScores %>% 
  group_by(Treatment) %>%
  slice(chull(NMDS1, NMDS2))

nmds_stress <- nmds$stress

ggplot() + 
  
  # add site scores
  geom_point(data = nmds_SiteScores, 
             aes(x=NMDS1, y=NMDS2, colour = Treatment), size = 2) + 
  
  # add species scores 
  geom_text(data = nmds_SpeciesScores, 
            aes(x=NMDS1, y=NMDS2, label = species)) +
  
  # add centroid 
  geom_point(data = Habitat_Centroid, 
             aes(x = axis1, y = axis2, color = Treatment), 
             size = 5, shape = 17) +
  
  # add convex hull
  geom_polygon(data = habitat.hull, 
               aes(x = NMDS1, y = NMDS2, fill = Treatment, group = Treatment), 
               alpha = 0.30) +
  
  # add stress value
  annotate("text", x = 0.75, y = 0.65, 
           label = paste("2d stress =", round(nmds_stress, 3))) +
  
  # edit theme
  labs(x = "NMDS1", y = "NMDS2") + 
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", 
                                    fill = NA, linewidth = .5),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.key.size = unit(.25, "cm")) 

#Printing a list for every taxa to check the assumption of Poisson distribution
pitfalls <- read.csv("pitfalls.csv", header = TRUE, sep = ";")
mean_variance_list <- list()

for (i in 5:52) {
  column_mean <- mean(pitfalls[[i]], na.rm = TRUE)
  column_variance <- var(pitfalls[[i]], na.rm = TRUE)
  
  mean_variance_list[[colnames(pitfalls)[i]]] <- list(mean = column_mean, variance = column_variance)
}
print(mean_variance_list)

#Fitting a negative binomial generalized mixed model for all the ants, accounting for the nested structure of the design and random effects of replicate and palms
ant_model_list <- list()
pairwise_results_list <- list()

for (i in 5:31) {
  
  response_var <- names(pitfalls)[i]
  
  formula <- as.formula(paste(response_var, "~ Treatment + (1 | Replicate/Palm)"))
  ant_model <- glmmTMB(formula, data = pitfalls, family = nbinom2)
  ant_model_list[[response_var]] <- summary(ant_model)
  
  emms <- emmeans(ant_model, ~ Treatment)
  posthoc <- contrast(emms, method = "pairwise")
  pairwise_summary <- summary(posthoc)
  
  pairwise_results_list[[response_var]] <- pairwise_summary
}
print(ant_model_list)

#Printing results
for (name in names(pairwise_results_list)) {
  cat("Results for", name, ":\n")
  print(pairwise_results_list[[name]])
}

#Performing a PERMANOVA to assess community distribution
data_comm <- read.csv("data_comm.csv", header = TRUE, sep = ";")
data_indep <- read.csv("data_indep.csv", header = TRUE, sep = ";")

perm <- adonis2(data_comm ~ Treatment, data = data_indep, strata = data_indep$Replicate)
perm

#Posthoc
dist_matrix <- vegdist(data_comm)
pairwise_results <- pairwise.adonis(dist_matrix, data_indep$Treatment)
print(pairwise_results)

##Similar analysis were conducted for the vegetation data