library(dplyr)
library(tidyverse)

#1:Import pitfalls data
pitfalls <- read.csv("KNDE_pitfalls.csv", header = TRUE, sep = ";")
#NULL values are absence of species, handled as 0s
pitfalls[is.na(pitfalls)] <- 0

#Calculating and adding the Shannon Index exclusively for the ants genus
shannon_index <- apply(pitfalls[, 5:29], 1, function(row) {
  diversity(row, index = "shannon")
})
pitfalls$Shannon_Index <- shannon_index

write.csv(pitfalls, "pitfalls.csv", row.names = FALSE)

#2:Initialize a dataframe for non-metric multi-dimensional scaling (nmds)
data_nmds <- pitfalls[1:29]
data_nmds$Palm_point <- paste(data_nmds$Palm, data_nmds$Point, sep = "-")
data_nmds <- data_nmds %>%
  select(Replicate, Treatment, Palm_point, everything()) %>%
  select(-Palm, -Point)

comm_matrix <- data_nmds %>%
  select(-Replicate)

#Keep rows where the number of individuals is greater than 0
comm_matrix <- comm_matrix[rowSums(comm_matrix[, 4:27]) > 0, ]

comm_matrix <- comm_matrix %>%
  column_to_rownames("Palm_point")
comm_matrix$Treatment <- as.factor(comm_matrix$Treatment)

write.csv(data_nmds, "data_nmds.csv", row.names = FALSE)
write.csv(comm_matrix, "comm_matrix.csv", row.names = FALSE)

#3:Cleaning data to meet the requirements' structure of PERMANOVA
data_comm <- pitfalls[, 5:29]
rownames(data_comm) <- paste0(pitfalls[ , 3], "_", pitfalls[ , 4])
data_indep <- pitfalls[ , c(1, 2, 3, 4)]
rownames(data_indep) <- paste0(pitfalls[ , 3], "_", pitfalls[ , 4])

#Removing rows with 0 individuals of ants
nonzero_indices <- rowSums(!data_comm) < ncol(data_comm)
data_comm <- data_comm[nonzero_indices, ]
data_indep <- data_indep[nonzero_indices, ]

write.csv(data_comm, "data_comm.csv", row.names = FALSE)
write.csv(data_indep, "data_indep.csv", row.names = FALSE)

##Similar cleaning was performed for vegetation data, soil physics and soil chemistry

#4:Cleaning data to perform an explorative PCA on soil parameters
Chem <- read.csv("KNDE_Chemistry.csv", header = TRUE, sep = ";")
Bulk <- read.csv("KNDE_Bulk.csv", header = TRUE, sep = ";")
Water <- read.csv("KNDE_Water.csv", header = TRUE, sep = ";")

#Filter data based on soil depth
Chem_0_5 <- filter(Chem, Depth == "0 - 5")
Bulk_0_5 <- filter(Bulk, Depth == "0 - 5")
Water_0_5 <- filter(Water, Depth == "0 - 5")

#Join based on an unique ID to create one dataframe
data_pca <- Chem_0_5 %>%
  left_join(Bulk_0_5, by = "Palm") %>%
  left_join(Water_0_5, by = "Palm")

#Perform correlation matrix to explore relationships within the data
corr_matrix = round(cor(data_pca), 2)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)

#Remove redundant information 
data_pca <- data_pca %>% select(-Replicate, -Treatment, -Depth, -Total, -C.N, -Mg.1, -K.1, -pH.KCl,-P_avail)

write.csv(data_pca, "data_pca.csv", row.names = FALSE)