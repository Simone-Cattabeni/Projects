library(ggcorrplot)
library(psych)
library(FactoMineR)
library(lme4)
library(emmeans)
library(ggplot2)
library(ggbiplot)
library(dplyr)

#1:Performing an explorative pca
data_pca <- read.csv("KNDE_pitfalls.csv", header = TRUE, sep = ";")

dataset <- data_pca[, 5:16]
visual <- data_pca[, 2]

pca <- prcomp(dataset, center = TRUE, scale = TRUE)
graph <- ggbiplot(pca, obs.scale = 1, var.scale = 1,
                  groups = visual, ellipse = FALSE,
                  circle = FALSE)
graph <- graph + scale_color_discrete(name = '')
graph <- graph + theme(legend.direction = 'horizontal',
                       legend.position = 'top')
print(graph)

#2:Calculating the soil health index (SHI) through the PCA approach
SHI <- read.csv("SHI.csv", header = TRUE, sep = ";")

#Performing correlation between the different components
corr_matrix = round(cor(SHI), 2)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)
#Performing a pca
pca <- prcomp(SHI, scale =T)
summary(pca)

#Rotating the loading with eigenvalue>1
rawLoadings <- pca$rotation[, 1:5]
rotated <- varimax(rawLoadings)
rotatedLoadings <- rotated$loadings
rotatedLoadings

##Calculate index on Excel based on the percentage of variance explained by principal components rotated.

index <- read.csv("index.csv", header = TRUE, sep = ";")

#Fitting a linear model for index
SHI_model <- lmer(SHI ~ T + (1 | r), data = index)
summary(SHI_model)

residuals <- residuals(SQI_model)
shapiro.test(residuals)

#ANOVA on the model
anova_result <- anova(SHI_model)
print(anova_result)
emm <- emmeans(SHI_model, ~ T)
pairwise_comparisons_tukey <- pairs(emm, adjust = "tukey")
print(pairwise_comparisons_tukey)

#Visualizing SHI over treatment
ggplot(index, aes(x = T, y = SHI)) +
  geom_boxplot(fill = "orange", color = "darkred") + 
  theme_classic() +                                
  labs(title = "Boxplot of SHI by Treatment",   
       x = "Treatment",                           
       y = "Soil Health Index (SHI)")

#Performing a linear regression between the index and other explainable data (e.g. yield)

model <- lm(Yield ~ SHI, data = index)
rsq <- summary(model)$r.squared

ggplot(index, aes(x = SHI, y = Yield)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Regression between SHI and Yield",
       x = "Soil Health Index (SHI)", 
       y = "Yield") +
  annotate("text", x = 0.7, y = 100, label = paste("R² = ", round(rsq, 3)), size = 5, color = "black") +
  theme_minimal()