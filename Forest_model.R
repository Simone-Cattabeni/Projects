#Creating a simple model to calculate growth and harvest

#1:Growth in a 4x4 grid
K <- 400

SoilFertility <- matrix(data=c(1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.3, 1.2,1.2,1.3,1.3,1.2,1.3,1.4,1.4),
                        nrow=4,ncol = 4, byrow=TRUE)

StandingBiomass <- matrix(data=c(150,150,150,190,150,150,150,194,150,150,150,188,260,298,342,392),
                          nrow=4,ncol = 4, byrow=TRUE)

r <- 0.1 * SoilFertility
growth <- r * StandingBiomass * (1 - (StandingBiomass/K))

year_1 <- growth
Total_year_1 <- sum(year_1)

#2:Growth forecasted for 100 years with fixed soil fertility
years <- 1:100
SoilFertility <- 1.2 
StandingBiomassIni <- 150 
r <- 0.1 * SoilFertility
growth <- 0

forest <- data.frame(year=years,biomass=NA,growth=NA)
StandingBiomass <- StandingBiomassIni
for (year in years) {
  StandingBiomass <- StandingBiomass + growth
  growth <- r * StandingBiomass * (1 - (StandingBiomass/K))
  forest$growth[year] <- growth
  forest$biomass[year] <- StandingBiomass
  if(growth<=0.05) break
}

#3:Growth simulations in 4 quadrants (NW, NE, SW, SE)
K <- 400
ages <- 1:100
SoilFertility <- c(NW=1.20, NE=1.23, SW=1.23, SE= 1.35)
StandingBiomassIni <- c(NW=1, NE=1, SW=1, SE= 1)
r <- 0.1 * SoilFertility
growth <- c(NW=0, NE=0, SW=0, SE=0)
forest <- data.frame(age=ages, biomassNW= NA, biomassNE= NA, biomassSW= NA, biomassSE= NA,
                     growthNW= NA, growthNE= NA, growthSW= NA, growthSE= NA)
StandingBiomass <- StandingBiomassIni

for (age in ages) {
  StandingBiomass <- StandingBiomass + growth
  growth <- r * StandingBiomass * (1 - (StandingBiomass/K))
  forest$growthNW[age] <- growth["NW"]
  forest$growthNE[age] <- growth["NE"]
  forest$growthSW[age] <- growth["SW"]
  forest$growthSE[age] <- growth["SE"]
  forest$biomassNW[age] <- StandingBiomass["NW"]
  forest$biomassNE[age] <- StandingBiomass["NE"]
  forest$biomassSW[age] <- StandingBiomass["SW"]
  forest$biomassSE[age] <- StandingBiomass["SE"]
}

#4:Growth with changing fertility
K <- 400
years <- 1:5
SoilFertility <- 1.2
StandingBiomassIni <- 150
r <- 0.1 * SoilFertility
growth <- 0
forest <- data.frame(year=years,biomass=NA,growth=NA)
StandingBiomass <- StandingBiomassIni
for (year in years) {
  StandingBiomass <- StandingBiomass + growth
  SoilFertility<- (0.1 * 2.^(StandingBiomass/K))- 0.145
  r<- 0.1 * SoilFertility
  growth <- r * StandingBiomass * (1 - (StandingBiomass/K))
  StandingBiomass<- StandingBiomass-15
  forest$growth[year] <- growth
  forest$biomass[year] <- StandingBiomass
}

