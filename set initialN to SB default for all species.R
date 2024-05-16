#### set up ####
library(mizer)
library(readxl)
library(dplyr)
# load parameters 
# sea bass species parameters (species, w_mat, w_mat25, w_max, and k_vb are sea bass specific, the rest of the values have been duplicated from cod)
SB_params <- read_excel("Cod=Seabass/SB_params.xlsx")
# sea bass fishing gear (values duplicated from cod)
SB_gear <- read_excel("Cod=Seabass/SB_gear.xlsx")
# sea bass initial fishing effort (values duplicated from cod)
SB_initial_effort <- read_excel("Cod=Seabass/SB_initial_effort.xlsx")
SB_initial_effort <- apply(SB_initial_effort, 2, as.numeric)
# sea bass interaction (values duplicated from cod)
inter <- read_excel("Cod=Seabass/inter.xlsx")
row_names <- inter[[1]]
inter <- inter[, -1]
inter <- apply(inter, 2, as.numeric)
inter <- as.matrix(inter)
rownames(inter) <- row_names
# North Sea + sea bass historical fishing effort values (sea bass values duplicatd from cod)
effort_NS_S <- matrix(nrow = 44, ncol = 13)
effort_NS_S[, 1:12] <- NS_sim@effort
effort_NS_S[, 13] <- effort_NS_S[, 11]
colnames(effort_NS_S) <- row_names
rownames(effort_NS_S) <- rownames(NS_sim@effort)
# North Sea and sea bass initialN (sea bass initial abundance is set to be at 1/10th of cod's initialN)
NS_initialN <- NS_sim@params@initial_n
SB_initialN <- read_excel("Cod=Seabass/initialN.xlsx")
SB_initialN <- select(SB_initialN, -"...1")
rownames(SB_initialN) <- "E.Seabass"
colnames(SB_initialN) <- colnames(NS_initialN)
NS_S_initialN <- rbind(NS_initialN, SB_initialN)
NS_S_initialN <- as.matrix(NS_S_initialN)

#### initialN same for all ####
# addSpecies 
  NS_S <- addSpecies(NS_sim@params, SB_params, SB_gear, SB_initial_effort, inter)
# change line colour to make SB more visible
  species_params(NS_S)["E.Seabass", "linecolour"] <- "darkorange1"
  
# setting SB's initialN to be the same for all species
  # Get the number of species
  num_species <- dim(NS_S@initial_n)[1]
  
  # Replicate the initial_n values of species 13 across all species
  replicated_initial_n <- matrix(rep(NS_S@initial_n[13, ], num_species), nrow = num_species, byrow = TRUE)
  
  # Assign the replicated matrix back to initial_n
  initialN(NS_S) <- replicated_initial_n
  
# Next we will set SB's initialN to species in the NS (no SB) model
  # extract MizerParams from NS_sim
    NS <- NS_sim@params
  # setting SB's initialN to be the same for all species
  # Get the number of species
    num_species_NS <- dim(NS_S@initial_n)[1]
  
  # Replicate the initial_n values of species 13 across all species
    initialN_NS <- matrix(replicated_initial_n[1:12,], nrow=12, ncol=100)
  
  # Assign the replicated matrix back to initial_n
    initialN(NS) <- initialN_NS
#### Project to MizerSims (steady and unsteady ; F=0,1,2) ####
    
# NS_S (unsteady)
  NS_0 <- project(NS, t_max = 20, effort = 0)
  NS_1 <- project(NS, t_max = 20, effort = 1)
  NS_2 <- project(NS, t_max = 20, effort = 2)
    
# NS_S_steady (MizerParams: steady() then project())
  NS_steady <- steady(NS)
  NS_steady_0 <- project(NS_steady, t_max = 20, effort = 0)
  NS_steady_1 <- project(NS_steady, t_max = 20, effort = 1)
  NS_steady_2 <- project(NS_steady, t_max = 20, effort = 2)
    
# NS_S (unsteady)
  NS_S_0 <- project(NS_S, t_max = 20, effort = 0)
  NS_S_1 <- project(NS_S, t_max = 20, effort = 1)
  NS_S_2 <- project(NS_S, t_max = 20, effort = 2)
  
# NS_S_steady (MizerParams: steady() then project())
  NS_S_steady <- steady(NS_S)
  NS_S_steady_0 <- project(NS_S_steady, t_max = 20, effort = 0)
  NS_S_steady_1 <- project(NS_S_steady, t_max = 20, effort = 1)
  NS_S_steady_2 <- project(NS_S_steady, t_max = 20, effort = 2)
  
####  Plot results ####  
  # Unsteady F=0 (NS)
  plotBiomass(NS_0)
  plotSpectra(NS_0)
  
  # Unsteady F=1 (NS)
  plotBiomass(NS_1)
  plotSpectra(NS_1)
  
  # Unsteady F=2 (NS)
  plotBiomass(NS_2)
  plotSpectra(NS_2)
  
  # Steady F=0 (NS)
  plotBiomass(NS_steady_0)
  plotSpectra(NS_steady_0)
  
  # Steady F=1 (NS)
  plotBiomass(NS_steady_1)
  plotSpectra(NS_steady_1)
  
  # Steady F=2 (NS)
  plotBiomass(NS_steady_2)
  plotSpectra(NS_steady_2)
  
  # Unsteady F=0 (NS_S)
    plotBiomass(NS_S_0)
    plotSpectra(NS_S_0)

  # Unsteady F=1 (NS_S)
    plotBiomass(NS_S_1)
    plotSpectra(NS_S_1)
    
  # Unsteady F=2 (NS_S)
    plotBiomass(NS_S_2)
    plotSpectra(NS_S_2)

  # Steady F=0 (NS_S)
    plotBiomass(NS_S_steady_0)
    plotSpectra(NS_S_steady_0)
    
  # Steady F=1 (NS_S)
    plotBiomass(NS_S_steady_1)
    plotSpectra(NS_S_steady_1)
    
  # Steady F=2 (NS_S)
    plotBiomass(NS_S_steady_2)
    plotSpectra(NS_S_steady_2)


