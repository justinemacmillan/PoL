# addSpecies 
  NS_S <- addSpecies(NS_sim@params, SB_params, SB_gear, SB_initial_effort, inter)
# change line colour to make SB more visible
  species_params(NS_S)["E.Seabass", "linecolour"] <- "darkorange1"
  
initialN(NS_S) <- as.matrix(NS_S@initial_n[13,])

NS_S_steady <- steady(NS_S)
  
NS_S_def0 <- project(NS_S, t_max = 200, effort = 0)
NS_S_def0 <- project(NS_S_steady, t_max = 200, effort = 0)


NS_S_def0@params@initial_n[1:12,] <- NS_S_def0@params@initial_n[13,]
class(initialN(NS_S_def0))


plotBiomass(NS_sim_steady0)
plotSpectra(NS_sim_steady0)
plotBiomass(NS_S_def0)
plotSpectra(NS_S_def0)
animateSpectra(NS_sim_steady0)


# Assuming NS_S@initial_n is your original initial_n matrix and 
  # it's correctly dimensioned
# Get the number of species
num_species <- dim(NS_S@initial_n)[1]

# Replicate the initial_n values of species 13 across all species
replicated_initial_n <- matrix(rep(NS_S@initial_n[13, ], num_species), nrow = num_species, byrow = TRUE)

# Assign the replicated matrix back to initial_n
initialN(NS_S) <- replicated_initial_n
