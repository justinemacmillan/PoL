#### Background ####
# CC will cause temperatures to increase, as a result, the North Sea (NS) will become less favourable for Atlantic cod and saithe; and more favourable for European seabass, sardines, and anchovies (Townhill et al., 2023) 

# Idea: What if, instead of waiting for Seabass to migrate to NS, we introduced it artificially.

# Models: NS; NS + seabass; NS + temperature; NS + seabass + temperature
# Based on Julia Blanchard's NS model (90% weight of tot catch)

# Question: how will adding seabass and/or increasing the temperature change the food web and ecosystem structure in the NS? Will the seabass compete with the cod?

# hypothesis: Seabass and cod are both opportunistic piscivores, possible that they will eat each other more.

#### load the required libraries ####
library(mizer) # where we will be doing the majority of our coding
library(tidyverse) # to analyse and plot data
library(rfishbase) # to extract fish data. Takes 15-30 minutes to install first time.

#### Seabass params df ####
NS_S_values <- NS_species_params_gears
NS_S_values 

# we want to create a new species called E.Seabass. We will be following the advice from the mizer course - Collect parameters (https://mizer.course.sizespectrum.org/build/collect-parameters.html)
class(NS_S_values)
# NS_S_values is a df with 12 observations (species) and 9 variables

# start by duplicating Cod's values and assign them to our new species
# create a row 
Seabass <- NS_S_values[nrow(NS_S_values)+1,]
# Copy all values from Cod to E.Seabass
Seabass <- NS_S_values[11,]
rownames(Seabass) <- "13"
Seabass[1,1] <- "E.Seabass"
# bind Seabass row to NS_S_values df
NS_S_values <- rbind(NS_S_values, Seabass)     
NS_S_values 

# For the moment E.Seabass has the same w_max, w_mat, beta, sigma, k_vb, gear, R_max, and w_inf as Cod
# We now want to find E.Seabass's params values

# w_max and w_inf
# w_inf = Asymptotic size is the maximum w_inf, not an average
# E.Seabass's latin name
latin_name <- "Dicentrarchus labrax"
# using rfishbase::species() to extract relevant information from Fishbase database
max_size_fishbase <- rfishbase::species(latin_name) |>
  select(latin_name = Species, l_max = Length)
# a and b weight-length conversion
length_weight <- rfishbase::estimate(latin_name, fields = c("Species", "a", "b"))
# Combining the l_max, a, and b values in a tibble & calculating w_inf
Seabass_values <- max_size_fishbase |> 
  left_join(length_weight, by = c("latin_name" = "Species")) |> 
  mutate(w_max = a*l_max^b)
# The recorded maximum weight for Seabass is 12kg and the weight that we get from the l_max conversion is 11.18841kg. We will use the l_max conversion weight as w_max and 12kg as w_inf, as the later is supposed to represent the absolute maximum weight Seabass can reach.
Seabass_values[,"w_inf"] <- 12000

# w_mat
w_mat <- rfishbase::maturity(latin_name) # provides many data points for each species

# Let's calculate the median age and length at maturity
w_mat <- w_mat |>  
  filter(!is.na(tm), !is.na(Lm)) |> 
  summarise(age_mat = median(tm),
            l_mat = median(Lm))
w_mat

w_mat <- add_column(w_mat, Species= "Dicentrarchus labrax")

# let's add these values to our Seabass_values tibble
Seabass_values <- Seabass_values |> 
  left_join(w_mat, by = c("latin_name" = "Species")) |> 
  mutate(w_mat = a*l_mat^b)

# k_vb
# (ICES & S. Vandamme, 2012) [https://archimer.ifremer.fr/doc/00124/23485/21314.pdf]
# p.63 "sea bass tagged near the Channel Islands in VIIe (south of Hurd Deep) moved as far as the southern North Sea"
# p.91 the K value for Seabass in Area VIIe is 0.07697
Seabass_values[,"k_vb"] <- 0.07697

# gear
# (ICES & S. Vandamme, 2012) [https://archimer.ifremer.fr/doc/00124/23485/21314.pdf]
# p.69 most Seabass caught in Area VIIe is caught through line fishing
class(NS_S_values$gear)
# gear is a character variable 
Seabass_values[,"gear"] <- "Otter"

# R_max
# R_max is not a value that one can find, instead it's necessary to tune the model's resilience (email Gustav Delius, 4.04.24)
# https://mizer.course.sizespectrum.org/build/tune-resilience.html

# beta & sigma predation kernel values
# Gustav Delius and James Rimmer are currently developing a technique to extract these values from stomach data. Results not available at time of writing (email Gustav Delius, 4.04.24)
# kept Cod's values.

# Let us use the Seabass_values in our NS_S_values dataset to make it more accurate.
for (col_name in names(NS_S_values)) {
  # Check if the column name exists in Seabass_values
  if (col_name %in% names(Seabass_values)) {
    # Replace the value in NS_S_values with the corresponding value from Seabass_values
    NS_S_values[nrow(NS_S_values), col_name] <- Seabass_values[[col_name]]            
  }
}
NS_S_values

# TO DO: find the Seabass values for: beta, sigma, and R_max
# TO DO: am I going to change the initial abundances? (see # Abundances in the mizer course version)
# TO DO: do Cod and Seabass predate on similar foods?
#### Seabass interaction matrix ####

# Start by looking at what Blanchard et al., 2014's interaction matrix looks like
inter  

# Let's add Seabass to this interaction matrix (using Cod's interaction values)
# DR. Gustav Delius and Postdoc James Rimmer are currently developing a technique to get the interaction matrix values from stomach data. 
# This is not yet available at the time of coding and write-up, they plan to start having results in mid-May '24. (email Gustav Delius 04.04.24) 

# make a new matrix to fill in with the pre-existing values and the new E.Seabass
NS_S_interaction <- matrix(0, nrow = 13, ncol = 13)

# Copy the existing interaction matrix NS_S_interaction to the top-left part of the new matrix
NS_S_interaction[1:12, 1:12] <- inter

# Copy the values for Cod into the new E.Seabass column and row 
NS_S_interaction[13, ] <- NS_S_interaction[11,]
NS_S_interaction[, 13] <- NS_S_interaction[,11]
NS_S_interaction

# Rename the columns and rows
row.names(NS_S_interaction) <- colnames(NS_S_interaction) <- c(row.names(inter), "E.Seabass")

#### Gear df ####
# Gear parameters
NS_gears <- NS_params@gear_params    

# Seabass is typically caught by line fishing https://archimer.ifremer.fr/doc/00124/23485/21314.pdf
# Seabass minimum catch size in UK is 42 cm https://www.gov.uk/government/publications/bass-industry-guidance-2023/bass-fishing-guidance-2023#:~:text=The%20minimum%20size%20of%20European,by%20fixed%20or%20drift%20nets.

# Create a new row with the provided information
S_gears <- data.frame(
  gear = "Line",
  species = "E.Seabass",
  sel_func = "knife_edge",
  knife_edge_size = 42,
  catchability = 1)

# Add the new row to the existing dataframe
NS_S_gears <- rbind(NS_gears, S_gears)

# Change the last row name to "E.Seabass, Line"
rownames(NS_S_gears)[nrow(NS_S_gears)] <- "E.Seabass, Line"

# Print the updated dataframe
print(NS_S_gears)

# Seems to be that (TO DO: find ref) for knife-edge fishing L25 and L50 values are not required
# Change Seabass sel_func to be sigmoidal

#### making the MizerParams object ####
# create a MizerParams object
# not steady state
  NS_S_params_notSteady <- mizer::newMultispeciesParams(species_params = NS_S_values,
                                              gear_params = NS_S_gears,
                                              interaction = NS_S_interaction, 
                                              initial_effort = 1)
  NS_S_sim_notSteady <- project(NS_S_params_notSteady, effort = 0, progress_bar = TRUE)

# Steady state
  NS_S_params <- mizer::newMultispeciesParams(species_params = NS_S_values,
                                              gear_params = NS_S_gears,
                                              interaction = NS_S_interaction, 
                                              initial_effort = 1)
  NS_S_params <- steady(NS_S_params)
  NS_S_sim <- project(NS_S_params, effort = 1, progress_bar = TRUE)


# # Add metadata to the model
# NS_S_params <- setMetadata(NS_S_params,
#                            title = "North Sea model with Seabass added")
# 
# #run to steady state
# NS_S_params <- steady(NS_S_params)
# # Let's see what our Biomass density curve looks like
# plotlySpectra(NS_S_params, power = 2)
# # Seabass has a rather low abundance compared to other species
# 
# # Calibrate the model
# # Calibration had been done by Blanchard et al., 2014 so we didn't recalibrate in order to be able to compare results between NS and NS_S models
# 
# #### Making the MizerSim Object ####
# # create MizerSim     
# NS_S_sim <- project(NS_S_params, effort = 1, progress_bar = TRUE)
# 
# #### plotting NS_S ####
# # plots
# plot(NS_S_sim)
# animate
# plotGrowthCurves(NS_S_params, species = "E.Seabass")
# 
# sim <- project(NS_params, effort = 1)
# plot(sim)
# plot(NS_S_sim, species="E.Seabass")
# # Biomass (g) graph seems to say that there's loads of Seabass up to year 60 when it starts dropping drasticly until there is no more Seabass at 100 years. Seabass max age is 30 so how is this possible? or is the species going extinct?
# # Also why is Seabass not subjected to any Fing pressure? I added the gear values? Do I need to add Effort values?
# plot(NS_S_sim, species="Cod")
# # why is Cod's B density flipped?
# plot(NS_sim, species = "Cod")
# plotDiet(NS_S_params)

# To do: NS_S
# Predation kernel default is 30, does that seem reasonable or should I stick to Cod values?
# To do: Am I including OBserved biomass and Biomass cutoff values?
# To do: create a line fishing technique specifically for Seabass
# To do: in first version, catchability of S is 1 for line and 0 for trawl
# To do: in second version, allow for some bycatch for line and trawl fishing techniques (e.g. some lines will catch N. pout and some trawls will catch S)
# To do: Follow the create your first model steps (calibrate and match B and g)
# To do: Refine your model
# To do: Match landings data
# To do: Tune resilience

# To do: analyse the model once it's created
# To do: number density, biomass density (log weights?)-> can I fit the power law regression (does adding Seabass remove the power law?)
# To do: make a single species model of Seabass (using the values from NS_S to see what the ss model looks like)
# To do: any change in the maturity curve? (day 2)
# To do: does this mention Seabass's PPMR? (https://academic.oup.com/icesjms/article/70/2/452/797965)
# To do: Find proof that Cod and Seabass are similar in modelling terms
# To do: plot and compare feeding levels
# To do: plot predation and Fing mort, plot diet
# To do: plotlyDeath()
# To do: Seabass is never preyd upon -> fix that
# To do: Any trophic cascades?
# To do: Any fishing induced cascades?
# To do: modify following code to show the differences between NS and NS_S
# We compare the steady states
# plotSpectra2(mp_lessRes_steady, name1 = "less resource", 
#              mp, name2 = "original",
#              total = TRUE, power = 2,
#              ylim = c(1e-8, NA), wlim = c(1e-3, NA))
# To do: Comparing ecosystem states: effects of fishing relative to an unfished state (w3d4)
# To do: What if we only have catch data? (w3d4)
# To do: Set up your own fishing scenario: simpler comparison of steady states (w3d4)


