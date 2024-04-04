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
library(rfishbase) # to extract fish data


#### Seabass params df ####
  NS_S_params <- NS_species_params_gears
  NS_S_params 
  # we want to create a new species called E.Seabass
    # For the moment E.Seabass has the same w_max, w_mat, beta, sigma, k_vb, gear, R_max, and w_inf
      class(NS_S_params)
        # NS_S_params is a df with 12 observations (species) and 9 variables
      # create a row 
        Seabass <- NS_S_params[nrow(NS_S_params)+1,]
      # Copy all values from Cod to E.Seabass
        Seabass <- NS_S_params[11,]
        rownames(Seabass) <- "13"
        Seabass[1,1] <- "E.Seabass"
      # bind Seabass row to NS_S_params df
        NS_S_params <- rbind(NS_S_params, Seabass)     
        NS_S_params 
        
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
          class(NS_S_params$gear)
            # gear is a character variable 
          Seabass_values[,"gear"] <- "Otter"
        
    # Let us use the Seabass_values in our NS_S_params dataset to make it more accurate.
        for (col_name in names(NS_S_params)) {
          # Check if the column name exists in Seabass_values
          if (col_name %in% names(Seabass_values)) {
            # Replace the value in NS_S_params with the corresponding value from Seabass_values
            NS_S_params[nrow(NS_S_params), col_name] <- Seabass_values[[col_name]]            
          }
        }
        NS_S_params
        
        # TO DO: find the Seabass values for: beta, sigma, and R_max
        # TO DO: am I going to change the initial abundances? (see # Abundances in the mizer course version)

#### Seabass interaction matrix ####
        
    inter  
  
  # make a new matrix to fill in with the pre-existing values and the new E.Seabass
    Seabass_inter <- matrix(0, nrow = 13, ncol = 13)
  
  # Copy the existing interaction matrix Seabass_inter to the top-left part of the new matrix
    Seabass_inter[1:12, 1:12] <- inter
  
  # Copy the values for Cod into the new E.Seabass column and row 
    Seabass_inter[13, ] <- Seabass_inter[11,]
    Seabass_inter[, 13] <- Seabass_inter[,11]
    Seabass_inter

  # Rename the columns and rows
    row.names(Seabass_inter) <- colnames(Seabass_inter) <- c(row.names(inter), "E.Seabass")
    
  # TO DO: find the interaction values for Seabass
    
#### Gear df ####
# Gear parameters
NS_gears <- NS_params@gear_params    
  # Seabass is typically caught by otter trawling https://www.mcsuk.org/goodfishguide/ratings/wild-capture/992/ ; minimum catch size in UK is 42 cm https://www.gov.uk/government/publications/bass-industry-guidance-2023/bass-fishing-guidance-2023#:~:text=The%20minimum%20size%20of%20European,by%20fixed%20or%20drift%20nets.

  # Create a new row with the provided information
    S_gears <- data.frame(
      gear = "Otter",
      species = "E.Seabass",
      sel_func = "knife_edge",
      knife_edge_size = 42,
      catchability = 1)


  # Add the new row to the existing dataframe
    NS_gears <- rbind(NS_gears, S_gears)
  
  # Change the last row name to "E.Seabass, Otter"
    rownames(NS_gears)[nrow(NS_gears)] <- "E.Seabass, Otter"
  
  # Print the updated dataframe
    print(NS_gears)

# Seems to be that (TO DO: find ref) for knife-edge fishing L25 and L50 values are not required

#### making the MizerParams object ####
  # create a MizerParams object
    NS_S_params <- mizer::newMultispeciesParams(NS_S_params)
  
  #run to steady state
    NS_S_params <- steady(NS_S_params)

#### Making the MizerSim Object ####
# create MizerSim     
NS_S_sim <- project(NS_S_params)

#### plotting NS_S ####
# plots
plot(NS_S_sim)
# plotGrowthCurves(NS_S_params, species = "E.Seabass")
plot(NS_sim)

plot(NS_S_sim, species="E.Seabass")
  # Biomass (g) graph seems to say that there's loads of Seabass up to year 60 when it starts dropping drasticly until there is no more Seabass at 100 years. Seabass max age is 30 so how is this possible? or is the species going extinct?
  # Also why is Seabass not subjected to any Fing pressure? I added the gear values? Do I need to add Effort values?
plot(NS_S_sim, species="Cod")
  # why is Cod's B density flipped?
plot(NS_sim, species = "Cod")
