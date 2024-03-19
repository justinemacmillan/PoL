# Background:
    # CC will cause temperatures to increase, as a result, the North Sea (NS) will become less favourable for Atlantic cod and saithe; and more favourable for European seabass, sardines, and anchovies (Townhill et al., 2023) 
    
    # Idea: What if, instead of waiting for Seabass to migrate to NS, we introduced it artificially.
    
    # Models: NS; NS + seabass; NS + temperature; NS + seabass + temperature
    # Based on Julia Blanchard's NS model (90% weight of tot catch)
    
    # Question: how will adding seabass and/or increasing the temperature change the food web and ecosystem structure in the NS? Will the seabass compete with the cod?
    
    # hypothesis: Seabass and cod are both opportunistic piscivores, possible that they will eat each other more.

# load the required libraries
  library(mizer) # where we will be doing the majority of our coding
  library(tidyverse) # to analyse and plot data
  library(rfishbase) # to extract fish data
 
# choose species to include
  # will base it on Blanchard et al., 2014's North Sea model 
    sp_NS <- NS_species_params$species
    
  # we will include European Seabass following Townhill et al., 2023 which states that the NS will become more habitable for them
    sp_NS <- c(sp_NS, "E. Seabass")
    
# Create a df to base to mizerParams on
  # base df: has species names
    sp_NS <- data.frame(species = sp_NS)
    # add metadata which explains species choice in the species column
      comment(sp_NS$species) <- "We use the species choice made by Blanchard et al., 2014 https://doi.org/10.1111/1365-2664.12238 and add Sebass as it is predicted that the NS will become more suitable for it by Townhill et al., 2023 https://doi.org/10.1111/faf.12773"
  
  # add a column with the scientific names of each species
      sp_NS$latin_name <- c("Sprattus sprattus", # Sprat
                         "Ammodutyes marinus", # Sandeel
                         "Trisopterus esmarkii", # Norway Pout
                         "Clupea harengus", # Herring
                         "Limanda limanda", # Common Dab
                         "Merlangius merlangus", # Whiting
                         "Solea solea", # Sole
                         "Eutrigla gurnardus", # Gurnard
                         "Pleuronectes platessa", # Plaice
                         "Melanogrammus aeglefinus", # Haddock
                         "Gadus morhua", # Cod
                         "Pollachius virens", # Saithe
                         "Dicentrarchus labrax") # European Seabass
      comment(sp_NS$latin_name) <- "used the latin names provided by Blanchard et al., 2014 https://doi.org/10.1111/1365-2664.12238 and the latin name found in Fishbase page for European Seabass"

  # add asymptotic size (w_inf). Asymptotic size is the maximum w_inf, not an average
      # using rfishbase::species() to extract relevant information from Fishbase database
          max_size_fishbase <- rfishbase::species(sp_NS[,2]) |>
            select(latin_name = Species, l_max = Length)
          
      # combining the df to have one master df
          max_size <- max_size_fishbase |>
            left_join(select(sp_NS, species, latin_name),
                      by = "latin_name")
          max_size <- bind_rows(max_size, max_size_fishbase) |>
            select(species, l_max)
          max_size
                          
                         
    
  
