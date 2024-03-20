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
    sp_NS <- c(sp_NS, "E.Seabass")
    
# Create a df to base to mizerParams on
  # species common names
    sp_NS <- data.frame(species = sp_NS)
    # add metadata which explains species choice in the species column
      comment(sp_NS$species) <- "We use the species choice made by Blanchard et al., 2014 https://doi.org/10.1111/1365-2664.12238 and add Sebass as it is predicted that the NS will become more suitable for it by Townhill et al., 2023 https://doi.org/10.1111/faf.12773"
  
  # add a column with the scientific names of each species
      sp_NS$latin_name <- c("Sprattus sprattus", # Sprat
                         "Ammodytes marinus", # Sandeel
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
      comment(sp_NS$latin_name) <- "We used the latin names provided by Blanchard et al., 2014 https://doi.org/10.1111/1365-2664.12238 and the latin name found in Fishbase page for European Seabass"

  # add asymptotic size (w_inf). Asymptotic size is the maximum w_inf, not an average
      # using rfishbase::species() to extract relevant information from Fishbase database
          max_size_fishbase <- rfishbase::species(sp_NS$latin_name) |>
              select(latin_name = Species, l_max = Length)
          
      # combining l_max df with names df
          max_size <- max_size_fishbase |>
            left_join(select(sp_NS, species, latin_name),
                      by = "latin_name")
          # Reorder the columns of max_size_fishbase
          max_size <- max_size %>%
            dplyr::select(species, latin_name, l_max)
          
          # add comments to the latin_name and l_max columns
          comment(max_size$latin_name) <- "We used the latin names provided by Blanchard et al., 2014 https://doi.org/10.1111/1365-2664.12238 and the latin name found in Fishbase page for European Seabass."
          
          comment(max_size$l_max) <- "We used rfishbase::species to gather the l_max information from the Fishbase dataset."
                          
      # a and b weight-length conversion
        length_weight <- rfishbase::estimate(sp_NS$latin_name, fields = c("Species", "a", "b"))
        
        # combining a&b df with l_max&names df
          sp_NS <- sp_NS |> 
            left_join(length_weight, by = c("latin_name" = "Species")) |> 
            left_join(max_size) |> 
            mutate(w_inf = a*l_max^b)
          
          # add comments to the columns
            comment(sp_NS$a) <- "Taken from the 'a' column in the 'estimates' table on Fishbase."
            comment(sp_NS$b) <- "Taken from the 'b' column in the 'estimates' table on Fishbase."
            comment(sp_NS$w_inf) <- "Calculated from 'l_max' using weight-length parameters 'a' and 'b'."
    
  # adding the growth parameters
    # extracting the age and length at maturity
      maturity_tbl <- rfishbase::maturity(sp_NS$latin_name) # provides many data points for each species
      median_maturity <- maturity_tbl |> 
        group_by(Species) |> 
        filter(!is.na(tm), !is.na(Lm)) |> 
        summarise(age_mat = median(tm),
                  l_mat = median(Lm))
      median_maturity
    
    # adding the age, length and weight at maturity to the sp_NS df
      sp_NS <- sp_NS |> 
        left_join(median_maturity, by = c("latin_name" = "Species")) |> 
        mutate(w_mat = a*l_mat^b)
      
      # add comments
        comment(sp_NS$l_mat) <- "Median of `Lm` over all observations on the 'maturity' table on FishBase that had both `Lm` and `tm`."
        comment(sp_NS$age_mat) <- "Median of `tm` over all observations on the 'maturity' table on FishBase that had both `Lm` and `tm`."
        comment(sp_NS$w_mat) <- "Calculated from `l_mat` using weight-length parameters `a` and `b`."
        
    # calculating the h variable
      sp_NS$h <- get_h_default(sp_NS)
      comment(sp_NS$h) <- "Calculated from 'age_mat' and 'w_mat' using 'get_h_default()'."
      
  # Predation kernel
    # finding beta (PPMR) and sigma (width of predation kernel) values
      beta_sigma <- select(NS_species_params, species, beta, sigma)
      
    # for the mean time: will use same values as cod for Seabass, until I can find better ones (TO DO)
    # TO DO: find a paper that says that cod and Seabass are similar fish in modelling terms
      
      # Create a new row for E.Seabass with beta and sigma values equal to Cod's values
      S_pred_kernel <- data.frame(species = "E.Seabass",
        beta = beta_sigma$beta[which(beta_sigma$species == "Cod")],
        sigma = beta_sigma$sigma[which(beta_sigma$species == "Cod")])
      
      # Add the new row to the existing data frame
      beta_sigma <- rbind(beta_sigma, S_pred_kernel)
      
    # adding the predation kernel values to our df

        # Adding the predation kernel values to our df
        sp_NS <- sp_NS %>%
        left_join(beta_sigma, by = "species")
        
    # comment on the sigma and beta columns
        comment(sp_NS$beta) <- comment(sp_NS$sigma) <- "Taken from Blanchard et al., 2014 https://doi.org/10.1111/1365-2664.12238, E.Seabass values coppied from cod as they are similar species."
    
  # Abundances
    # use an average of real-world conditions as ecosystems are said to fluctuate around the steady state See https://mizer.course.nov22.sizespectrum.org/build/find-species-parameters.html#abundances
      
    # stock assessments are available for the NS, however the data comes with its own assumptions because they are single species estimates
      
  # Species interaction matrix
    NS_int <- NS_params@interaction
    
    # Find the row index for "Cod"
    cod_row_index <- which(rownames(NS_int) == "Cod")
    cod_col_index <- which(colnames(NS_int) == "Cod")
    
    # Copy the row for "Cod" to create a new row for "E.Seabass"
    Seabass_row <- t(data.frame(NS_int[cod_row_index, ]))
    Seabass_column <- data.frame(NS_int[,cod_col_index])
    
    # Replace the species name with "E.Seabass"
    Seabass_row <- data.frame(Seabass_row)
    rownames(Seabass_row) <- "E.Seabass"
    colnames(Seabass_column) <- "E.Seabass"
    
    # bind new Seabass interaction column to Blanchard's intercation matrix
    NS_int <- cbind(NS_int, Seabass_column)
    
    # create a new cell in the row for E.Seabass * E.Seabass
    print(Seabass_row)
    # Create a new column in Seabass_col with 1 as default interaction value
      s1 <- Seabass_row$E.Seabass <- 1
      s1 <- Seabass_row
    
    # bind the Seabass row to the interactions matrix
    NS_int <- rbind(NS_int, s1)

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
      