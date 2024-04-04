# load mizer
library(mizer)

# load NS model
  NS_cod_params <- NS_species_params_gears
  NS_cod_params  
  
#### Cod2 params df  ####
  # we want to create a new species called Cod2 ###
  # Cod2 has the same w_max, w_mat, beta, sigma, k_vb, gear, R_max, and w_inf
  
  class(NS_cod_params)
    # NS_cod_params is a df with 12 observations (species) and 9 variables
  
  # create a row 
    Cod2 <- NS_cod_params[nrow(NS_cod_params)+1,]
    
    
    # Copy all values from Cod to Cod2
      Cod2 <- NS_cod_params[11,]
      rownames(Cod2) <- "13"
      Cod2[1,1] <- "Cod2"
    
  # bind Cod2 row to NS_cod_params df
    NS_cod_params <- rbind(NS_cod_params, Cod2)     
    NS_cod_params  
    

#### Cod2 interactions matrix ####   
  inter  
  
  # make a new matrix to fill in with the pre-existing values and the new Cod2
    Cod2_inter <- matrix(0, nrow = 13, ncol = 13)
  
  # Copy the existing interaction matrix Cod2_inter to the top-left part of the new matrix
    Cod2_inter[1:12, 1:12] <- inter
  
  # Copy the values for Cod into the new Cod2 column and row 
    Cod2_inter[13, ] <- Cod2_inter[11,]
    Cod2_inter[, 13] <- Cod2_inter[,11]
    Cod2_inter

  # Rename the columns and rows
  row.names(Cod2_inter) <- colnames(Cod2_inter) <- c(row.names(inter), "Cod2")
  
#### Gear df ####
  gear_Cod2 <- gear_params(NS_params)
  class(gear_Cod2)
  
  gear_Cod2 <- gear_Cod2[nrow(gear_Cod2)+1,]
  
  gear_Cod2 <- rbind(gear_params(NS_params), gear_Cod2)
  
  # Identify the row index corresponding to "Cod, Otter"
    Cod_row_index <- which(row.names(gear_Cod2) == "Cod, Otter")
  
  # Copy values from the identified row into the last row
    gear_Cod2[nrow(gear_Cod2), ] <- gear_Cod2[Cod_row_index, ]
    
  # Change the rowname and species name to Cod2
    last_row_index <- nrow(gear_Cod2)
    row.names(gear_Cod2)[last_row_index] <- "Cod2, Otter"
    
  # Update the species name in the last row to "Cod2"
    class(gear_Cod2[last_row_index, "species"])
      # the species col has factor values, let's add a Cod2 level
    gear_Cod2$species <- factor(gear_Cod2$species, levels = c(levels(gear_Cod2$species), "Cod2"))
    gear_Cod2[last_row_index, "species"] <- "Cod2"
  
  # Print the updated data frame
  print(gear_Cod2)
  
#### making the MizerParams object ####
  Cod_params <- newMultispeciesParams(NS_cod_params,
                                      interaction = Cod2_inter,
                                      kappa=9.27e10,
                                      gear_params = gear_Cod2)
  
#### Making the MizerSim object ####
  
  Cod_sim <- projectToSteady(Cod_params)
  
  plot(Cod_sim)
  plot(NS_sim)
  