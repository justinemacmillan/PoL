# load mizer
library(mizer)

# load NS model
  NS_cod_params <- NS_species_params_gears
  NS_cod_params  
  
# we want to create a new species called Cod2
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
    
# Now that we have our cod2, we want to simulate this new model
  project(NS_cod_params)
  