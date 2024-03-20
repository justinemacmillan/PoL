install.packages("icesDatras")
library(icesDatras)
getDATRAS(record = "HH",
          survey="NS-IBTS",
          years = 2013:2023,
          quarters = 1:4)


# Load the icesDatras library
library(icesDatras)

# Define the survey code for NS-IBTS
survey_code <- "NS-IBTS"

# Define the years range
years <- 2014:2023

# Extract species names from the "species" column of the data frame sp_NS
species_names <- sp_NS$species

# Initialize a list to store abundance data for each species
abundance_data <- list()

# Loop through each species and extract abundance data
for (species in species_names) {
  # Download abundance data for the specified species, survey, and years
  data <- get_datras_data(survey = survey_code, species = species, year = years)
  
  # Extract abundance (biomass observed) data
  abundance <- data$BIO
  
  # Store abundance data in the list
  abundance_data[[species]] <- abundance
}

# Print abundance data for each species
for (species in species_names) {
  print(paste("Abundance data for", species))
  print(abundance_data[[species]])
}


lsf.str("package:dplyr")
ls("package:icesDatras")
search()


# Load the icesDatras package
library(icesDatras)

# Define the survey code for NS-IBTS
survey_code <- "NS-IBTS"

# Define the years of interest
years <- 2014:2023

# Get the species codes corresponding to the species names in sp_NS
species_codes <- get_species_codes(species = sp_NS$species)

# Initialize a list to store abundance data for each species
abundance_data <- list()

# Loop through each species and extract abundance data
for (i in seq_along(sp_NS$species)) {
  species_name <- sp_NS$species[i]
  species_code <- species_codes[i]
  
  # Download abundance data for the species from NS-IBTS survey
  abundance <- getCatchWgt(survey = survey_code, species = species_code, year = years)
  
  # Add species name as a column to the abundance data
  abundance$species <- species_name
  
  # Store abundance data for the species in the list
  abundance_data[[i]] <- abundance
}

# Combine abundance data for all species into a single data frame
abundance_all <- do.call(rbind, abundance_data)

# View the combined abundance data
print(abundance_all)


# Load the icesDatras package
library(icesDatras)

# Define the survey code for NS-IBTS
survey_code <- "NS-IBTS"

# Define the years of interest
years <- 2014:2023

# Initialize a list to store abundance data for each species
abundance_data <- list()

# Loop through each species in sp_NS and extract abundance data
for (species_name in sp_NS$species) {
  
  # Assuming species names in sp_NS match DATRAS data, use the species name directly
  # You may need to adjust this if there are discrepancies between species names in sp_NS and DATRAS
  species_code <- species_name
  
  # Download abundance data for the species from NS-IBTS survey
  abundance <- getCatchWgt(survey = survey_code, species = species_code, year = years)
  
  # Add species name as a column to the abundance data
  abundance$species <- species_name
  
  # Store abundance data for the species in the list
  abundance_data[[species_name]] <- abundance
}

# Combine abundance data for all species into a single data frame
abundance_all <- do.call(rbind, abundance_data)

# View the combined abundance data
print(abundance_all)

# Load the icesDatras package
library(icesDatras)

# Define the survey code for NS-IBTS
survey_code <- "NS-IBTS"

# Define the years of interest
years <- 2014:2023

# Initialize a list to store abundance data for each species
abundance_data <- list()

# Loop through each species in sp_NS and extract abundance data
for (species_name in sp_NS$species) {
  
  # Assuming species names in sp_NS match DATRAS data, use the species name directly
  # You may need to adjust this if there are discrepancies between species names in sp_NS and DATRAS
  species_code <- species_name
  
  # Download abundance data for the species from NS-IBTS survey
  abundance <- getCatchWgt(survey = survey_code, year = years, quarters = 1:4)
  
  # Filter abundance data for the current species
  abundance_species <- abundance[abundance$species == species_name, ]
  
  # Store abundance data for the species in the list
  abundance_data[[species_name]] <- abundance_species
}

# Combine abundance data for all species into a single data frame
abundance_all <- do.call(rbind, abundance_data)

# View the combined abundance data
print(abundance_all)
