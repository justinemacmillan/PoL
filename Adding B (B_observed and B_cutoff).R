# Abundance
# we need the abundance values to be able to calibrate our model further down the line.
# Couldn't find where the biomass_observed and biomass_cutoff values so I will have to provide these for all species
plotBiomassVsSpecies(NS_params)

# Used stock assessment data (https://standardgraphs.ices.dk/stockList.aspx), chose stock assessments in the North Sea with the highest possible data category ranking
# Create the new columns
NS_S_values$biomass_observed <- NA
NS_S_values$biomass_cutoff <- NS_S_values$w_mat

# Sprat https://standardgraphs.ices.dk/ViewCharts.aspx?key=17739
NS_S_values$biomass_observed[1] <- 186017
# Sandeel https://standardgraphs.ices.dk/ViewCharts.aspx?key=17718
NS_S_values$biomass_observed[2] <- 229027
# N.pout https://standardgraphs.ices.dk/ViewCharts.aspx?key=18423
NS_S_values$biomass_observed[3] <- 119524
# Herring https://standardgraphs.ices.dk/ViewCharts.aspx?key=17752
NS_S_values$biomass_observed[4] <- 1689588
# Dab https://standardgraphs.ices.dk/ViewCharts.aspx?key=18184
NS_S_values$biomass_observed[5] <- 
  # Whiting https://standardgraphs.ices.dk/ViewCharts.aspx?key=17795
  NS_S_values$biomass_observed[6] <- 201745
# Sole https://standardgraphs.ices.dk/ViewCharts.aspx?key=17901
NS_S_values$biomass_observed[7] <- 49008
# Gurnard https://standardgraphs.ices.dk/ViewCharts.aspx?key=18403
NS_S_values$biomass_observed[8] <- 
  # Plaice https://standardgraphs.ices.dk/ViewCharts.aspx?key=17820
  NS_S_values$biomass_observed[9] <- 682358
# Haddock https://standardgraphs.ices.dk/ViewCharts.aspx?key=17902
NS_S_values$biomass_observed[10] <- 198344
# Cod https://standardgraphs.ices.dk/ViewCharts.aspx?key=18223
NS_S_values$biomass_observed[11] <- 9032
# Saithe https://standardgraphs.ices.dk/ViewCharts.aspx?key=17807
NS_S_values$biomass_observed[12] <- 229071
# E.Seabass https://standardgraphs.ices.dk/ViewCharts.aspx?key=18142
NS_S_values$biomass_observed[13] <- 16326