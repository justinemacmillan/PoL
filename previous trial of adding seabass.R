# load Julia Blanchard's NS model
NS_sp <- NS_species_params
NS_i <- NS_interaction
NS_sp_g <- NS_species_params_gears   


# Adding SeaBass to NS
# converting length to weight (a*length^b)(mizer cheatsheet says, if no a or b, use a=0.001 and b=3)
# values found in Fishbase (https://fishbase.mnhn.fr/summary/SpeciesSummary.php?ID=63&AT=European+Seabass)
Seabass_L_mat = 36.1
Seabass_L_max = 103
# Seabass_w_max = 12000

# TO DO : real w_max is 12kg, find the a and b values for European Seabass
# used the calculated max weight for consistency as maturity weight is not provided in fishbase

# Assigning values to a and b (length-weight equation) for European Seabass. Currently using default values provided in the mizer cheat sheet
a_SB <- 0.001
b_SB <- 3

# creating the Seabass MizerParams object
Seabass_params <- data.frame(species= "Seabass",
                             w_mat = Seabass_L_mat,
                             w_max = Seabass_L_max,
                             a = a_SB,
                             b = b_SB)
# calling the new NS with seabass (NS_S for North Sea + seabass)
NS_S_params <- addSpecies(NS_params,Seabass_params)
plotSpectra(NS_S_params)

# Check if axes are logged
x_axis_logged <- par()$xlog
y_axis_logged <- par()$ylog

# Check if axes are exponential
x_axis_exp <- all(diff(par()$usr[1:2]) > 0)
y_axis_exp <- all(diff(par()$usr[3:4]) > 0)

# Print the results
if (x_axis_logged) {
  cat("X-axis is logged.\n")
} else if (x_axis_exp) {
  cat("X-axis is on an exponential scale.\n")
} else {
  cat("X-axis is on a linear scale.\n")
}

if (y_axis_logged) {
  cat("Y-axis is logged.\n")
} else if (y_axis_exp) {
  cat("Y-axis is on an exponential scale.\n")
} else {
  cat("Y-axis is on a linear scale.\n")
}
# The axes are on an exponential scale
# Jon: is this bad? Should I change it to a log10 axis?

# Check that it's really Seabass
# Check growth 
df_growth <- data.frame(getEGrowth(NS_S_params))
# looks like after 97.8g size class there are no more Seabasses.
plotGrowthCurves(NS_S_params, species ="Seabass")
# growth curve shows that Seabass's maturity size is ~35g and 100g its maximum weight
# let's check what it should actually be...
# Seabass_L_mat = 36.1 ; Seabass_L_max = 103 ; Seabass_w_max = 12000
# a_SB <- 0.001 ; b_SB <- 3
# mat weight = 47.04g
a_SB*Seabass_L_mat^b_SB
# max weight = 1092.727g
a_SB*Seabass_L_max^b_SB
# Okay, so how did I code Seabass in? Because it's obviously not working
Seabass_params <- data.frame(species= "Seabass",
                             w_mat = Seabass_L_mat,
                             w_max = Seabass_L_max,
                             a = a_SB,
                             b = b_SB)



# TO DO: check that it truly is SeaBass
# TO DO: look at the single species plots

# TO DO: look at what mizer is doing on an individual level (von-Bertalanffy growth curve)
# TO DO: plot growth curves? in early mizer material
# TO DO: check that the curves look similar to that of a similar species (similar growth params on Fishbase, params table in mizer) and that live in similar area (pelagic, demersal)

# TO DO: see what happens when all interactions are default vs more based on real data
# TO DO: Observation data to tell mizer 

# TO DO: Long term - BH