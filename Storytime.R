################################################################################
# Modelling Sea Bass in the North Sea
# Justine Laura Macmillan
#
# Submitted in partial fulfillment of the requirements
# for the degree of Integrated Masters in Biology (MBiol)
# University of York
# Department of Biology
# May 2024
#
################################################################################
#
# Aim: to model Sea Bass in the North Sea, starting from the North Sea
# model produced by (Blanchard et al., 2014 doi: 10.1111/1365-2664.12238).
#
#
################################################################################
#
# Models:
# NS_sim_steady - the model produced by Blanchard et al., distributed with Mizer, run to steady state
# NS_S_his - NS_sim extended to include European sea bass. InitialN and effort are based on Blanchard's historical values
# NS_S_def - NS_sim extended to include European sea bass. InitialN is set to default and effort is set as a constant value which varies between 0, 1, and 2 (NS_S_def0, NS_S_def1, NS_S_def2).
#
################################################################################

# Background:
# CC will cause temperatures to increase, as a result, the North Sea (NS) will become a more favourable habitat for European sea bass (Townhill et al., 2023; DOI: 10.1111/faf.12773). A published model already exists of the Nort sea (Blanchard et al., 2014 doi: 10.1111/1365-2664.12238)

################################################################################

# Load required libraries
  library(mizer)
  library(mizerExperimental)
  library(readxl)
  library(dplyr)
  library(ggplot2)

# load parameters 
  # sea bass species parameters (species, w_mat, w_mat25, w_max, and k_vb are sea bass specific, the rest of the values have been duplicated from cod)
    SB_params <- read_excel("Cod=Seabass/SB_params.xlsx")
  # sea bass fishing gear (values duplicated from cod)
    SB_gear <- read_excel("Cod=Seabass/SB_gear.xlsx")
  # sea bass initial fishing effort (values duplicated from cod)
    SB_initial_effort <- read_excel("Cod=Seabass/SB_initial_effort.xlsx")
    SB_initial_effort <- apply(SB_initial_effort, 2, as.numeric)
  # sea bass interaction (values duplicated from cod)
    inter <- read_excel("Cod=Seabass/inter.xlsx")
    row_names <- inter[[1]]
    inter <- inter[, -1]
    inter <- apply(inter, 2, as.numeric)
    inter <- as.matrix(inter)
    rownames(inter) <- row_names
  # North Sea + sea bass historical fishing effort values (sea bass values duplicatd from cod)
    effort_NS_S <- matrix(nrow = 44, ncol = 13)
    effort_NS_S[, 1:12] <- NS_sim@effort
    effort_NS_S[, 13] <- effort_NS_S[, 11]
    colnames(effort_NS_S) <- row_names
    rownames(effort_NS_S) <- rownames(NS_sim@effort)
  # North Sea and sea bass initialN (sea bass initial abundance is set to be at 1/10th of cod's initialN)
    NS_initialN <- NS_sim@params@initial_n
    SB_initialN <- read_excel("Cod=Seabass/initialN.xlsx")
    SB_initialN <- select(SB_initialN, -"...1")
    rownames(SB_initialN) <- "E.Seabass"
    colnames(SB_initialN) <- colnames(NS_initialN)
    NS_S_initialN <- rbind(NS_initialN, SB_initialN)
    NS_S_initialN <- as.matrix(NS_S_initialN)

#### historical F effort (NS_S_his) ####
  # addSpecies() to add sea bass to Blanchard's North Sea model
    NS_S_params_his <- addSpecies(NS_sim@params, SB_params, SB_gear, SB_initial_effort, inter)
  # set initialN. initialN is set by Blanchard et al. for all original species and is set to 1/10th of cod's value for sea bass. This is because sea bass is less abundant than cod.
    initialN(NS_S_params_his) <- NS_S_initialN
      # Warning: The dimnames do not match. I will ignore them. 
      # in NS_S_params_his the dims are called $sp and $w. In NS_S_initialN they are called [[1]] and [[2]]. This does not affect the model and can be ignored.
  # modify sea bass's line colour to dark orange, to make it more visible
    species_params(NS_S_params_his)["E.Seabass", "linecolour"] <- "darkorange1"
  # run to steady state
    NS_S_params_his <- steady(NS_S_params_his)
  # project MizerParams to a MizerSim (NS_S_his)
    NS_S_his <- project(NS_S_params_his, t_max = 20, effort = effort_NS_S)

  # Supplementary figure 1: Sea bass is interacting with other species in the ecosystem
    # It has a normal looking size spectrum curve, with the emblematic bump after w_mat.
      plotSpectra(NS_S_his, species = "E.Seabass")
    # It is being fished
      plotFMort(NS_S_his, species = "E.Seabass")
    # It is eating fish in the ecosystem as well as the resource
      plotDiet(NS_S_his@params, species = "E.Seabass")
    # It is also getting eaten
      plotPredMort(NS_S_his, species = "E.Seabass")
    # Finally its growth curve is as expected. It reaches w_max before its maximum recorded age of 30 years.
      plotGrowthCurves(NS_S_his, species = "E.Seabass")

  # Extract NS_params from Blanchard's model NS_sim@params. Note used NS_sim@params as this is very different from NS_params. NS_sim@params values are more accurate are have been calibrated and tune using real-world data.
    NS_params <- NS_sim@params
  # set initialN to NS_params values
    initialN(NS_params) <- NS_sim@params@initial_n
  # run NS_params to steady state
    NS_params <- steady(NS_params)
  # project NS_params using historical fishing effort
    NS_sim_steady <- project(NS_params, t_max = 20, effort = NS_sim@effort)

  # Figure 2: comparing NS_sim_steady (control) and NS_S_his (with sea bass) using historical fishing effort values
    plotBiomass(NS_sim_steady) + custom_theme()
    plotSpectra(NS_sim_steady) + custom_theme()
    plotBiomass(NS_S_his) + custom_theme()
    plotSpectra(NS_S_his) + custom_theme()

#### default SB initialN and effort=0 ####
  # run to steady state and project NS_params with a constant fishing effort of 0. 
    NS_params_def <- NS_sim@params
    NS_params_def <- steady(NS_params_def)
    NS_sim_steady0 <- project(NS_params, t_max = 20, effort = 0)

  # use addSpecies() again to create a MizerParams containing blanchard' North sea species and SB
    NS_S_params_def <- addSpecies(NS_sim@params, SB_params, SB_gear, SB_initial_effort, inter)
  # change line colour to make sea bass lines more visible
    species_params(NS_S_params_def)["E.Seabass", "linecolour"] <- "darkorange1"
  # run to steady state. Note, we did not set sea bass's initialN so mizer will choose a default value.
    NS_S_params_def <- steady(NS_S_params_def)
  # project for 20 years at 0 fishing effort
    NS_S_def0 <- project(NS_S_params_def, t_max = 20, effort = 0)

  # Figure 3: SB biomass drops by several orders of magnitude. SB, cod, and saithe's size spectra show an unconventional curve.
    plotBiomass(NS_sim_steady0) + custom_theme()
    plotSpectra(NS_sim_steady0) + custom_theme()
    plotBiomass(NS_S_def0) + custom_theme()
    plotSpectra(NS_S_def0) + custom_theme()
  
  # Figure S3: screenshots taken every 2 years show that largest size class maintains the same biomass density, while size classes below drop in abundance. 
    animateSpectra(NS_sim_steady0)

#### default SB initialN and effort=1 ####
  # Same sequence of steps as default "SB initalN; Effort=0", changed fishing effort to 1
    NS_params <- steady(NS_sim@params)
    NS_sim_steady1 <- project(NS_params, t_max = 20, effort = 1)
    
    NS_S_def1 <- project(NS_S_params_def, t_max = 20, effort = 1)

  # Figure 4: SB biomass drops but only solightly, all size spectra look normal
    plotBiomass(NS_sim_steady1) + custom_theme()
    plotSpectra(NS_sim_steady1) + custom_theme()
    plotBiomass(NS_S_def1) + custom_theme()
    plotSpectra(NS_S_def1) + custom_theme()

#### default SB initialN and effort=2 ####
  # Same sequence of steps as default "SB initalN; Effort=0", changed fishing effort to 2
    NS_params <- steady(NS_sim@params)
    NS_sim_steady2 <- project(NS_params, t_max = 20, effort = 2)

    NS_S_def2 <- project(NS_S_params_def, t_max = 20, effort = 2)

  # Figure 5: SB biomass drops by 3 orders of magnitude, size spectra look normal for all species
    plotBiomass(NS_sim_steady2) + custom_theme()
    plotSpectra(NS_sim_steady2) + custom_theme()
    plotBiomass(NS_S_def2) + custom_theme()
    plotSpectra(NS_S_def2) + custom_theme()

#### initialN ####
# The initialN used in all 3 def models are the same
  iN_0_1 <- NS_S_def0@params@initial_n - NS_S_def1@params@initial_n
  iN_0_2 <- NS_S_def0@params@initial_n - NS_S_def2@params@initial_n
  compareParams(NS_S_params_his, NS_S_params_def)
  compareParams(NS_S_params_his, NS_params)

#### Fishing just SB or all but SB ####
# F=0 for all
  plotBiomass(NS_S_def0) + theme(legend.position="none")
  plotFMort(NS_S_def0)  + theme(legend.position="none")
  plotSpectra(NS_S_def0)  + theme(legend.position="none")

# F=1 for all
  plotBiomass(NS_S_def1) + theme(legend.position="none")
  plotFMort(NS_S_def1)  + theme(legend.position="none")
  plotSpectra(NS_S_def1)  + theme(legend.position="none")

# F=1 for SB only -> gives plots for Figure 6 
  # SB goes extinct, only has 1g left in the ecosystem afer 20 years
  NS_S_F_y1 <- project(NS_S_params_def, t_max = 20, effort = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1))
  
  plotBiomass(NS_S_F_y1) + custom_theme()
  plotFMort(NS_S_F_y1)  + theme(legend.position="none")
  plotSpectra(NS_S_F_y1)  + theme(legend.position="none")

# F=1 for all but SB
  NS_S_F_n1 <- project(NS_S_params_def, t_max = 20, effort = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0))
  
  plotBiomass(NS_S_F_n1) + theme(legend.position="none")
  plotFMort(NS_S_F_n1)  + theme(legend.position="none")
  plotSpectra(NS_S_F_n1)  + theme(legend.position="none")

# F=0.5 for SB and F=1 for rest
  NS_S_F_05 <- project(NS_S_params_def, t_max = 20, effort = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5))
  
  plotBiomass(NS_S_F_05) + theme(legend.position="none")
  plotFMort(NS_S_F_05)  + theme(legend.position="none")
  plotSpectra(NS_S_F_05)  + theme(legend.position="none")

# F=2 for SB
  NS_S_F_y2 <- project(NS_S_params_def, t_max = 20, effort = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2))
  
  plotBiomass(NS_S_F_y2) + theme(legend.position="none")
  plotFMort(NS_S_F_y2)  + theme(legend.position="none")
  plotSpectra(NS_S_F_y2)  + theme(legend.position="none")

# F=2 for all but SB
  NS_S_F_n2 <- project(NS_S_params_def, t_max = 20, effort = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0))
  
  plotBiomass(NS_S_F_n2) + theme(legend.position="none")
  plotFMort(NS_S_F_n2)  + theme(legend.position="none")
  plotSpectra(NS_S_F_n2)  + theme(legend.position="none")

# F=2 for all
  plotBiomass(NS_S_def2) + theme(legend.position="none")
  plotFMort(NS_S_def2)  + theme(legend.position="none")
  plotSpectra(NS_S_def2)  + theme(legend.position="none")

# F=1 for cod only -> gives plots for Figure 8
  # SB thrives and sees an increase in biomass over the time range, size spectrum of Cod looks normal, and SB size spectrum curve improves
  NS_S_F_c1 <- project(NS_S_params_def, t_max = 20, effort = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0))
  
  plotBiomass(NS_S_F_c1) + theme(legend.position="none")
  plotFMort(NS_S_F_c1)  + theme(legend.position="none")
  plotSpectra(NS_S_F_c1)  + theme(legend.position="none")

#### plotDeath ####
  # Figure 7: plotDeath of Cod, SB, and Saithe at fishing effort = 0. Cod is the leading cause of Cod and SB (due to value duplication) death after 100grams. Saithe is the leading cause of saithe death after 100g.
  plotDeath(object = NS_S_def0, species = "E.Seabass")
  plotDeath(object = NS_S_def0, species = "Saithe")
  plotDeath(object = NS_S_def0, species = "Cod")
  NS_species_params

#### w_mat dip starts at 100g not w_mat -> Supplementary figure 2 ####

spectra_plot <- plotSpectra(NS_S_def0, species = c("Cod", "Saithe", "E.Seabass")) +
  geom_vline(xintercept = 1606.0000 , color = "#142300" , linetype = "dashed" , size = 1) +
  geom_vline(xintercept = 1076.0000 , color = "#a08dfb", linetype = "dashed", size = 1) +
  geom_vline(xintercept = 536.4007 , color = "darkorange1" , linetype = "dashed" , size = 1) +
  geom_vline(xintercept = 100 , color = "red" , linetype = "dashed" , size = 1) +
  labs(title = "Size Spectra with Maturity Sizes")
print(spectra_plot)




