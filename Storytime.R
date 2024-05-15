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
    plotBiomass(NS_sim_steady)
    plotSpectra(NS_sim_steady)
    plotBiomass(NS_S_his)
    plotSpectra(NS_S_his)

#### default initialN and effort=0 ####
  # project NS_params with a constant fishing effort of 0. Note that we did not set the initial abundance, mizer will   
    NS_params_def <- NS_sim@params
    NS_params_def <- steady(NS_params_def)
    NS_sim_steady0 <- project(NS_params, t_max = 20, effort = 0)

NS_S_params_def <- addSpecies(NS_sim@params, SB_params, SB_gear, SB_initial_effort, inter)
species_params(NS_S_params_def)["E.Seabass", "linecolour"] <- "darkorange1"
NS_S_params_def <- steady(NS_S_params_def)
NS_S_def0 <- project(NS_S_params_def, t_max = 20, effort = 0)

NS_S_def0@params@initial_n

plotBiomass(NS_sim_steady0)
plotSpectra(NS_sim_steady0)
plotBiomass(NS_S_def0)
plotSpectra(NS_S_def0)
animateSpectra(NS_sim_steady0)

#### default initialN and effort=1 ####
NS_params <- steady(NS_sim@params)
NS_sim_steady1 <- project(NS_params, t_max = 20, effort = 1)

NS_S_def1 <- project(NS_S_params_def, t_max = 20, effort = 1)

plotBiomass(NS_sim_steady1)
plotSpectra(NS_sim_steady1)
plotBiomass(NS_S_def1)
plotSpectra(NS_S_def1)

#### default initialN and effort=2 ####
NS_params <- steady(NS_sim@params)
NS_sim_steady2 <- project(NS_params, t_max = 20, effort = 2)

NS_S_def2 <- project(NS_S_params_def, t_max = 20, effort = 2)

plotBiomass(NS_sim_steady2)
plotSpectra(NS_sim_steady2)
plotBiomass(NS_S_def2)
plotSpectra(NS_S_def2)


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


# F=1 for SB only
NS_S_F_y1 <- project(NS_S_params_def, t_max = 20, effort = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1))
plotBiomass(NS_S_F_y1) + theme(legend.position="none")
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


#### plotlyDeath ####
plotDeath(object = NS_S_def0, species = "E.Seabass")
plotDeath(object = NS_S_def1, species = "E.Seabass")
plotDeath(object = NS_S_def2, species = "E.Seabass")

plotDeath(object = NS_S_def0, species = "Saithe")
plotDeath(object = NS_S_def1, species = "Saithe")
plotDeath(object = NS_S_def2, species = "Saithe")

plotDeath(object = NS_S_def0, species = "Cod")
plotDeath(object = NS_S_def1, species = "Cod")
plotDeath(object = NS_S_def2, species = "Cod")
NS_species_params

#### w_mat dip ####

plotEnergyBudget(NS_S_def1, species = c("Cod", "Saithe", "E.Seabass"))
# what is income?

spectra_plot <- plotSpectra(NS_S_def0, species = c("Cod", "Saithe", "E.Seabass")) +
  geom_vline(xintercept = 1606.0000 , color = "#142300" , linetype = "dashed" , size = 1) +
  geom_vline(xintercept = 1076.0000 , color = "#a08dfb", linetype = "dashed", size = 1) +
  geom_vline(xintercept = 536.4007 , color = "darkorange1" , linetype = "dashed" , size = 1) +
  labs(title = "Size Spectra with Maturity Sizes")
print(spectra_plot)

spectra_plot3 <- plotSpectra(NS_S_def0, species = c("Cod","Saithe", "E.Seabass")) +
  geom_vline(xintercept = 1606.0000 , color = "#142300" , linetype = "dashed" , size = 1) +
  geom_vline(xintercept = 1076.0000 , color = "#a08dfb", linetype = "dashed", size = 1) +
  geom_vline(xintercept = 536.4007 , color = "darkorange1" , linetype = "dashed" , size = 1) +
  labs(title = "Size Spectra with Maturity Sizes")
print(spectra_plot3)

# changing interaction
intera <- inter
intera[11,13] <- 0.5

NS_S_params_def <- addSpecies(NS_sim@params, SB_params, SB_gear, SB_initial_effort, intera)
species_params(NS_S_params_def)["E.Seabass", "linecolour"] <- "darkorange1"
NS_S_params_def <- steady(NS_S_params_def)
NS_S_def0 <- project(NS_S_params_def, t_max = 20, effort = 0)
plot(NS_S_def0)

