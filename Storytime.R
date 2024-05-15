library(mizer)
library(mizerExperimental)
library(readxl)
library(dplyr)
library(ggplot2)


#### load parameters ####
SB_params <- read_excel("Cod=Seabass/SB_params.xlsx")
SB_gear <- read_excel("Cod=Seabass/SB_gear.xlsx")
SB_initial_effort <- read_excel("Cod=Seabass/SB_initial_effort.xlsx")
SB_initial_effort <- apply(SB_initial_effort, 2, as.numeric)
inter <- read_excel("Cod=Seabass/inter.xlsx")
row_names <- inter[[1]]
inter <- inter[, -1]
inter <- apply(inter, 2, as.numeric)
inter <- as.matrix(inter)
rownames(inter) <- row_names
effort_NS_S <- matrix(nrow = 44, ncol = 13)
effort_NS_S[, 1:12] <- NS_sim@effort
effort_NS_S[, 13] <- effort_NS_S[, 11]
colnames(effort_NS_S) <- row_names
rownames(effort_NS_S) <- rownames(NS_sim@effort)
NS_initialN <- NS_sim@params@initial_n
SB_initialN <- read_excel("Cod=Seabass/initialN.xlsx")
SB_initialN <- select(SB_initialN, -"...1")
rownames(SB_initialN) <- "E.Seabass"
colnames(SB_initialN) <- colnames(NS_initialN)
NS_S_initialN <- rbind(NS_initialN, SB_initialN)
NS_S_initialN <- as.matrix(NS_S_initialN)

#### historical F effort ####
NS_S_params_his <- addSpecies(NS_sim@params, SB_params, SB_gear, SB_initial_effort, inter)
initialN(NS_S_params_his) <- NS_S_initialN
species_params(NS_S_params_his)["E.Seabass", "linecolour"] <- "darkorange1"
NS_S_params_his <- steady(NS_S_params_his)
NS_S_his <- project(NS_S_params_his, t_max = 20, effort = effort_NS_S)

# Seabass is acting like a normal species!
# plotGrowthCurves(NS_S_his, species = "E.Seabass")
# plotSpectra(NS_S_his, species = "E.Seabass")

NS_params <- steady(NS_sim@params)
NS_sim_steady <- project(NS_params, t_max = 20, effort = NS_sim@effort)


plotBiomass(NS_sim_steady)
plotSpectra(NS_sim_steady)
plotBiomass(NS_S_his)
plotSpectra(NS_S_his)

#### default initialN and effort=0 ####
NS_params <- steady(NS_sim@params)
NS_sim_steady0 <- project(NS_params, t_max = 20, effort = 0)

NS_S_params_def <- addSpecies(NS_sim@params, SB_params, SB_gear, SB_initial_effort, inter)
species_params(NS_S_params_def)["E.Seabass", "linecolour"] <- "darkorange1"
NS_S_params_def <- steady(NS_S_params_def)
NS_S_def0 <- project(NS_S_params_def, t_max = 20, effort = 0)

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

