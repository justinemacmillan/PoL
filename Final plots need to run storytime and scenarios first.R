plotBiomass(NS_sim_steady)+ custom_theme()
plotSpectra(NS_sim_steady)+ custom_theme()
plotBiomass(NS_S_his)+ custom_theme()
plotSpectra(NS_S_his)+ custom_theme()

plotBiomass(NS_sim_steady0)+ custom_theme()
plotSpectra(NS_sim_steady0)+ custom_theme()
plotBiomass(NS_S_def0)+ custom_theme()
plotSpectra(NS_S_def0)+ custom_theme()

plotBiomass(NS_sim_steady1) + custom_theme()
plotSpectra(NS_sim_steady1) + custom_theme()
plotBiomass(NS_S_def1) + custom_theme()
plotSpectra(NS_S_def1) + custom_theme()

plotBiomass(NS_sim_steady2) + custom_theme()
plotSpectra(NS_sim_steady2) + custom_theme()
plotBiomass(NS_S_def2) + custom_theme()
plotSpectra(NS_S_def2) + custom_theme()

NS_S_F_y1 <- project(NS_S_params_def, t_max = 20, effort = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1))
plotBiomass(NS_S_F_y1) + custom_theme()
plotFMort(NS_S_F_y1)  + custom_theme()
plotSpectra(NS_S_F_y1)  + custom_theme()

plotDeath(object = NS_S_def0, species = "E.Seabass") + custom_theme()
plotDeath(object = NS_S_def0, species = "Saithe") + custom_theme()
plotDeath(object = NS_S_def0, species = "Cod") + custom_theme()

NS_S_F_c1 <- project(NS_S_params_def, t_max = 20, effort = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0))

plotBiomass(NS_S_F_c1)  + custom_theme()
plotFMort(NS_S_F_c1)  + custom_theme()
plotSpectra(NS_S_F_c1)  + custom_theme()
