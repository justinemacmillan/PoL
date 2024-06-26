plotBiomassVsSpecies <- function(params) {
  no_sp <- length(params@species_params$species)
  cutoff <- params@species_params$biomass_cutoff
  # When no cutoff known, set it to 0
  if (is.null(cutoff)) cutoff <- rep(0, no_sp)
  cutoff[is.na(cutoff)] <- 0
  observed <- params@species_params$biomass_observed
  if (is.null(observed)) observed <- rep(NA, no_sp)
  
  # selector for foreground species
  foreground <- !is.na(params@A)
  foreground_indices <- (1:no_sp)[foreground]
  biomass_model <- foreground_indices  # create vector of right length
  for (i in seq_along(foreground_indices)) {
    sp <- foreground_indices[i]
    biomass_model[i] <- sum((params@initial_n[sp, ] * params@w * params@dw)
                            [params@w >= cutoff[[sp]]])
  }
  species <- factor(params@species_params$species[foreground],
                    levels = params@species_params$species[foreground])
  df <- rbind(
    data.frame(Species = species,
               Type = "Observation",
               Biomass = observed[foreground],
               other = biomass_model),
    data.frame(Species = species,
               Type = "Model",
               Biomass = biomass_model,
               other = observed[foreground])
  )
  # Get rid of unobserved entries
  df <- df[df$Biomass > 0 & !is.na(df$Biomass), ] 
  
  ggplot(df, aes(x = Species, y = Biomass)) +
    geom_point(aes(shape = Type), size = 4) +
    geom_linerange(aes(ymin = Biomass, ymax = other, colour = Species)) +
    scale_y_continuous(name = "Biomass [g]", trans = "log10",
                       breaks = log_breaks()) + 
    scale_colour_manual(values = getColours(params)) +
    scale_shape_manual(values = c(Model = 1, Observation = 15)) +
    guides(colour = "none") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}