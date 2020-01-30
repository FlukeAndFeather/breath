skip("test manually")

# Plot a dive
plot_dive_event(bm181021_dives, 7, 20)

# Fit ADL over 1 and 4 dives
adl1 <- fit_adl(bm181021_dives)
adl4 <- fit_adl(bm181021_dives, n_dives = 4)
plot_adl(adl1)
plot_adl(adl4)
