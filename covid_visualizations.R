source("import_preproc_data.R")

# Global aggregate over time ----------------------------------------------
global_tot %>%
    ggplot(aes(x = date, y = total, colour = outcome)) +
    geom_line() + geom_point() +
    theme_minimal() +
    geom_text(data = global_tot %>%
                  filter(date == max(.$date)),
              aes(label = outcome, colour = outcome, x = max(date), y = total),
              size = 8, nudge_y = 1500, nudge_x = -3) +
    theme(legend.position = 'bottom') +
    guides(color = 'none') +
    labs(title = "Coronavirus global outcomes - 2020",
         x = "Date", y = "Cumulative total", color = "Outcomes") +
    scale_colour_manual(values = c("orange2", 'red3', "green4"))



# Generate coronavirus animated plot over time --------------------

world_map <- ggplot() +
    borders("world", colour = "gray50", lwd = .5) +
    theme_map()

world_plot <- world_map +
    geom_point(data = covid_master %>%
                   pivot_longer(cols = c(confirmed, deaths, recoveries),
                                names_to = "outcome", values_to = "total"),
               aes(group = province_state, y = lat, x = long,
                   size = total, colour = outcome),
               alpha = .75) +

    theme_minimal() + guides(size = 'none') +
    scale_size(range = c(0, 15)) +
    scale_color_manual(values = c("orange","darkred","#77AF43")) +
    theme(legend.text = element_text(size = 12),
          legend.title = element_text(size = 14, hjust = 0),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0,
                                    size = 30, face = "bold",
                                    margin=margin(b = 10, unit = "pt"))) +
    transition_states(date, transition_length = 1, state_length = 1) +
    ggtitle('Date {closest_state}') +
    labs(x= '', y='', color = "Outcomes", size = "Cumulative number of cases") +
    theme(axis.text = element_blank(), axis.ticks = element_blank())
animate(world_plot, height = 1000, width =1500)

anim_save("coronavirus_spread.gif", world_plot)
