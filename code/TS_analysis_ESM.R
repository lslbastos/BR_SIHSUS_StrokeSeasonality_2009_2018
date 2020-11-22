
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(ggfortify)
# library(seastests)
# library(forecast)


# Input data --------------------------------------------------------------
df_stroke_series_month <- read_csv("input/files/aih_stroke_2009_2018_month.csv") %>%
        map_at("regiao", ~factor(., levels = c("N", "NE", "CO", "SE", "S", "BR"),
                                 labels = c("North", "Northeast", "Center-West", "Southeast", "South", "Brazil"), ordered = T)) %>%
        bind_cols()



stl_ggplot <- function(stl_data) {
        
        stl_autoplot <- autoplot(stl_data) 
        stl_autoplot$data$plot_group <- factor(stl_autoplot$data$plot_group, 
                                               levels = c("Data", "trend", "seasonal", "remainder"),
                                                               ordered = TRUE)
        
        stl_plot <- stl_autoplot$data %>% 
                ggplot() +
                geom_line(aes(x = Index, y = value)) +
                facet_wrap(. ~ plot_group, scales = "free_y", ncol = 1) +
                scale_x_date(date_breaks = "6 months",
                             labels = scales::date_format("%b-%Y"),
                             limits = as.Date(c('2008-12-31','2019-01-01')), ) +
                labs(x = "", y = "") +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))

        
        return(stl_plot)
}

# TS data -----------------------------------------------------------------


# List of TSs objects
ls_ts_regions <- df_stroke_series_month %>%
        split(.$regiao) %>% 
        map(~ts(pull(., age_hosp_rate),
                start = c(2009, 01),
                frequency = 12)
            )

# List of STL objects for each TS
ls_ts_stl_obj <- ls_ts_regions %>% 
        map(~stl(., s.window = "periodic")) 

# List of STL plots
ls_stl_plot <- ls_ts_stl_obj %>% 
        imap(~stl_ggplot(.x) +
                    labs(title = .y)) 


# Export STL plts
ls_stl_plot %>% 
        iwalk(~ggsave(paste0("output/figures/Month/plot_stl", .y,"_month_smooth.png"), .x,
                      height = 8, width = 7, dpi = 800, units = "in"))
        
