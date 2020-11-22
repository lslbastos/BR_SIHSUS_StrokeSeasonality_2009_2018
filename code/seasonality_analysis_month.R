# Libraries ---------------------------------------------------------------
library(tidyverse)

# Input data --------------------------------------------------------------

# df_stroke_brazil_month <- read_csv("input/files/aih_stroke_2009_2017_serie_brasil_mes.csv") %>% 
#         # map_at("regiao", ~factor(., levels = c("N", "NE", "CO", "SE", "S"), 
#         #                          labels = c("North", "Northeast", "Mid-West", "Southeast", "South"), ordered = T)) %>% 
#         bind_cols()

# 
# df_stroke_region_month <- read_csv("input/files/aih_stroke_2009_2017_serie_regiao_mes.csv") %>% 
#         map_at("regiao", ~factor(., levels = c("N", "NE", "CO", "SE", "S"),
#                                  labels = c("North", "Northeast", "Center-West", "Southeast", "South"), ordered = T)) %>%
#         bind_cols()

df_stroke_month <- read_csv("input/files/aih_stroke_2009_2018_month.csv") %>% 
        map_at("regiao", ~factor(., levels = c("N", "NE", "CO", "SE", "S", "BR"),
                                 labels = c("North", "Northeast", "Center-West", "Southeast", "South", "Brazil"), ordered = T)) %>%
        bind_cols()


# Brazil -----------------------------------------------------------------
# plot_stroke_cases_median_brazil_month <- df_stroke_brazil_month %>%
#         group_by(mes) %>% 
#         summarise(mediana = median(age_hosp_rate),
#            q1 = quantile(age_hosp_rate, probs = 0.25),
#            q3 = quantile(age_hosp_rate, probs = 0.75)) %>% 
#         ungroup() %>% 
#         ggplot() +
#         geom_point(aes(x = mes, y = mediana)) +
#         geom_errorbar(aes(x = mes, ymin = q1, ymax = q3), width = 0.1) +
#         scale_x_continuous(breaks = 1:12, labels = month.abb) +
#         # scale_y_continuous(limits = c(0, NA)) +
#         labs(x = "Month", y = "Age-adjusted hospitalization rate (per 100,000 pop.)") + 
#         theme_bw() +
#         theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
#               plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
#               panel.grid.major = element_blank(), # get rid of major grid
#               panel.grid.minor = element_blank(), # get rid of minor grid
#               legend.background = element_rect(fill = "transparent"), # get rid of legend bg
#               legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
#               )
# 
# 
# ggsave("output/figures/Month/plot_stroke_cases_median_brazil_month.png", plot_stroke_cases_median_brazil_month, height = 5, width = 6, dpi = 800, units = "in", bg = "transparent")

plot_stroke_cases_median_brazil_month_smooth <- df_stroke_month %>%
        mutate(regiao = "Brazil") %>% 
        # group_by(mes) %>% 
        # summarise(mediana = median(age_hosp_rate),
        #           q1 = quantile(age_hosp_rate, probs = 0.25),
        #           q3 = quantile(age_hosp_rate, probs = 0.75)) %>% 
        # ungroup() %>% 
        ggplot() +
        # geom_point(aes(x = mes, y = mediana)) +
        # geom_errorbar(aes(x = mes, ymin = q1, ymax = q3), width = 0.1) +
        geom_smooth(aes(x = mes, y = age_hosp_rate), color = "black") +
        scale_x_continuous(breaks = 1:12, labels = month.abb) +
        scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
        labs(x = "", y = "Age-adjusted hospitalization rate \n(per 100,000 pop.)") +
        facet_wrap(. ~ regiao, ncol = 1, scales = "free_y") +
        theme_bw() +
        theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
              plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              panel.grid.major = element_blank(), # get rid of major grid
              panel.grid.minor = element_blank(), # get rid of minor grid
              legend.background = element_rect(fill = "transparent"), # get rid of legend bg
              legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
        )


ggsave("output/figures/Month/plot_stroke_cases_median_brazil_month_smooth.png", plot_stroke_cases_median_brazil_month_smooth, height = 3, width = 4, dpi = 800, units = "in", bg = "transparent")


# Regions -----------------------------------------------------------------
# plot_stroke_cases_median_region_month <- df_stroke_region_month %>%
#         group_by(regiao, mes) %>% 
#         summarise(mediana = median(age_hosp_rate),
#            q1 = quantile(age_hosp_rate, probs = 0.25),
#            q3 = quantile(age_hosp_rate, probs = 0.75)) %>% 
#         ungroup() %>% 
#         ggplot() +
#         geom_point(aes(x = mes, y = mediana, color = regiao)) +
#         geom_errorbar(aes(x = mes, ymin = q1, ymax = q3, color = regiao), width = 0.1) +
#         scale_x_continuous(breaks = 1:12, labels = month.abb) +
#         scale_color_manual(name = "Region", values = RColorBrewer::brewer.pal(6, name = "Set2"), guide = F) +
#         labs(x = "Month", y = "Median Age-Adjusted Hospitalization Rate/100,000 population") +
#         facet_wrap(. ~ regiao, ncol = 1, scales = "free_y") +
#         theme_bw() 
# 
# ggsave("output/figures/Month/plot_stroke_cases_median_region_month.png", plot_stroke_cases_median_region_month, height = 12, width = 6, dpi = 800, units = "in")
# 


# Regions (Separated) -----------------------------------------------------
# plot_stroke_cases_median_region_month <- lapply(levels(df_stroke_region_month$regiao), function(region) {
# 
#         region_index <- which(levels(df_stroke_region_month$regiao) == region)
# 
#         plot_stroke_cases_median_allregion_month <- df_stroke_region_month %>%
#                 filter(regiao == region) %>%
#                 group_by(regiao, mes) %>%
#                 summarise(mediana = median(age_hosp_rate),
#                           q1 = quantile(age_hosp_rate, probs = 0.25),
#                           q3 = quantile(age_hosp_rate, probs = 0.75)) %>%
#                 ungroup() %>%
#                 ggplot() +
#                 geom_point(aes(x = mes, y = mediana, color = regiao)) +
#                 geom_errorbar(aes(x = mes, ymin = q1, ymax = q3, color = regiao), width = 0.1) +
#                 scale_x_continuous(breaks = 1:12, labels = month.abb) +
#                 # scale_y_continuous(limits = c(0, NA)) +
#                 scale_color_manual(name = "Region", values = RColorBrewer::brewer.pal(6, name = "Set2")[region_index], guide = F) +
#                 labs(x = "Month", y = "Median Age-Adjusted Hospitalization Rate/100,000 population") +
#                 facet_wrap(. ~ regiao, ncol = 1, scales = "free_y") +
#                 theme_bw() +
#                 theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
#                       plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
#                       panel.grid.major = element_blank(), # get rid of major grid
#                       panel.grid.minor = element_blank(), # get rid of minor grid
#                       legend.background = element_rect(fill = "transparent"), # get rid of legend bg
#                       legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
#                       )
# 
# 
#                 ggsave(paste0("output/figures/Month/plot_stroke_cases_median_", region,"_month.png"), plot_stroke_cases_median_allregion_month, height = 5, width = 7, dpi = 800, units = "in", bg = "transparent")
#                 return(plot_stroke_cases_median_allregion_month)
#         }
# )

color_pallete <- c("#9DD3AF","#FDD1A2", "#A88F71", "#A7C4D2", "#7190C9", "black")
# color_pallete <- RColorBrewer::brewer.pal(6, name = "Set2")

plot_stroke_cases_median_region_month_smooth <- lapply(levels(df_stroke_month$regiao), function(region) {
        
        region_index <- which(levels(df_stroke_month$regiao) == region)
        
        plot_stroke_cases_median_allregion_month <- df_stroke_month %>%
                filter(regiao == region) %>%
                # group_by(regiao, mes) %>%
                # summarise(mediana = median(age_hosp_rate),
                #           q1 = quantile(age_hosp_rate, probs = 0.25),
                #           q3 = quantile(age_hosp_rate, probs = 0.75)) %>%
                ungroup() %>%
                ggplot() +
                # geom_point(aes(x = mes, y = mediana, color = regiao)) +
                # geom_errorbar(aes(x = mes, ymin = q1, ymax = q3, color = regiao), width = 0.1) +
                geom_smooth(aes(x = mes, y = age_hosp_rate, color = regiao)) +
                scale_x_continuous(breaks = 1:12, labels = month.abb) +
                scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
                scale_color_manual(name = "Region", values = color_pallete[region_index], guide = F) +
                # scale_fill_manual(name = "Region", values = color_pallete[region_index], guide = F) +
                labs(x = "", y = "Age-adjusted hospitalization rate \n(per 100,000 pop.)") +
                facet_wrap(. ~ regiao, ncol = 1, scales = "free_y") +
                theme_bw() +
                theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
                      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                      panel.grid.major = element_blank(), # get rid of major grid
                      panel.grid.minor = element_blank(), # get rid of minor grid
                      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
                      legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
                )
        
        
        ggsave(paste0("output/figures/Month/plot_stroke_cases_median_", region,"_month_smooth.png"), plot_stroke_cases_median_allregion_month, height = 3, width = 4, dpi = 800, units = "in", bg = "transparent")
        return(plot_stroke_cases_median_allregion_month)
}
)



# plot_stroke_cases_median_allregion_month <- df_stroke_region_month %>%
#         # filter(regiao == region) %>%
#         # group_by(regiao, mes) %>%
#         # summarise(mediana = median(age_hosp_rate),
#         #           q1 = quantile(age_hosp_rate, probs = 0.25),
#         #           q3 = quantile(age_hosp_rate, probs = 0.75)) %>%
#         ungroup() %>%
#         ggplot() +
#         # geom_point(aes(x = mes, y = mediana, color = regiao)) +
#         # geom_errorbar(aes(x = mes, ymin = q1, ymax = q3, color = regiao), width = 0.1) +
#         geom_smooth(aes(x = mes, y = age_hosp_rate, color = regiao, fill = regiao)) +
#         scale_x_continuous(breaks = 1:12, labels = month.abb) +
#         scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
#         scale_color_manual(name = "Region", values = color_pallete) +
#         scale_fill_manual(name = "Region", values = color_pallete) +
#         labs(x = "", y = "Age-adjusted hospitalization rate \n(per 100,000 pop.)") +
#         # facet_wrap(. ~ regiao, ncol = 1, scales = "free_y") +
#         theme_bw() +
#         theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
#               plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
#               panel.grid.major = element_blank(), # get rid of major grid
#               panel.grid.minor = element_blank(), # get rid of minor grid
#               legend.background = element_rect(fill = "transparent"), # get rid of legend bg
#               legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
#         )




