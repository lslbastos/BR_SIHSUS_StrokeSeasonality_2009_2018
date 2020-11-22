# Libraries ---------------------------------------------------------------
library(tidyverse)

# Input data --------------------------------------------------------------

## Dados - Temperatura
df_temp_series_month <- vroom::vroom("input/files/df_dados_temp_regiao_2009_2018_tzBT.csv") %>%
        bind_rows(read_csv("input/files/df_dados_temp_brazil_2009_2018_tzBT.csv") %>% 
                          select(-estado)) %>% 
        map_at("regiao", ~factor(., levels = c("N", "NE", "CO", "SE", "S", "BR"),
                                 labels = c("North", "Northeast", "Mid-West", "Southeast", "South", "Brazil"), ordered = T)) %>%
        bind_cols()


# df_temp_series_month_br <- read_csv("input/files/df_dados_temp_brazil_2009_2018_tzBT.csv") %>% 
#         select(-estado) %>%
#         map_at("regiao", ~factor(., levels = c("N", "NE", "CO", "SE", "S"),
#                                  labels = c("North", "Northeast", "Mid-West", "Southeast", "South"), ordered = T)) %>%
#         bind_cols()




## Descriptive analysis

## Regiao - Month quantiles - Median Air Temperature
df_desc_temp_month <- df_temp_series_month %>%
        select(regiao, p50_temp_ar) %>% 
        split(.$regiao) %>% 
        map(~quantile(.$p50_temp_ar, probs = c(0.25, 0.50, 0.75))) %>% 
        bind_rows() %>% 
        mutate(q = c("q25", "q50", "q75")) %>% 
        pivot_longer(cols = c(North:Brazil), names_to = "regiao", values_to = "temp") %>% 
        pivot_wider(names_from = "q", values_from = "temp") %>% 
        mutate(temp_med_p50_iqr = paste0(round(q50, 1), " [", round(q25, 1), ", ", round(q75, 1), "]")) %>% 
        select(regiao, temp_med_p50_iqr)



df_strength_temp_month <- df_temp_series_month %>% 
        select(regiao, p50_temp_ar) %>% 
        split(.$regiao) %>%
        map(~ts(pull(., p50_temp_ar),
                start = c(2009, 01), end = c(2018, 12),
                frequency = 12)
            ) %>%
        imap(~bind_cols(regiao = .y, select(tsfeatures::tsfeatures(., features = "stl_features", s.window = "periodic", scale = FALSE),
                                            trend, seasonal_strength, peak, trough))) %>% 
        bind_rows()
        


df_stats_month_regiao <- inner_join(df_desc_temp_month, 
                                    df_strength_temp_month,
                                    by = c("regiao" = "regiao"))
        
        
write_csv(df_stats_month_regiao, "output/tables/df_stats_month_regiao.csv")


write_csv(df_temp_series_month, "input/files/df_temp_month_2009_2018.csv")

# ts_temp_br <- ts(df_temp_br$p50_temp_ar,
#            start = c(2009, 01), end = c(2018, 12),
#            frequency = 12)
# 
# 
# quantile(ts_temp_br)
# 
# select(tsfeatures::tsfeatures(ts_temp_br, features = "stl_features", s.window = "periodic", scale = FALSE),
#        trend, seasonal_strength, peak, trough)



# plot_temp_smooth_med_temp_ar <- df_temp_series_avg %>% 
#         filter(regiao != "Brazil") %>% 
#         ggplot() +
#         geom_line(aes(x = mes, y = p50_temp_ar, color = regiao)) +
#         scale_x_continuous(breaks = 1:12, labels = month.abb) +
#         # scale_y_continuous(limits = c(5, NA)) +
#         scale_color_manual(name = "Region", values = RColorBrewer::brewer.pal(6, name = "Set2"), guide = F) +
#         # scale_fill_manual(name = "Region", values = RColorBrewer::brewer.pal(6, name = "Set2"), guide = F) +
#         labs(x = "Month", y = "Median Air Temperature (ºC)") + 
#         facet_wrap(. ~ regiao, ncol =  1) +
#         theme_bw()
# 
# 
# plot_temp_smooth_med_temp_ar_brazil <- df_temp_series_avg %>% 
#         filter(regiao == "Brazil") %>% 
#         ggplot() +
#         geom_line(aes(x = mes, y = med_temp_ar, color = regiao)) +
#         geom_errorbar(aes(ymin = q1_temp_ar, ymax = q3_temp_ar, x = mes, color = regiao), width = 0.1) +
#         scale_x_continuous(breaks = 1:12, labels = month.abb) +
#         # scale_y_continuous(limits = c(0, NA)) +
#         scale_color_manual(name = "Region", values = RColorBrewer::brewer.pal(6, name = "Set2")[6], guide = F) +
#         # scale_fill_manual(name = "Region", values = RColorBrewer::brewer.pal(6, name = "Set2"), guide = F) +
#         labs(x = "Month", y = "Median Air Temperature (ºC)") +
#         facet_wrap(. ~ regiao, ncol =  1) +
#         theme_bw()



## Plots for monthly boxplot 


color_pallete <- c("#9DD3AF","#FDD1A2", "#A88F71", "#A7C4D2", "#7190C9", "black")

plot_temp_median_region_month <- lapply(levels(df_temp_series_month$regiao), function(region) {
        
        region_index <- which(levels(df_temp_series_month$regiao) == region)
                
        plot_temp_cases_median_allregion_month <- df_temp_series_month %>% 
                filter(regiao == region) %>%
                # group_by(regiao, mes) %>%
                # summarise(mediana = median(age_hosp_rate),
                #           q1 = quantile(age_hosp_rate, probs = 0.25),
                #           q3 = quantile(age_hosp_rate, probs = 0.75)) %>%
                ungroup() %>%
                ggplot() +
                # geom_point(aes(x = mes, y = mediana, color = regiao)) +
                # geom_errorbar(aes(x = mes, ymin = q1, ymax = q3, color = regiao), width = 0.1) +
                geom_smooth(aes(x = mes, y = p50_temp_ar, color = regiao)) +
                scale_x_continuous(breaks = 1:12, labels = month.abb) +
                scale_y_continuous(labels = function(x) sprintf("%.1f", x), limits = c(0, 30)) +
                scale_color_manual(name = "Region", values = color_pallete[region_index], guide = F) +
                # scale_fill_manual(name = "Region", values = color_pallete[region_index], guide = F) +
                labs(x = "", y = "Median Air Temperature (Celsius)") +
                facet_wrap(. ~ regiao, ncol = 1) +
                theme_bw() +
                theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
                      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                      panel.grid.major = element_blank(), # get rid of major grid
                      panel.grid.minor = element_blank(), # get rid of minor grid
                      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
                      legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
                )
        
        
        
        ggsave(paste0("output/figures/Month/plot_temp_median_", region,"_month_smooth.png"), plot_temp_cases_median_allregion_month, height = 3, width = 4, dpi = 800, units = "in", bg = "transparent")
        return(plot_temp_cases_median_allregion_month)

        }
)



plot_temp_cases_median_allregion_month <- df_temp_series_month %>% 
                # filter(regiao == region) %>%
                # group_by(regiao, mes) %>%
                # summarise(mediana = median(age_hosp_rate),
                #           q1 = quantile(age_hosp_rate, probs = 0.25),
                #           q3 = quantile(age_hosp_rate, probs = 0.75)) %>%
                ungroup() %>%
                ggplot() +
                # geom_point(aes(x = mes, y = mediana, color = regiao)) +
                # geom_errorbar(aes(x = mes, ymin = q1, ymax = q3, color = regiao), width = 0.1) +
                geom_smooth(aes(x = mes, y = p50_temp_ar, color = regiao)) +
                scale_x_continuous(breaks = 1:12, labels = month.abb) +
                scale_y_continuous(labels = function(x) sprintf("%.1f", x), limits = c(10, 30)) +
                scale_color_manual(name = "Region", values = color_pallete, guide = F) +
                # scale_fill_manual(name = "Region", values = color_pallete[region_index], guide = F) +
                labs(x = "", y = "Median Air Temperature (Celsius)") +
                facet_wrap(. ~ regiao, ncol = 2) +
                theme_bw() +
                theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
                      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                      panel.grid.major = element_blank(), # get rid of major grid
                      panel.grid.minor = element_blank(), # get rid of minor grid
                      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
                      legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
                )

ggsave(paste0("output/figures/Month/plot_temp_median_all.png"), plot_temp_cases_median_allregion_month, 
       height = 9, width = 8, dpi = 800, units = "in")
