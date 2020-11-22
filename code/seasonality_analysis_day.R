# Libraries ---------------------------------------------------------------
library(tidyverse)

# Input data --------------------------------------------------------------
source("data_preparation.R")

df_stroke_brazil_day <- df_stroke_series_brazil %>% 
        inner_join(df_pop_ibge %>% 
                           group_by(idade, ano, elder_group) %>% 
                           summarise(populacao_total = sum(populacao_total)),
                   by = c("idade_grupo_who" = "idade",
                          "ano" = "ano",
                          "elder_group" = "elder_group")
                   ) %>%
        inner_join(df_pop_who_rates, by = c("idade_grupo_who" = "age_group",
                                            "elder_group" = "elder_group")) %>% 
        group_by(ano, mes, dia) %>% 
        summarise(admissoes = sum(n),
                  pop_total = sum(populacao_total),
                  age_hosp_rate = 100000*epitools::ageadjust.direct(count = n, pop = populacao_total, stdpop = pop_rate / 100)[2]
                  ) %>%
        ungroup() %>% 
        # map_at("regiao", ~factor(., levels = c("N", "NE", "CO", "SE", "S"), 
        #                          labels = c("North", "Northeast", "Mid-West", "Southeast", "South"), ordered = T)) %>% 
        bind_cols()


df_stroke_region_day <- df_stroke_serie_regiao %>% 
        inner_join(df_pop_ibge, by = c("estado" = "estado",
                                       "regiao" = "regiao",
                                       "idade_grupo_who" = "idade",
                                       "ano" = "ano",
                                       "elder_group" = "elder_group")) %>%
        inner_join(df_pop_who_rates, by = c("idade_grupo_who" = "age_group",
                                            "elder_group" = "elder_group")) %>% 
        group_by(regiao, ano, mes, dia) %>% 
        summarise(admissoes = sum(n),
                  pop_total = sum(populacao_total),
                  age_hosp_rate = 100000*epitools::ageadjust.direct(count = n, pop = populacao_total, stdpop = pop_rate / 100)[2]
                  ) %>%
        ungroup() %>% 
        map_at("regiao", ~factor(., levels = c("N", "NE", "CO", "SE", "S"),
                                 labels = c("North", "Northeast", "Mid-West", "Southeast", "South"), ordered = T)) %>%
        bind_cols()


# Brazil -----------------------------------------------------------------
plot_stroke_cases_median_brazil_day <- df_stroke_brazil_day %>%
        group_by(mes) %>% 
        summarise(mediana = median(age_hosp_rate),
           q1 = quantile(age_hosp_rate, probs = 0.25),
           q3 = quantile(age_hosp_rate, probs = 0.75)) %>% 
        ungroup() %>% 
        ggplot() +
        geom_point(aes(x = mes, y = mediana)) +
        geom_errorbar(aes(x = mes, ymin = q1, ymax = q3), width = 0.1) +
        scale_x_continuous(breaks = 1:12, labels = month.abb) +
        # scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Month", y = "Median Age-Adjusted Hospitalization Rate/100,000 population (day)") + 
        theme_bw() +
                theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
              plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              panel.grid.major = element_blank(), # get rid of major grid
              panel.grid.minor = element_blank(), # get rid of minor grid
              legend.background = element_rect(fill = "transparent"), # get rid of legend bg
              legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
              )


ggsave("output/figures/Day/plot_stroke_cases_median_brazil_day.png", plot_stroke_cases_median_brazil_day, height = 5, width = 6, dpi = 800, units = "in", bg = "transparent")



# Regions -----------------------------------------------------------------
plot_stroke_cases_median_region_day <- df_stroke_region_day %>%
        group_by(regiao, mes) %>% 
        summarise(mediana = median(age_hosp_rate),
           q1 = quantile(age_hosp_rate, probs = 0.25),
           q3 = quantile(age_hosp_rate, probs = 0.75)) %>% 
        ungroup() %>% 
        ggplot() +
        geom_point(aes(x = mes, y = mediana, color = regiao)) +
        geom_errorbar(aes(x = mes, ymin = q1, ymax = q3, color = regiao), width = 0.1) +
        scale_x_continuous(breaks = 1:12, labels = month.abb) +
        # scale_y_continuous(limits = c(0, NA)) +
        scale_color_manual(name = "Region", values = RColorBrewer::brewer.pal(6, name = "Set2"), guide = F) +
        labs(x = "Month", y = "Median Age-Adjusted Hospitalization Rate/100,000 population (day)") +
        facet_wrap(. ~ regiao, ncol = 1, scales = "free") +
        theme_bw() 

ggsave("output/figures/Day/plot_stroke_cases_median_region_day.png", plot_stroke_cases_median_region_day, height = 12, width = 6, dpi = 800, units = "in", bg = "transparent")



# Regions (Separated) -----------------------------------------------------
plot_stroke_cases_median_region_day <- lapply(levels(df_stroke_region_day$regiao), function(region) {
        
        region_index <- which(levels(df_stroke_region_day$regiao) == region)
                
        plot_stroke_cases_median_allregion_day <- df_stroke_region_day %>%
                filter(regiao == region) %>%
                group_by(regiao, mes) %>%
                summarise(mediana = median(age_hosp_rate),
                          q1 = quantile(age_hosp_rate, probs = 0.25),
                          q3 = quantile(age_hosp_rate, probs = 0.75)) %>% 
                ungroup() %>% 
                ggplot() +
                geom_point(aes(x = mes, y = mediana, color = regiao)) +
                geom_errorbar(aes(x = mes, ymin = q1, ymax = q3, color = regiao), width = 0.1) +
                scale_x_continuous(breaks = 1:12, labels = month.abb) +
                # scale_y_continuous(limits = c(0, NA)) +
                scale_color_manual(name = "Region", values = RColorBrewer::brewer.pal(6, name = "Set2")[region_index], guide = F) +
                labs(x = "Month", y = "Median Age-Adjusted Hospitalization Rate/100,000 population (day)") +
                facet_wrap(. ~ regiao, ncol = 1, scales = "free_y") +
                theme_bw() +
                        theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
              plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              panel.grid.major = element_blank(), # get rid of major grid
              panel.grid.minor = element_blank(), # get rid of minor grid
              legend.background = element_rect(fill = "transparent"), # get rid of legend bg
              legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
              )



                ggsave(paste0("output/figures/Day/plot_stroke_cases_median_", region,"_day.png"), plot_stroke_cases_median_allregion_day, height = 5, width = 7, dpi = 800, units = "in", bg = "transparent")
                return(plot_stroke_cases_median_allregion_day)
        }
        
        

)

