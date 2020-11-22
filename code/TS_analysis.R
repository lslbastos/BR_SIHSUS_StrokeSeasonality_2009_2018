
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(seastests)
library(forecast)


# Input data --------------------------------------------------------------
df_stroke_series_month <- read_csv("input/files/aih_stroke_2009_2018_month.csv") %>%
        map_at("regiao", ~factor(., levels = c("N", "NE", "CO", "SE", "S", "BR"),
                                 labels = c("North", "Northeast", "Center-West", "Southeast", "South", "Brazil"), ordered = T)) %>%
        bind_cols()


# Analysis ----------------------------------------------------------------
## BRAZIL
# ts_series_br <- ts(df_stroke_series_month %>%
#                            arrange(ano, mes) %>%
#                            pull(age_hosp_rate),
#                    start = 2009,
#                    frequency = 12)

# ts_series_br_stl_plot <- ts_series_br_stl %>% 
#         autoplot() +
#         theme_bw()

# ts_series_br_ts_features <- ts_series_br %>% bind_cols(tsfeatures::tsfeatures(ts_series_br,
#                                                    features = "stl_features",
#                                                    s.window = "periodic",
#                                                    scale = FALSE,
#                                                    robust = TRUE) %>%
#         mutate(peak = month.name[peak]) %>%
#         bind_cols(median_iqr = paste0(
#                 round(quantile(ts_series_br, probs = 0.5), 2),
#                 " (", round(quantile(ts_series_br, probs = 0.25), 2), ", ", round(quantile(ts_series_br, probs = 0.75), 2), ")"
#                 )
#         ) %>% 
#         select(peak, trend, seasonal_strength)

        
        
        


# Regions -----------------------------------------------------------------
# df_stroke_series_regions <- read_csv("input/files/aih_stroke_2009_2017_serie_estado_dia.csv") %>% 
#         map_at("regiao", ~factor(., levels = c("N", "NE", "CO", "SE", "S"),
#                                  labels = c("North", "Northeast", "Mid-West", "Southeast", "South"), ordered = T)) %>%
#         bind_cols()




df_desc_stroke_series <- df_stroke_series_month %>% 
        group_by(regiao) %>%
        summarise(q25 = quantile(age_hosp_rate, probs = 0.25),
                  q50 = quantile(age_hosp_rate, probs = 0.5),
                  q75 = quantile(age_hosp_rate, probs = 0.75)) %>%
        mutate(temp_med_p50_iqr = paste0(round(q50, 1), " [", round(q25, 1), ", ", round(q75, 1), "]")) %>% 
        select(regiao, temp_med_p50_iqr)

df_strength_all_series <- df_stroke_series_month %>% 
        split(.$regiao) %>% 
        map(~ts(pull(., age_hosp_rate),
                start = c(2009, 01), end = c(2018, 12),
                frequency = 12)
            ) %>% 
        imap(~bind_cols(regiao = .y, select(tsfeatures::tsfeatures(., features = "stl_features", s.window = "periodic", scale = FALSE),
                        trend, seasonal_strength, peak, trough))) %>% 
        bind_rows()
        # map(~stl(., s.window = "periodic")) %>% 
        # map(~ts_summary(.)) %>% 
        # imap(~tibble(regiao = .y, as_tibble(.x))) %>% 
        # bind_rows()
        
# tsfeatures::tsfeatures(ts_series, features = "stl_features", s.window = "periodic", scale = FALSE) 

# ts_periodogram <- spec.pgram(ts_series, plot = FALSE, taper = 0, fast = FALSE)
# ts_periodogram$coh

        
# series_list <- df_stroke_series_month %>%
#         split(.$regiao) %>%
#         map(~ts(pull(., age_hosp_rate),
#                 start = c(2009, 01), end = c(2018, 12),
#                 frequency = 12)
#             ) 


df_stroke_ts_seastest <- series_list %>% 
        imap(~bind_cols(regiao = .y, 
                        kw_estimate = seastests::kw(x = ., diff = F, residuals = F, autoarima = F, freq = NA)$stat,
                        kw_pval = seastests::kw(x = ., diff = F, residuals = F, autoarima = F, freq = NA)$Pval)) %>% 
        bind_rows()


df_stroke_ts_stats <- inner_join(df_desc_stroke_series, 
                                 df_strength_all_series,
                                 by = c("regiao" = "regiao")) %>% 
        inner_join(df_stroke_ts_seastest, c("regiao" = "regiao"))




write_csv(df_stroke_ts_stats, "output/tables/df_stroke_ts_stats.csv")



### Stroke Temp Association

df_temp_month <- read_csv("input/files/df_temp_month_2009_2018.csv") %>% 
        map_at("regiao", ~factor(., levels = c("North", "Northeast", "Mid-West", "Southeast", "South", "Brazil"),
                                 labels = c("North", "Northeast", "Center-West", "Southeast", "South", "Brazil"), ordered = T)) %>%
        bind_cols()



df_stroke_temp <- df_stroke_month %>% 
        select(regiao, ano, mes, age_hosp_rate) %>% 
        inner_join(df_temp_month %>% 
                                   select(regiao, ano, mes, p50_temp_ar),
                   by = c("regiao" = "regiao",
                          "ano" = "ano",
                          "mes" = "mes"))

df_stroke_temp %>%
        split(.$regiao) %>% 
        map(~cor.test(.$age_hosp_rate, .$p50_temp_ar, method = "spearman"))


# # State -----------------------------------------------------------------
# df_stroke_series_regions <- read_csv("input/files/aih_stroke_2009_2017_serie_estado_mes.csv") %>% 
#         bind_cols()
# 
# 
# strength_all_series <- df_stroke_series_regions %>% 
#         split(.$estado) %>% 
#         map(~ts(pull(., age_hosp_rate),
#                 start = c(2009, 01), end = c(2017, 12),
#                 frequency = 12)
#             ) %>% 
#         imap(~bind_cols(regiao = .y, select(tsfeatures::tsfeatures(., features = "stl_features", s.window = "periodic", scale = FALSE),
#                         trend, seasonal_strength, peak, trough))) %>% 
#         bind_rows()
# 
#         # map(~ts_summary(.)) %>% 
#         # imap(~tibble(regiao = .y, as_tibble(.x))) %>% 
#         # bind_rows()
# 
# 
# 
# strength_all_series %>% 
#         inner_join(df_state_lats, by = c("regiao" = "estado")) %>%
#         gather(indicador, valor, trend:trough) %>% 
#         filter(!(indicador %in% c("trend", "seasonal_strength"))) %>% 
#         ggplot() +
#         geom_point(aes(x = valor, y = lat)) +
#         facet_wrap(.~ indicador) +
#         labs() +
#         theme_bw()
# 
# 
# 
# frequency_all_series <- df_stroke_series_regions %>% 
#         split(.$estado) %>% 
#         map(~ts(pull(., age_hosp_rate_day_p50),
#                 start = c(2009, 01), end = c(2017, 12),
#                 frequency = 12)
#             ) %>% 
#         map(~findfrequency(.))
# 
# 
# findfrequency(frequency_all_series[[10]])
