# Libraries ---------------------------------------------------------------
library(tidyverse)


# Input data --------------------------------------------------------------

df_stroke_dados <- vroom::vroom("input/files/aih_stroke_2020jan.csv") %>%
        mutate(elder_group = case_when(
                idade_grupo_who %in% c("60-64", "65-69", "70-74", "75-79", "80-84", "85+") ~ "elder",
                TRUE ~ "n_elder"
                )
        ) %>% 
        filter(!(idade_grupo_who %in% c("0-4", "5-9", "10-14", "15-19")))
        


df_pop_ibge <- read_csv2("input/source_files/pop_ibge_estado.csv") %>% 
        gather(ano, populacao, `2009`:`2018`) %>% 
        map_at("ano", as.numeric) %>% 
        bind_cols() %>% 
        mutate(idade = case_when(
                idade %in% c("85-89", "90") ~ "85+",
                TRUE ~ as.character(idade)
                )
               ) %>% 
        group_by(regiao, estado, idade, ano) %>%
        summarise(populacao_total = sum(populacao)) %>% 
        ungroup() %>% 
        mutate(elder_group = case_when(
                idade %in% c("60-64", "65-69", "70-74", "75-79", "80-84", "85+") ~ "elder",
                TRUE ~ "n_elder"
                )
               )

df_pop_who_rates <- read_csv("input/source_files/who_pop_std_rates.csv") %>% 
        mutate(elder_group = case_when(
                age_group %in% c("60-64", "65-69", "70-74", "75-79", "80-84", "85+") ~ "elder",
                TRUE ~ "n_elder"
                )
        )

# Data Preparation --------------------------------------------------------

## Data Brazil
df_stroke_series_brazil <- df_stroke_dados %>%
        count(idade_grupo_who, elder_group, ano, mes, dia) %>%
        mutate(regiao = "BR", estado = "BR") %>%
        select(regiao, estado, idade_grupo_who, elder_group, ano, mes, dia, n)

## Data Region / State
df_stroke_serie_regiao <- df_stroke_dados %>% 
        count(regiao, estado, idade_grupo_who, elder_group, ano, mes, dia) %>% 
        bind_rows(df_stroke_series_brazil)


       
# Age-adjusted data Region -------------------------------------------------

df_stroke_serie_regiao_age_month <- df_stroke_serie_regiao %>% 
        group_by(regiao, estado, idade_grupo_who, elder_group, ano, mes) %>% 
        summarise(casos_total = sum(n)) %>% 
        ungroup() %>% 
        inner_join(df_pop_ibge, by = c("estado" = "estado",
                                       "regiao" = "regiao",
                                       "idade_grupo_who" = "idade",
                                       "ano" = "ano",
                                       "elder_group" = "elder_group")) %>%
        inner_join(df_pop_who_rates, by = c("idade_grupo_who" = "age_group",
                                            "elder_group" = "elder_group")) %>% 
        group_by(regiao, ano, mes) %>% 
        summarise(admissoes = sum(casos_total),
                  pop_total = sum(populacao_total),
                  age_hosp_rate = 100000*epitools::ageadjust.direct(count = casos_total, pop = populacao_total, stdpop = pop_rate / 100)[2]
                  ) %>%
        group_by(regiao) %>% 
        arrange(regiao, ano, mes) %>%
        mutate(periodo = 1:n()) %>% 
        ungroup()


# Age-adjusted data Brazil -------------------------------------------------

df_stroke_serie_brazil_age_month <- df_stroke_series_brazil %>% 
        group_by(regiao, estado, idade_grupo_who, elder_group, ano, mes) %>% 
        summarise(casos_total = sum(n)) %>% 
        ungroup() %>% 
        inner_join(df_pop_ibge %>% 
                           group_by(idade, ano, elder_group) %>% 
                           summarise(populacao_total = sum(populacao_total)),
                   by = c("idade_grupo_who" = "idade",
                          "ano" = "ano",
                          "elder_group" = "elder_group")
                   ) %>%
        inner_join(df_pop_who_rates, by = c("idade_grupo_who" = "age_group",
                                            "elder_group" = "elder_group")) %>% 
        group_by(regiao, ano, mes) %>% 
        summarise(admissoes = sum(casos_total),
                  pop_total = sum(populacao_total),
                  age_hosp_rate = 100000*epitools::ageadjust.direct(count = casos_total, pop = populacao_total, stdpop = pop_rate / 100)[2]
                  ) %>%
        ungroup() %>% 
        mutate(periodo = 1:n())   



df_stroke_serie_age_month <- bind_rows(df_stroke_serie_regiao_age_month, 
                                        df_stroke_serie_brazil_age_month)

# Export ------------------------------------------------------------------
write_csv(df_stroke_serie_age_month, "input/files/aih_stroke_2009_2018_month.csv")
