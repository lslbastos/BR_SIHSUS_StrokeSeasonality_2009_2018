# Libraries ---------------------------------------------------------------
library(tidyverse)
# library(infer)

# Input data --------------------------------------------------------------

df_stroke_dados <- read_csv("input/aih_stroke_2009_2017_dados2.csv") %>% 
        select(morte, regiao, mes, sexo, idade_real_anos) %>%
        bind_rows(read_csv("input/aih_stroke_2009_2017_dados2.csv") %>%
                          select(morte, regiao, mes, sexo, idade_real_anos) %>%
                          mutate(regiao = "BR")) %>% 
        mutate(sexo = ifelse(sexo == 1, "M", "F")) %>% 
        map_at(c("morte", "regiao", "mes", "sexo"), factor) %>% 
        bind_cols() %>% 
        filter(idade_real_anos >= 20)



# Multivariate Logistic  Regression ---------------------------------------

# model_multivar_log_reg <- glm(morte ~ idade_real_anos + relevel(sexo, ref = "M") + relevel(mes, ref = "1") + relevel(regiao, ref = "N"),
#                               data = df_stroke_dados, 
#                               family = "binomial")


model_variables <- c("Intercept", "Age", "Female", 
                     "February", "March", "April", "May", "June", 
                     "July", "August", "September", "October", "November", 
                     "December", "January (Ref.)", "Male (Ref.)")


model_variables_order <- c("Intercept", "Age", "Male (Ref.)", "Female", "January (Ref.)",
                     "February", "March", "April", "May", "June", 
                     "July", "August", "September", "October", "November", "December")


ls_model_multivar_log_reg <- df_stroke_dados %>%
        split(.$regiao) %>% 
        map(~glm(morte ~ idade_real_anos + relevel(sexo, ref = "M") + relevel(mes, ref = "1"),
                              data = ., 
                              family = "binomial")) %>% 
        map(summary) %>% 
        map(coefficients) %>% 
        map(~tibble(variable = row.names(.),
                           or = exp(.[,1]),
                           or_upper = exp(.[,1] - 1.96*.[,2]),
                           or_lower = exp(.[,1] + 1.96*.[,2])
                           )
            ) %>% 
        map(~bind_rows(., tibble(variable = c("January (Ref.)", "Male (Ref.)"),
                        or = NA, 
                        or_upper = NA,
                        or_lower = NA
                        )
                       )
            ) %>% 
        imap_dfr(~mutate(., regiao = .y)) %>%
        map_at("regiao", ~factor(., levels = c("N", "NE", "CO", "SE", "S", "BR"), labels = c("North", "Northeast", "Mid-West", "Southeast", "South", "Brazil"), ordered = T)) %>%
        map_at("variable", ~factor(., levels = rev(unique(.)), labels = rev(model_variables), ordered = T)) %>% 
        map_at("variable", ~factor(., levels = rev(model_variables_order), labels = rev(model_variables_order), ordered = T)) %>%
        bind_cols()
        

        
        
# model_coefficients <- coef(summary(model_multivar_log_reg))


# df_boot_coeff <- tibble(variables = row.names(model_coefficients))
# 
# n_boots <- 20
# ls_boot_coef_values <- vector(mode = "list", length = n_boots)
# 
# for (b in seq_along(1:n_boots)) {
#         boot_samples <- df_stroke_dados %>% 
#                 generate(1, type = "bootstrap") %>%
#                 ungroup()
# 
#         model_multivar_log_reg_boot <- glm(morte ~ idade_real_anos + relevel(sexo, ref = "M") + relevel(mes, ref = "1") + relevel(regiao, ref = "N"),
#                               data = boot_samples, 
#                               family = "binomial")
#         
#         ls_boot_coef_values[[b]] <- coef(summary(model_multivar_log_reg_boot))[, 1]
#         
#         if(b %% 10 == 0) {print(b)}
# }
# 
# df_boot_coeff %>% 
#         bind_cols(estimate = model_coefficients[, 1], ls_boot_coef_values) %>% 
#         gather(boot, values, V1:V20) %>%
#         group_by(variables) %>% 
#         summarise(estimate = mean(estimate),
#                   or_down = exp(2 * estimate - quantile(values, probs = 1 - 0.05/2)),
#                   or_up = exp(2 *estimate - quantile(values, probs = 0.05/2))
#                   ) %>% 
#         mutate(or = exp(estimate)) %>% 
#         select(variables, or, or_down, or_up)
        

# df_model_results <- tibble(variable = row.names(model_coefficients),
#                            or = exp(model_coefficients[,1]),
#                            or_upper = exp(model_coefficients[,1] - 1.96*model_coefficients[,2]),
#                            or_lower = exp(model_coefficients[,1] + 1.96*model_coefficients[,2])
#                            ) 


plot_ors <- ls_model_multivar_log_reg %>% 
        filter(variable != "Intercept") %>% 
        ggplot() +
        geom_point(aes(y = variable, x = or), na.rm = T) +
        geom_errorbarh(aes(xmin = or_lower, xmax = or_upper, y = variable), height = 0.25, na.rm = T) +
        geom_vline(aes(xintercept = 1), linetype = "dashed") +
        labs(x = "Odds Ratio", y = "") +
        scale_x_log10() +
        facet_wrap(. ~ regiao, ncol = 2) +
        theme_bw()




# Exports -----------------------------------------------------------------
write_csv(ls_model_multivar_log_reg, "output/df_model_results.csv")

ggsave("output/plot_ors_multivar_composto.png", plot_ors, height = 8, width = 8, dpi = 800, units = "in")
