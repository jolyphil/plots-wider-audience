library(dplyr)
library(ggplot2)
library(haven)
library(survey)

# Import raw GLES data ----------------------------------------------------

gles_raw <- read_dta("data-raw/GLES_pre_post_election_2021/ZA7702_v1-0-0.dta")


# Select main variables ---------------------------------------------------

gles <- gles_raw |> 
  mutate(party_id = case_when(q75a %in% 1:3 ~ "Anhänger*innen der CDU/CSU",
                              q75a == 4 ~ "Anhänger*innen der SPD",
                              q75a == 5 ~ "Anhänger*innen der FDP",
                              q75a == 6 ~ "Anhänger*innen der GRÜNEN",
                              q75a == 7 ~ "Anhänger*innen der LINKE",
                              q75a == 322 ~ "Anhänger*innen der AfD")) |> 
  mutate(across(c(q17b, q17c, q17d, q17h, q17e, q17g, q17f), 
                ~ if_else(.x %in% 1:11, .x - 6, NA_real_))) |> 
  rename(like_CDU = q17b,
         like_CSU = q17c,
         like_SPD = q17d,
         like_AfD = q17h,
         like_FDP = q17e,
         like_DIE_LINKE = q17g,
         like_GRUENE = q17f) |> 
  filter(!is.na(party_id)) |> 
  filter(!is.na(w_ipfges)) |> 
  select(starts_with("like_"), party_id, vpoint, w_ipfges, sample)


# Define survey design ----------------------------------------------------

gles_des <- svydesign(ids = ~vpoint,
                      weights = ~w_ipfges, 
                      data = gles)


# Functions to compute weighted means -------------------------------------

# Evaluation of one party, by one partisan group
mean_like <- function(in_group, var) {
  fm <- paste0("~", var) |> 
    as.formula()
  x <- svymean(fm, 
               subset(gles_des, party_id == in_group & sample == 8),
               na.rm = TRUE)
  ci <- confint(x)
  
  data.frame(party_id = in_group,
             var = var,
             estimate = x[1], 
             std.error = SE(x)[1],
             conf.low = ci[1],
             conf.high = ci[2])
}


# Combined evaluations of one party, by all partisan groups
df_one_var <- function(var) {
  out <- lapply(unique(gles$party_id),
         mean_like,
         var = var) |> 
    bind_rows()
  rownames(out) <- NULL
  out
}


# Get data plot -----------------------------------------------------------

vars <- c("like_CDU", 
          "like_CSU", 
          "like_SPD", 
          "like_AfD", 
          "like_FDP", 
          "like_DIE_LINKE", 
          "like_GRUENE")

data_plot <- lapply(vars, df_one_var) |> 
  bind_rows() |>
  mutate(var = case_when(var == "like_CDU" ~ "CDU",
                         var == "like_CSU" ~ "CSU",
                         var == "like_SPD" ~ "SPD",
                         var == "like_AfD" ~ "AfD",
                         var == "like_FDP" ~ "FDP",
                         var == "like_DIE_LINKE" ~ "DIE LINKE",
                         var == "like_GRUENE" ~ "GRÜNE"),
         var = factor(var, levels = rev(c("SPD", 
                                          "CDU", 
                                          "CSU", 
                                          "GRÜNE", 
                                          "FDP", 
                                          "AfD", 
                                          "DIE LINKE"))),
         party_id = factor(party_id, levels = c("Anhänger*innen der SPD",
                                                "Anhänger*innen der CDU/CSU",
                                                "Anhänger*innen der GRÜNEN",
                                                "Anhänger*innen der FDP",
                                                "Anhänger*innen der AfD",
                                                "Anhänger*innen der LINKE")),
         direction = (estimate > 0),
         estimate_label = sprintf("%.1f", estimate),
         estimate_label = sub("\\.", ",", estimate_label),
         estimate_label = if_else(estimate > 0, 
                                  paste0("+", estimate_label),
                                  estimate_label),
         estimate_label_pos = if_else(estimate > 0, 
                                      estimate + 1.7, 
                                      estimate - 1.7)) 


# Render plot -------------------------------------------------------------

data_plot |> 
  ggplot(aes(x = estimate, 
             y = var, 
             fill = direction, 
             color = direction,
             label = estimate_label)) +
  geom_vline(xintercept = 0,
             linetype="dashed") +
  geom_col(width = 0.05) +
  geom_point() +
  geom_text(aes(x = estimate_label_pos), size = 4) +
  facet_wrap(vars(party_id)) +
  labs(title="Parteiskalometer nach Parteiidentifikation",
       subtitle = "Frage: Was halten Sie so ganz allgemein von den einzelnen politischen Parteien? \nSkala: -5 (überhaupt nichts) bis +5 (sehr viel)",
       x = "Gewichteter Mittelwert",
       y = "",
       caption = "Grafik: Philippe Joly (FU Berlin), 2023 \nQuelle: GLES Querschnitt 2021, Nachwahl \nLizenz: CC BY 4.0") +
  theme_bw() +
  expand_limits(x = c(-7, 7)) +
  scale_x_continuous(
    breaks = c(-5, 0, 5),
    labels = c("-5", "0", "+5"),
    minor_breaks = NULL
  ) +
  theme(plot.caption = element_text(hjust = 0), 
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot",
        legend.position = "none") #NEW parameter

ggsave("figures/2023-06-18_affective_polarization_gles_2021.png", 
       width = 8, height = 6)
