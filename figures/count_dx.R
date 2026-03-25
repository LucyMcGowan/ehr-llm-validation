library(ggplot2)
paper_colors = c("#ff99ff", "#787ff6", "#8bdddb", "#7dd5f6", "#ffbd59") # Define colors
plot_data <- pat_dx_flags |>
  summarize(has_match = sum(has_match), not_has_match = n() - has_match) |>
  mutate(variable = "Original Roadmap") |>
  pivot_longer(
    cols = has_match:not_has_match,
    names_to = "value",
    values_to = "n"
  ) |>
  bind_rows(
    pat_dx_flags_llm_context |>
      summarize(
        has_match = sum(has_match_llm_context),
        not_has_match = n() - has_match
      ) |>
      mutate(variable = "LLM (Context) Roadmap") |>
      pivot_longer(
        cols = has_match:not_has_match,
        names_to = "value",
        values_to = "n"
      )
  ) |>
  bind_rows(
    pat_dx_flags_llm_context_clinician |>
      summarize(
        has_match = sum(has_matched_terms_llm_context_clinician),
        not_has_match = n() - has_match
      ) |>
      mutate(variable = "LLM (Context) + Clinician Reviewed Roadmap") |>
      pivot_longer(
        cols = has_match:not_has_match,
        names_to = "value",
        values_to = "n"
      )
  ) |>
  bind_rows(
    pat_dx_flags_nocontext |>
      summarize(
        has_match = sum(has_match_llm_nocontext),
        not_has_match = n() - has_match
      ) |>
      mutate(variable = "LLM (No Context) Roadmap") |>
      pivot_longer(
        cols = has_match:not_has_match,
        names_to = "value",
        values_to = "n"
      )
  ) |>
  mutate(
    variable = factor(
      x = variable,
      levels = c(
        "LLM (No Context) Roadmap",
        "Original Roadmap",
        "LLM (Context) + Clinician Reviewed Roadmap",
        "LLM (Context) Roadmap"
      ), 
      labels = c(
        "LLM (No Context)",
        "Original",
        "LLM (Context) + Clinician",
        "LLM (Context)"
      )
    ), 
    value = factor(x = value, 
                   levels = c("has_match", "not_has_match"), 
                   labels = c("True", "False"))
  )

ggplot(plot_data, aes(x = variable, y = n, fill = value)) +
  geom_col(position = position_dodge(width = 1)) +
  geom_text(aes(label = scales::comma(n)), 
            position = position_dodge(width = 1)) +
  xlab("Roadmap") + 
  ylab("Number of Patient Diagnosis Codes") + 
  theme_minimal(base_size = 14) + 
  theme(title = element_text(face = "bold"), 
        legend.position = "inside", 
        legend.position.inside = c(0, 0.85),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12, face = "bold"), 
        legend.justification = "left", 
        legend.background = element_rect(fill = "white")) + 
  scale_y_continuous(labels = scales::comma) + 
  scale_fill_manual(values = paper_colors[c(4, 5)], 
                    name = "Has Match:")
ggsave(filename = here::here("figures/count_matched_dx.png"), 
       width = 9, height = 5, unit = "in")

ggplot(plot_data, aes(x = value, y = n, fill = variable)) +
  geom_col(position = position_dodge(width = 1)) +
  geom_text(aes(label = scales::comma(n)), 
            position = position_dodge(width = 1)) +
  xlab("Has Match") + 
  ylab("Number of Patient Diagnosis Codes") + 
  theme_minimal(base_size = 14) + 
  theme(title = element_text(face = "bold"), 
        legend.position = "inside", 
        legend.position.inside = c(0, 0.79),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12, face = "bold"), 
        legend.justification = "left", 
        legend.background = element_rect(fill = "white")) + 
  scale_y_continuous(labels = scales::comma) + 
  scale_fill_manual(values = paper_colors, 
                    name = "Roadmap:")
ggsave(filename = here::here("figures/count_matched_dx_alt.png"), 
       width = 9, height = 5, unit = "in")
