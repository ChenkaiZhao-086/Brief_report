seir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I / N
    dE <- beta * S * I / N - sigma * E
    dI <- sigma * E - gamma * I
    dR <- gamma * I
    NewConfirmI <- (beta * S * I / N) - NewConfirmI

    return(list(c(dS, dE, dI, dR, NewConfirmI)))
  })
}

RunModel <- function(input) {
  N <- input$N
  I0 <- input$I
  R0 <- input$R
  R0_values <- c(1.25, 2.5, 5, 10)
  incubation_period <- input$incubation_period
  infectious_period <- input$infectious_period
  simulation_time <- input$simulation_time

  S0 <- N - I0 - R0

  initial_state <- c(S = S0, E = 0, I = I0, R = R0, NewConfirmI = 0)
  times <- seq(0, simulation_time, by = 1)

  results <- list()

  for (i in 1:4) {
    parameters <- c(
      beta = R0_values[i] / infectious_period,
      sigma = 1 / incubation_period,
      gamma = 1 / infectious_period,
      N = N
    )

    output <- ode(y = initial_state, times = times, func = seir_model, parms = parameters)
    output <- output %>%
      as.data.frame() %>%
      mutate(SummI = cumsum(NewConfirmI))

    results[[i]] <- as.data.frame(output)
    results[[i]]$R0 <- R0_values[i]
  }

  all_results <- do.call(rbind, results)

  NewConfirm_Fig <- ggplot(all_results, aes(x = time, color = as.factor(R0))) +
    geom_line(aes(y = NewConfirmI)) +
    # geom_line(aes(y = S, linetype = "S"), size = 1) +
    # geom_line(aes(y = E, linetype = "E"), size = 1) +
    # geom_line(aes(y = I, linetype = "I"), size = 1) +
    # geom_line(aes(y = R, linetype = "R"), size = 1) +
    scale_color_discrete(name = "R0") +
    # scale_linetype_manual(
    #   name = "类别",
    #   values = c(
    #     "S" = "solid", "E" = "dashed",
    #     "I" = "dotted", "R" = "dotdash"
    #   )
    # ) +
    # facet_wrap(~R0) +
    labs(
      x = "时间(天)", y = "人数"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 18, face = "bold"),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text.x = element_text(margin = margin(b = 5)),
      strip.text = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = 14, face = "bold"),
      legend.position = "right"
    )

  Cum_Fig <- ggplot(all_results, aes(x = time, color = as.factor(R0))) +
    geom_line(aes(y = SummI)) +
    scale_color_discrete(name = "R0") +
    # scale_linetype_manual(
    #   name = "类别",
    #   values = c(
    #     "S" = "solid", "E" = "dashed",
    #     "I" = "dotted", "R" = "dotdash"
    #   )
    # ) +
    # facet_wrap(~R0) +
    labs(
      x = "时间(天)", y = "人数"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 18, face = "bold"),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text.x = element_text(margin = margin(b = 5)),
      strip.text = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = 14, face = "bold"),
      legend.position = "right"
    )

  return(list(
    NewConfirm_Fig,
    Cum_Fig
  ))
}
