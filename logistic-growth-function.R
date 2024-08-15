rm(list = ls())

logistic_growth <- function(N0, K, r, time){
  Nt <- K / (1 + ((K-N0) / N0) * exp(-r * time)) 

return(Nt)  
}

logistic_growth(N0 = 100, K = 6000, r = 0.27, time = 40)

time_vec <- seq(from = 0, to = 50, by = 0.1)

pop_1 <- logistic_growth(N0 = 100, K = 6000, r = 0.27, time = time_vec)

pop_vec_1 <- vector(mode = "numeric", length = length(time_vec))

for (i in seq_along(time_vec)) {
  population <- logistic_growth(N0 = 100, K = 6000, r = 0.27, time = time_vec[i])
  pop_vec_1[i] <- population
}

pop_time_1 <- data.frame(time_vec, pop_vec_1)

ggplot(data = pop_time_1, aes(x = time_vec, y = pop_vec_1)) +
  geom_line()

r_seq <- seq(from = 0.2, to = 0.4, by = 0.01)

out_matrix <- matrix (nrow = length(time_vec), ncol = length (r_seq))

for (i in seq_along(r_seq)) { #outer loop of growth rates
  for (j in seq_along(time_vec)) { # inner loop of time steps
    population <- logistic_growth(N0 = 100, K = 6000, r = r_seq[i], time = time_vec[j])
  out_matrix[j,i] <- population
  }
}

out_df <- data.frame(out_matrix, time = time_vec)
colnames(out_df) <- c(paste0("growth_rate_", r_seq), "time")

out_df_long <- out_df %>%
  pivot_longer(cols = -time, names_to = "growth_rate", values_to = "populatin_size")

ggplot(data = out_df_long, aes(x =time, y = populatin_size)) +
  geom_line(aes(color = growth_rate), show.legend = FALSE) +
  theme_minimal()
  