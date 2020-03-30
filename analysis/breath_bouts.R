# Splitting breaths into bouts
ibi <- bm181021_dives$data %>%
  filter(is_breath) %>%
  mutate(
    ibi_difftime = lead(dt) - dt,
    ibi = as.numeric(ibi_difftime, unit = "secs"),
    logibi = log(ibi)
  )
logibi <- ibi$logibi[-nrow(ibi)]
logibi_kmeans <- kmeans(logibi, 2)

ibi_clus <- ibi %>%
  mutate(cluster = c(logibi_kmeans$cluster, NA))
ibi_clus %>%
  ggplot(aes(logibi, dt, color = factor(cluster))) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none")

ibi_summ <- ibi_clus %>%
  drop_na() %>%
  group_by(cluster) %>%
  summarize(mu = mean(logibi),
            variance = var(logibi),
            std = sd(logibi),
            size = n()) %>%
  mutate(alpha = size / sum(size))

library(mixtools)
# Gaussian
ibi_mix <- normalmixEM(
  na.omit(ibi_clus$logibi),
  lambda = .5,
  mu = ibi_summ$mu,
  sigma = ibi_summ$std
)
ibi_mix[c("lambda", "mu", "sigma")]
ggplot(ibi_clus, aes(logibi)) +
  geom_histogram(
    aes(y = ..density..),
    binwidth = 0.25,
    color = "black",
    fill = "white"
  ) +
  stat_function(
    fun = ~ dnorm(.x, ibi_mix$mu[1], ibi_mix$sigma[1]) * ibi_mix$lambda[1],
    col = 'red'
  ) +
  stat_function(
    fun = ~ dnorm(.x, ibi_mix$mu[2], ibi_mix$sigma[2]) * ibi_mix$lambda[2],
    col = 'blue'
  )

# Gamma
# 3 components
logibi <- ibi$logibi[-nrow(ibi)]
logibi_kmeans <- kmeans(logibi, 3)
ibi_clus <- ibi %>%
  mutate(cluster = c(logibi_kmeans$cluster, NA))
ibi_summ <- ibi_clus %>%
  drop_na() %>%
  group_by(cluster) %>%
  summarize(alpha = mean(logibi)^2 / var(logibi),
            beta = var(logibi) / mean(logibi),
            size = n()) %>%
  mutate(lambda = size / sum(size))
ibi_mix <- gammamixEM(
  na.omit(ibi_clus$logibi),
  alpha = ibi_summ$alpha,
  beta = ibi_summ$beta,
  lambda = ibi_summ$lambda,
  verb = TRUE
)
# There's a warning it's not convergent but the log-lik diff was getting there
# Final value = 0.104
ibi_mix$lambda
ibi_mix$gamma.pars
p <- ggplot(ibi_clus, aes(logibi)) +
  geom_histogram(
    aes(y = ..density..),
    binwidth = 0.25,
    color = "black",
    fill = "white"
  ) +
  stat_function(
    fun = ~ dgamma(.x,
                   shape = ibi_mix$gamma.pars[1, 1],
                   scale = ibi_mix$gamma.pars[2, 1]) *
      ibi_mix$lambda[1],
    col = 'red'
  ) +
  stat_function(
    fun = ~ dgamma(.x,
                   shape = ibi_mix$gamma.pars[1, 2],
                   scale = ibi_mix$gamma.pars[2, 2]) *
      ibi_mix$lambda[2],
    col = 'blue'
  ) +
  stat_function(
    fun = ~ dgamma(.x,
                   shape = ibi_mix$gamma.pars[1, 3],
                   scale = ibi_mix$gamma.pars[2, 3]) *
      ibi_mix$lambda[3],
    col = 'green'
  ) +
  labs(x = "Log inter-breath interval [log(s)]") +
  theme_minimal()
p

# Intersection of gamma distributions
pdf_inter <- optimize(
  function(x) {
    d2 <- dgamma(x,
                 shape = ibi_mix$gamma.pars[1, 2],
                 scale = ibi_mix$gamma.pars[2, 2]) *
      ibi_mix$lambda[2]
    d3 <- dgamma(x,
                 shape = ibi_mix$gamma.pars[1, 3],
                 scale = ibi_mix$gamma.pars[2, 3]) *
      ibi_mix$lambda[3]
    abs(1 - d2 / d3)
  },
  interval = range(ibi_clus$logibi, na.rm = TRUE)
)$minimum
p + geom_vline(xintercept = pdf_inter)

# Classify by greatest probability
dcat <- function(x, cat) {
  dgamma(x,
         shape = ibi_mix$gamma.pars[1, cat],
         scale = ibi_mix$gamma.pars[2, cat]) *
    ibi_mix$lambda[cat]
}
categorize_breaths <- function(ibi) {
  logibi <- log(ibi)
  d1 <- dcat(log(ibi), 1)
  d2 <- dcat(log(ibi), 2)
  d3 <- dcat(log(ibi), 3)
  factor(max.col(cbind(d1, d2, d3), ties.method = "last"))
}
hour1 <- bm181021_dives$data %>%
  filter(as.numeric(dt - min(dt), unit = "hours") < 1)
hour1breaths <- hour1 %>%
  filter(is_breath) %>%
  mutate(breath_cat = categorize_breaths(ibi))
ggplot(hour1, aes(dt, p)) +
  geom_line() +
  geom_point(aes(y = -2, fill = breath_cat),
             data = hour1breaths,
             shape = 25,
             size = 3) +
  scale_y_reverse() +
  scale_fill_viridis_d()
breaths <- bm181021_dives$data %>%
  filter(is_breath) %>%
  mutate(breath_cat = categorize_breaths(ibi))
ggplot(breaths, aes(dt, log(ibi), fill = breath_cat)) +
  geom_point(shape = 25)

bm181021_dives$data %>%
  left_join(select(breaths, dt, breath_cat), by = "dt") %>%
  View()
