p8160\_project3\_jsg
================
Jared Garfinkel
4/21/2020

``` r
df = read_csv("./covid_final.csv") %>% 
  janitor::clean_names() %>% 
  mutate(province_state = factor(province_state),
         country_region = factor(country_region),
         date = as.Date(date, "%m/%d/%y"))
```

    ## Parsed with column specification:
    ## cols(
    ##   province_state = col_character(),
    ##   country_region = col_character(),
    ##   lat = col_double(),
    ##   long = col_double(),
    ##   date = col_character(),
    ##   confirmed_cases = col_double(),
    ##   fatalities = col_double()
    ## )

``` r
df_raw = df %>% 
  dplyr::select(lat, long, confirmed_cases, fatalities) %>% 
  drop_na()
```

``` r
# correlation matrix
res <- round(cor(df_raw) %>% as.matrix(), 2)

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.45)
```

<img src="p8160_project3_jsg_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" />

``` r
cor(df_raw)
```

    ##                         lat        long confirmed_cases fatalities
    ## lat              1.00000000 -0.42909791      0.02585938 0.02401693
    ## long            -0.42909791  1.00000000      0.09841674 0.05962983
    ## confirmed_cases  0.02585938  0.09841674      1.00000000 0.90346925
    ## fatalities       0.02401693  0.05962983      0.90346925 1.00000000

``` r
df_filtered = df %>% 
  dplyr::select(everything(), confirmed_cases, -fatalities)

y = df_filtered %>% 
  pull(confirmed_cases)

x = df_filtered %>% 
  dplyr::select(province_state:date) %>% 
  mutate(date = factor(date),
         date = fct_inorder(date))

dat = cbind(y, x)
```

``` r
log_lik = function(cumsum = pull(dat, y), params = list(a = 1000, b = 2, c = 100)) {
  dat %>% group_by(country_region, province_state)
  t = 0
  while (y > 0) {
    t = t + 1
  }
  for (i in 1:n_distinct(pull(dat, country_region))) {
    like[i] = params$a / (1 + exp(-params$b * (t - params$c)))
    log_lik = log(like)
    return(log_lik)
}

log_lik()
```

``` r
# logistic function

logistic_stuff = function(x = pull(dat, x), y = pull(dat, y), params = list(a = 1000, b = 2, c = 100), func = log_lik) {
  dat %>% group_by(country_region, province_state)
  t = 0
  while (y > 0) {
    t = t + 1
  }
  u = params$a / (1 + exp(-params$b * (t - params$c)))
  expu = exp(u)
  loglik = func(y, params)
  p = expu/(1 + expu)
  grad = rep(0, 3)
  for (i in 1:length(grad)) {
    grad[i] = sum(t(x[,i]) %*% (y - p))
  }
  return(grad)
}
  Hess <- matrix(0, length(grad), length(grad))
  for (i in 1:nrow(X)) {
    Hess = Hess - X[i,] %*% t(X[i,]) * p[i] * (1 - p[i])
  }
  return(list(loglik = loglik, grad = grad, Hess = Hess))
}

logistic_stuff()

dat %>% 
  group_by(country_region, province_state) %>% 
  filter(confirmed_cases == 0)

pull(dat, confirmed_cases)

logistic_stuff()

  u = params$a / (1 +  exp(-params$b * (t - params$c)))
  
}
```

## Clustering

``` r
int_slope_df <- int_slope_dat %>% 
  nest(data = year:ppp) %>%
  mutate(
    models = map(data, ~lm(ppp ~ year, data = .x)),
    result = map(models, broom::tidy)
  ) %>% 
  select(county, result) %>% 
  unnest(result) %>% 
  select(county, term, estimate) %>% 
  pivot_wider(
    names_from = term,
    values_from = estimate
  ) %>% 
  rename(int = "(Intercept)", slope = year)
# Cluster data by intercept and slope #
km_fit = 
  kmeans(
    int_slope_df[,c(2,3)]  %>% scale, 
    centers = 3)
int_slope_df =
  broom::augment(km_fit, int_slope_df)
# Create a ggplot of the pills data #
pills_plot <- left_join(int_slope_dat, int_slope_df) %>% 
  group_by(county) %>% 
  ggplot(aes(x = year, y = ppp, color = .cluster, text = county)) + 
  geom_point() + 
  geom_path() +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6), labels = c("2006", "2007", "2008", "2009", "2010", "2011", "2012")) +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("Pills Bought Per Person")
ggplotly(pills_plot, tooltip = "text")
```
