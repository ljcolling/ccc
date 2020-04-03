suppressMessages(require(tidyverse))
suppressMessages(require(patchwork))
suppressMessages(require(bayesplay))
suppressMessages(require(BayesFactor))
suppressMessages(require(logspline))
suppressMessages(require(furrr))
suppressMessages(require(tidyverse))
suppressMessages(require(magrittr))
suppressMessages(require(logspline))
suppressMessages(require(patchwork))
suppressMessages(require(broom))
suppressMessages(require(glue))


# run this chunk to set up the simulation function

set.seed(612) # Set the seed for reproducibility

run.exp = function(sample_size, average){
  # define function for running and experiment


  # we don't know how much the pointer actually swings around,
  # so lets just pick a random range between 1 and 10!
  dev = runif(1,1,10); min_possible = 0 -dev; max_possible = 0 + dev

  # generate a sample of readings
  this_sample = runif(sample_size, min_possible, max_possible)

  # make descriptive stats
  tibble::tibble(sample_mean = mean(this_sample),
                 sample_sd = sd(this_sample),
                 n = sample_size,
                 se = sample_sd / sqrt(sample_size))
}

inParallel = TRUE #  set to TRUE for local or FALSE for cloud
if(inParallel){
  future::plan(multiprocess)
  no_of_exps = 100000 # Set the number of experiments to simulate
  map_df <- furrr::future_map_dfr
} else {
  no_of_exps =  100
}


all_exps <- map_df(1:no_of_exps, function(x)
  run.exp(sample_size = 10, average = 0) %>% dplyr::mutate(i = x)) # run the experiments

all_exps = all_exps %>% mutate(t = (sample_mean * sqrt(n)) / sample_sd)

# make a histogram of the unscaled averages
all_exps %>% ggplot(aes(x = sample_mean)) + geom_histogram(fill = "seagreen",bins = 100, na.rm = TRUE) +
  labs(x = "mean difference", y = "number of experiments") +
  xlim(c(-max(abs(all_exps$sample_mean)) * 1.10, max(abs(all_exps$sample_mean)) * 1.10)) +
  theme_minimal(18) -> mean_hist

# make a histograme of the scaled averages
all_exps %>% ggplot(aes(x = t)) + geom_histogram(fill = "seagreen", bins = 100, na.rm = TRUE) +
  labs(x = "t stat", y = "number of experiments") + xlim(c(-4,4)) +
  theme_minimal(18) -> t_hist

# convert the histogram of the scaled averages to a probability density
tibble(x = seq(-4, 4, length.out = 10000)) %>%
  mutate(y = dlogspline(x,logspline(all_exps$t))) %>%
  ggplot(aes(x = x, y = y)) + geom_line(colour = "darkblue") +
  labs(x = "t", y = "density") + xlim(c(-4,4)) +
  theme_minimal(18) -> density_plot


Fig1 = ((mean_hist | t_hist) / density_plot ) + plot_annotation(tag_levels = "A")




get_p <- function(d,n) t.test(rnorm(n = n, d ,sd = 1)) %>% broom::tidy() %>% select(statistic, p.value)

p_sims_0 = map_df(1:no_of_exps,function(x) get_p(d = 0, n = 10))
p_sims_02 = map_df(1:no_of_exps,function(x) get_p(d = 0.2, n = 10))
p_sims_05 = map_df(1:no_of_exps,function(x) get_p(d = 0.5, n = 10))
p_sims_1 = map_df(1:no_of_exps,function(x) get_p(d = 1, n = 10))

Fig2 = ggplot(p_sims_0) + geom_histogram(aes(x = p.value), binwidth = .01, fill = 'seagreen') + scale_x_continuous(name = expression(paste(~italic("p")," value")))  + scale_y_continuous(name = "frequency", breaks = NULL) + theme_minimal(18)



gen_t_p <- function(plot_data, title_text){
list(p = ggplot(plot_data) + geom_histogram(aes(x = p.value), binwidth = .01, fill = 'seagreen', na.rm = T) + scale_x_continuous(name = expression(paste(~italic("p")," value")), limits = c(0,1))  + scale_y_continuous(name = "frequency", breaks = NULL) + theme_minimal(14) +
       labs(title = glue::glue("δ = {title_text}")) + theme(title = element_text(size = 10)),

t = ggplot(plot_data) + geom_histogram(aes(x = statistic), binwidth = .01, fill = 'seagreen', na.rm = T) + scale_x_continuous(name = expression(paste(~italic("t")," value")), limits = c(-4,4))  + scale_y_continuous(name = "frequency", breaks = NULL) + theme_minimal(14) + theme(title = element_text(size = 10)))}



t_p_02 <- gen_t_p(p_sims_02,"0.2")
t_p_05 <- gen_t_p(p_sims_05, "0.5")
t_p_1 <- gen_t_p(p_sims_1, "1")

c(t_p_02,t_p_05,t_p_1) %>% wrap_plots(byrow = F) -> Fig3


do_flips = function(n_flips = 10, pr_heads = .5){ # set default to 10 flips with a fair coin

  # generate n_flips bernoulli trials and count number of heads
  tibble(n = n_flips) %>%
    mutate(h = sum(purrr::rbernoulli(n,pr_heads)),t = n - h)
}

n_flips = 10; pr_heads = .5

all_flips = map_dfr(1:no_of_exps, function(x) do_flips(n_flips, pr_heads))

all_flips_summary = all_flips %>% add_count(name = "total")  %>%
  group_by(h,total) %>% summarise(d_heads = n()) %>%
  mutate(d_heads = d_heads/total) %>% ungroup()

ggplot(all_flips_summary, aes(x = h, y = d_heads)) + geom_point(size = 4, colour = "darkblue") +
  geom_line() + theme_minimal(14) +
  scale_x_continuous(name = "number of heads", limits = c(0,10), breaks = seq(0,10,2)) +
  scale_y_continuous(name = "relative frequency") ->Fig4


coin_flip_v1 = function(n_flips, pr_heads, obs_heads){

  pmap_df(tibble(heads = 1:n_flips, flips = n_flips, pr_heads = pr_heads), # input values

          function(heads, flips, pr_heads)
            tibble(flips = flips, heads = heads,
                   freq = dbinom(heads, flips, pr_heads))) %>% # get the frequency
    mutate(our_ob = case_when(flips == n_flips & heads == obs_heads ~ TRUE, TRUE ~ FALSE)) # mark our observation

}

n_flips = 10
pr_heads_values = c(1/10,5/10,9/10) # set our pr_heads values
obs_heads = 8

coin_flip_v1_plots = pmap(tibble(n_flips = n_flips, pr_heads = pr_heads_values, obs_heads = obs_heads),
                          function(n_flips, pr_heads, obs_heads)
                            coin_flip_v1(n_flips, pr_heads, obs_heads) %>%
                            ggplot(aes(x = heads, y = freq)) + geom_line(alpha = .25) +
                            geom_point(aes(colour = our_ob), size = 3)  +
                            scale_colour_manual(guide = "none", values = c("TRUE" = "black", "FALSE" = "grey")) +
                            labs(x = glue("number of heads in {n_flips} flips"),
                                 y = "relative frequency",
                                 title = glue("Pr heads = {pr_heads}")) + theme_minimal())

# make the plots pretty and arrange them
coin_flip_v1_plots = map(coin_flip_v1_plots,
                         function(x) x + scale_x_continuous(breaks = seq(0,10,2)) +
                           scale_y_continuous(limits = c(0,.5)) +
                           theme(axis.text = element_text(size = 10),
                                 axis.title = element_text(size = 10)))

patchwork::wrap_plots(coin_flip_v1_plots,nrow = 3) + plot_annotation(tag_levels = "A") -> Fig5


number_of_heads = 8
number_of_flips = 10

# and set the range of bias to consider
pr_heads_range = seq(0,1,.1) # 0/10, 1/10 ... 9/10, 10/10

# translate our observation into the parameters needed for version 1
# generate the data and pull out the relative frequency of our specific observation

n_flips = number_of_flips
obs_heads = number_of_heads
likelihood_v1 = map_df(pr_heads_range, function(x) coin_flip_v1(n_flips, x, obs_heads) %>%
                         filter(our_ob == TRUE) %>% select(freq) %>% mutate(pr_heads = x))

likelihood_v1_plot = likelihood_v1 %>% ggplot(aes(x = pr_heads, y = freq)) + geom_point() + geom_line() +
  theme_minimal(18) + scale_x_continuous(name = "Pr heads", breaks = seq(0,1,.2)) +
  scale_y_continuous(limits = c(0,.4), name = "relative frequency") +
  labs(title = glue("likelihood function for {number_of_heads} heads in {number_of_flips} flips"),
       subtitle = glue("stopping after {number_of_flips}")) -> Fig6







save.image("~/GitHub/ccc/slides.RData")
