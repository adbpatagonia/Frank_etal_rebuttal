
library(data.table)
library(cowplot)

## Calculate mean weight at age from the survey data
## These means are bias-corrected using the equations in
## Morgan and Hoeing 1997

## Use data from the main strat analysis
load("interimsteps/condition_fall_2J3KL.RData")
#out <- rv_data$strat$fall_2J3KL

## Strat totals by age and length group
tots <- out$strat1$length$abundance$summary[, c("survey.year", "length", "total")]
alk <- na.omit(data.table(out$raw.data$age.length.keys))
setnames(alk, names(alk), c("length", "age", "survey.year", "n"))
alk[, prop := n / sum(n), by = c("survey.year", "length")]
tots <- merge(tots, alk, by = c("survey.year", "length"))
tots$total <- tots$total * tots$prop

## Work out length weight proportions
ag <- out$raw.data$age.growth
prop <- data.table(ag)
prop <- prop[!is.na(prop$weight), ]
prop <- prop[, list(n = .N), by = c("length", "weight", "survey.year", "NAFOdiv")]
prop[, prop := n / sum(n), by = c("survey.year", "length", "NAFOdiv")]
tots <- merge(prop, tots[, c("survey.year", "length", "age", "total")],
              allow.cartesian = TRUE, by = c("survey.year", "length"))
tots$total <- tots$total * tots$prop
tots <- tots[, list(total = sum(total)), by = c("survey.year", "weight", "age", "NAFOdiv")]

## Calculate weighted mean weight at age
means <- tots[, list(wt_mean = sum(weight * total) / sum(total)), by = c("survey.year", "age", "NAFOdiv")]
means$age <- as.numeric(means$age)

## Calculate unweighted means at age
unwt_means <- data.table(ag)
unwt_means <- unwt_means[, list(unwt_mean = mean(weight, na.rm = TRUE)),
                         by = c("survey.year", "age", "NAFOdiv")]
means <- merge(means, unwt_means, by = c("survey.year", "age", "NAFOdiv"))

## Scale mean weights at age by age for plotting
means[, scaled_mean := scale(wt_mean), by = "age"]
ggplot(data = means[means$age %in% 3:5],
       aes(x = survey.year, y = scaled_mean, colour = factor(age))) +
  geom_line(size = 0.75) +
  geom_point(size = 2) +
  geom_vline(xintercept = 1991, colour = "grey", size = 1, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1985, 2015, by = 5)) +
  labs(x = "Year", y = "Cod weight at age (normalized)", colour = "Age") +
  facet_wrap(.~ , NAFOdiv)
ggsave("data-raw/weights/scaled_mean_weight_at_age.png", dpi = 600, height = 4.5, width = 7)


## Cross-check with previous values
weight <- read.delim("data-raw/weights/weights.txt", header = TRUE, na.strings = ".")
weight$X0 <- NULL

n <- nrow(weight)
year <- seq(1983, length = n)
age <- 1:16
n.age <- length(age)
n.year <- length(year)
agem <- matrix(age, nrow = n.year, ncol = n.age, byrow = T)
yearm <- matrix(year, nrow = n.year, ncol = n.age, byrow = F)

wts <- data.frame(
  weight = as.numeric(unlist(weight)),
  age = as.numeric(unlist(agem)),
  year = as.numeric(unlist(yearm))
)

temp <- means[, c("survey.year", "age", "wt_mean", "unwt_mean")]
names(temp) <- c("year", "age", "wt_mean", "unwt_mean")
wts <- merge(wts, temp, by = c("year", "age"))
# plot(wts$weight, wts$unwt_mean)
## These differences are concerning...have to investigate!


## Scale mean weights at age by age for plotting
wts <- data.table(wts)
wts[, scaled_mean := scale(weight), by = "age"]
ggplot(data = wts[wts$age %in% 3:5],
       aes(x = year, y = scaled_mean, colour = factor(age))) +
  geom_line(size = 0.75) +
  geom_point(size = 2) +
  geom_vline(xintercept = 1991, colour = "grey", size = 1, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1985, 2015, by = 5)) +
  labs(x = "Year", y = "Cod weight at age (normalized)", colour = "Age", title = "Previous weighted means") +
  theme_bw()

wts[, scaled_mean := scale(wt_mean), by = "age"]
ggplot(data = wts[wts$age %in% 3:5],
       aes(x = year, y = scaled_mean, colour = factor(age))) +
  geom_line(size = 0.75) +
  geom_point(size = 2) +
  geom_vline(xintercept = 1991, colour = "grey", size = 1, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1985, 2015, by = 5)) +
  labs(x = "Year", y = "Cod weight at age (normalized)", colour = "Age", title = "New weighted means") +
  theme_bw()

wts[, scaled_mean := scale(unwt_mean), by = "age"]
ggplot(data = wts[wts$age %in% 3:5],
       aes(x = year, y = scaled_mean, colour = factor(age))) +
  geom_line(size = 0.75) +
  geom_point(size = 2) +
  geom_vline(xintercept = 1991, colour = "grey", size = 1, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1985, 2015, by = 5)) +
  labs(x = "Year", y = "Cod weight at age (normalized)", colour = "Age", title = "Unweighted means") +
  theme_bw()

