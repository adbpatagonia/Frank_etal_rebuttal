
library(plotly)
library(TMB)
compile("analysis/ricker.cpp")
dyn.load(dynlib("analysis/ricker"))

cap <- read.csv("data/capelin_biomass_all_surveys.csv")
names(cap) <- c("year", "index", "index_upr", "index_lwr", "survey")

plot_ly(data = cap, x = ~year, y = ~index, color = ~survey) %>% 
    add_ribbons(ymin = ~index_lwr, ymax = ~index_upr, 
                line = list(width = 0), opacity = 0.5, showlegend = FALSE) %>% 
    add_trace(mode = "markers+lines") %>% 
    layout(yaxis = list(title = "Capelin biomass (kilotonnes)", type = "log"),
           xaxis = list(title = "Year"))

years <- sort(unique(cap$year))
# years <- c(years, (max(years) + 1):(max(years) + 5))

dat <- list(years = years,
            log_y = log(cap$index),
            year = cap$year,
            iyear = cap$year - min(cap$year),
            isurvey = as.numeric(factor(cap$survey)) - 1,
            survey = cap$survey)
dat$n <- length(dat$log_y)
dat$nyears <- length(dat$years)
dat$nsurvey <- length(unique(dat$isurvey))

par <- list(log_K = 1,
            log_r = 0,
            log_sigma_y = rep(0, dat$nsurvey),
            log_q = rep(0, dat$nsurvey),
            log_x = rep(10, dat$nyears),
            log_sigma_x = 0)

obj <- MakeADFun(dat, par, DLL = "ricker", random = "log_x",
                 map = list(log_q = factor(c(1, NA, 2, 3, 4)))) # have to fix one q to 1
opt <- nlminb(obj$par, obj$fn, obj$gr)
sd_rep <- sdreport(obj)
sd_rep

est <- as.list(sd_rep, "Est")
sd <- as.list(sd_rep, "Std")
lwr <- lapply(names(est), function(i) est[[i]] - 1.96 * sd[[i]])
upr <- lapply(names(est), function(i) est[[i]] + 1.96 * sd[[i]])
names(lwr) <- names(upr) <- names(est)

pro <- data.frame(year = dat$years, est = est$log_x, 
                  lwr = lwr$log_x, upr = upr$log_x)
pro$diff <- c(0, diff(pro$est))
pro$cdiff <- cumsum(pro$diff)

plot(pro$year, pro$diff, type = "l")

plot_ly(data = pro, x = ~year) %>% 
    add_ribbons(ymin = ~exp(lwr), ymax = ~exp(upr), name = "95% CI", 
                alpha = 0.2, color = I("darkgrey")) %>% 
    add_lines(y = ~exp(est), name = "estimate", color = I("black")) %>% 
    layout(xaxis = list(title = "Year"),
           yaxis = list(title = "Biomass", type = "log"))

ind <- names(sd_rep$value) == "pred_y"
fits <- data.frame(year = dat$year, survey = dat$survey, log_y = dat$log_y, 
                   pred_y = sd_rep$value[ind], sd_y = sd_rep$sd[ind])
fits$pred_y[fits$pred_y == 0] <- NA
fits$lwr <- fits$pred_y - 1.96 * fits$sd_y
fits$upr <- fits$pred_y + 1.96 * fits$sd_y

plot(fits$pred_y, (fits$log_y - fits$pred_y))
plot(fits$year, (fits$log_y - fits$pred_y))

plot_ly(data = fits, x = ~year, color = ~survey) %>% 
    add_ribbons(ymin = ~exp(lwr), ymax = ~exp(upr), alpha = 0.2) %>% 
    add_lines(y = ~exp(pred_y)) %>% 
    add_markers(y = ~exp(log_y)) %>% 
    layout(xaxis = list(title = "Year"),
           yaxis = list(title = "I", type = "log"))




## Plot for paper ---------------------------------

library(ggplot2)
library(cowplot)
library(scales)
library(grid)
library(gridExtra)
library(RColorBrewer)

dg <- 0.5
mypalette <- brewer.pal(5, "Set1")

## subset to year range by survey
split_fits <- split(fits, fits$survey)
sub_fits <- lapply(names(split_fits), function(nm) {
    sub <- split_fits[[nm]]
    rng <- range(sub$year[!is.na(sub$log_y)])
    sub[sub$year >= rng[1] & sub$year <= rng[2], ]
})
sub_fits <- do.call(rbind, sub_fits)
sub_fits <- merge(sub_fits, cap, by = c('year', 'survey'))

sub_fits <- transform(sub_fits,
                      survey = factor(survey,levels=c(
                          "fa",
                          "ussrfa",
                          "sa",
                          "ussrsa",
                          'tb')))

p <- ggplot(data = sub_fits, aes(x = year, y = index, group = survey, shape = survey, color = survey))
 p <- p + geom_linerange( aes(ymin = index_lwr, ymax = index_upr),
                        position = position_dodge(width = dg), size = 0.5, na.rm = T, alpha = 0.4)

p <- p + geom_point(position = position_dodge(width = dg), size = 1.5)
#p <- p + geom_line(aes(y = exp(pred_y)))

p <- p + scale_y_log10(label = comma, limits = c(1,10000), breaks = c(1,10,100,1000,10000))
p <- p + annotation_logticks(sides = "l")
p <- p + theme_set(theme_cowplot())
p <- p + labs(x = '', y = '')
p <- p + theme(legend.title = element_blank())#, axis.text.x  = element_text(angle=90, vjust=0.5))
p <- p + scale_color_manual(values =  mypalette[c(1,2,4,3,5)], 
                            breaks = c( levels(unique(sub_fits$survey))),
                            labels = c("2J3K Fall Canada", '2J3K Fall USSR', "3L Spring Canada", "3LNO Spring USSR", 'Trinity Bay (3L)'))
p <- p +   scale_shape_manual(breaks = c( levels(unique(sub_fits$survey))),
                              values = c(15, 16, 18, 17, 8),
                              #                         :18,8),
                              labels = c("2J3K Fall Canada", '2J3K Fall USSR', "3L Spring Canada", "3LNO Spring USSR", 'Trinity Bay (3L)'))
p <- p + scale_x_continuous(breaks = seq(1975, 2015, by = 5))
p <- p + theme(legend.position = c(0.05, 0.18), 
               legend.text = element_text(size = 7),
               legend.key.size = unit(10, 'point'),
               plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
               axis.title = element_blank())

p
p + guides(shape = guide_legend(override.aes = list(linetype = c(0, 0, 1, 0, 1))))

p + scale_linetype_discrete(breaks = c('tb', 'sa') ) 

fits_p <- p


#20,12000

p <- ggplot(data = pro, aes(x = year))
p <- p + geom_ribbon(aes(ymin = exp(lwr), ymax = exp(upr)), alpha = 0.25)
#p <- p + geom_line(aes(y = exp(est)))
p <- p + scale_y_log10(labels = comma, limits = c(1,12000), breaks = c(1,10,100,1000,10000))
p <- p + annotation_logticks(sides = "l")
p <- p + theme_set(theme_cowplot())
p <- p + scale_x_continuous(breaks = seq(1975, 2015, by = 5))
p <- p + labs(x = '', y = '')
p <- p + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
               axis.title = element_blank())
p
pro_p <- p

p <- plot_grid(pro_p, fits_p, nrow = 2, labels = "AUTO",
               align = "v", axis = "l", label_x = 0.92, label_y = 0.92,
               label_fontface = 1)

y_grob <- textGrob("Capelin biomass (kilotonnes)", rot = 90)
x_grob <- textGrob("Year")

p <- grid.arrange(arrangeGrob(p, left = y_grob, bottom = x_grob, 
                         padding = unit(1, "line")))

# labs(x = 'Year', y = 'Capelin biomass (kilotonnes)')

save_plot("analysis/biomass_plot_v4.png", p, base_height = 6, base_aspect_ratio = 0.9) # make room for figure legend)




