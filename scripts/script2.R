library(tidyverse)

weight_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/dvgg02/master/data/weight.csv")

ggplot(weight_df,
       mapping = aes(x = height, fill = gender)
) + geom_histogram(binwidth = 3)

ggplot(weight_df,
       mapping = aes(x = height, colour = gender)
) + geom_freqpoly(binwidth = 2)

ggplot(weight_df,
       mapping = aes(x = height, colour = gender)
) + geom_freqpoly(binwidth = 2, position = 'stack')

ggplot(weight_df,
       mapping = aes(x = height, colour = gender)
) + geom_freqpoly(binwidth = 2, position = 'dodge')

ggplot(weight_df,
       mapping = aes(x = height, colour = gender)
) + geom_freqpoly(binwidth = 2, position = 'identity')

# we will come by to this...
ggplot(weight_df,
       mapping = aes(x = height, fill = gender)
) + geom_area(stat = 'bin', 
              binwidth = 5, 
              position = 'identity', alpha = 0.6)

ggplot(weight_df,
       mapping = aes(x = height, fill = gender)
) + geom_density()

ggplot(weight_df,
       mapping = aes(x = height, fill = gender)
) + geom_density(bw = 20)

ggplot(weight_df,
       mapping = aes(x = height, fill = gender)
) + geom_density(bw = 2)


# line plots --------------------------------------------------------------

nottingham_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/dvgg02/master/data/nottingham_temp.csv")

ggplot(nottingham_df,
       aes(x = month, y = value)
) + geom_point()

ggplot(nottingham_df,
       aes(x = month, y = value)
) + geom_point() + geom_line()

ggplot(nottingham_df,
       aes(x = month, y = value, group = year)
) + geom_point() + geom_line()

ggplot(nottingham_df,
       aes(x = month, 
           y = value, 
           group = year, 
           colour = factor(year))
) + geom_point() + geom_line()

ggplot(nottingham_df,
       aes(x = month, 
           y = value, 
           group = year, 
           linetype = factor(year))
) + geom_point() + geom_line()

M <- lm(value ~ poly(month, 2), 
        data = nottingham_df)

notts_df <- tibble(month = seq(12))
library(modelr)
add_predictions(notts_df, M) %>% 
  ggplot(aes(x = month, y = pred)) + 
  geom_point() +
  geom_line()

notts_df2 <- as_tibble(predict(M, 
                               newdata = notts_df,
                               interval = 'confidence')) %>% 
  bind_cols(notts_df)

ggplot(notts_df2,
       aes(x = month, y = fit)
) + geom_point()

ggplot(notts_df2,
       aes(x = month, y = fit, ymin = lwr, ymax = upr)
) + geom_pointrange()

ggplot(notts_df2,
       aes(x = month, y = fit, ymin = lwr, ymax = upr)
) + geom_pointrange() + geom_line()

ggplot(notts_df2,
       aes(x = month, y = fit, ymin = lwr, ymax = upr)
) + geom_linerange() + geom_line()

ggplot(notts_df2,
       aes(x = month, y = fit, ymin = lwr, ymax = upr)
) + geom_ribbon(alpha = 0.25) + geom_line()


ggplot(notts_df2,
       aes(x = month)
) + geom_line(aes(y = fit)) +
  geom_line(aes(y = lwr), linetype = 2) +
  geom_line(aes(y = upr), linetype = 2) +
  geom_ribbon(aes(ymax = upr, ymin = lwr), alpha = 0.1)


ggplot(nottingham_df,
       mapping = aes(x = year, y = month, fill = value)
) + geom_tile() + scale_fill_gradient(low = 'yellow',
                                      high = 'red')

# spatial maps ------------------------------------------------------------

mapdata <- read_csv("https://raw.githubusercontent.com/mark-andrews/dvgg02/master/data/local_authority_map_data.csv")

ggplot(mapdata,
       mapping = aes(x = long, 
                     y = lat, 
                     group = group)
) + geom_polygon(colour = 'white', 
                 fill = 'grey60',
                 size = 0.1) + 
  coord_equal()

referendum_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/dvgg02/master/data/EU-referendum-result-data.csv")

referendum_map_df <- inner_join(mapdata, 
                                referendum_df,
                                by = c('id' = 'Area_Code')
)

ggplot(referendum_map_df,
       mapping = aes(x = long, 
                     y = lat, 
                     group = group,
                     fill = Pct_Leave),
) + geom_polygon(colour = 'white', 
                 size = 0.1) + 
  coord_equal() +
  scale_fill_distiller(palette = 3,
                       limits = c(0, 80)) +
  theme_minimal()
                       

# Correlation matrices ----------------------------------------------------

library(GGally)
ggcorr(mtcars)
ggcorr(mtcars, 
       label = TRUE)

ggcorr(mtcars,
       label = TRUE,
#       nbreaks = 10,
       palette = 'Greys')

ggcorr(select(mtcars, 1:5), label = T)

ggpairs(select(mtcars, 1:5))

ggpairs(select(mtcars, 1:5),
        diag = list(continuous = 'barDiag'),
)

ggpairs(flea, 
        aes(fill = species, colour = species),
        columns = 2:7)
  
data(tips, package = 'reshape')      
tips_df <- select(tips, 1:4)
ggpairs(tips_df)


ggpairs(tips_df,
        upper = list(continuous = 'points',
                     combo = 'box_no_facet'),
        lower = list(continuous = 'density',
                     combo = 'dot_no_facet'),
)


# fine control ------------------------------------------------------------

p12 <- ggplot(weight_df,
              mapping = aes(x = height, 
                            y = weight, 
                            colour = gender)
) + geom_point()

p12 + theme_classic()
p12 + theme_minimal()
p12 + theme_bw()
p12 + theme_dark()

library(ggthemes)

p12 + theme_fivethirtyeight()
p12 + theme_economist()
p12 + theme_economist_white()
p12 + theme_excel()
p12 + theme_excel_new()
p12 + theme_stata()
p12 + theme_tufte()

p13 <- p12 + theme_classic()
p13 + xlab('Height (cm)') + ylab('Weight (kg)')
p13 + labs(x = "Height (cm)",
           y = "Weight (kg)")
p13 + ggtitle("A scatterplot of height and weight")

library(latex2exp)
TeX('$\\alpha$')

p13 + xlab(TeX("$\\sqrt{\\alpha}$"))

p13 + scale_colour_manual(values = c('blue', 'green'))

p14 <- ggplot(weight_df,
              aes(x = weight, fill = gender)
) + geom_histogram(binwidth = 3)

p14 + scale_fill_manual(values = c('green', 'red'))

ggplot(weight_df,
       mapping = aes(x = height, 
                     y = weight)
) + geom_point() + scale_colour_manual(values = c('blue'))

p13 + theme(legend.position = 'bottom')
p13 + theme(legend.position = 'top')
p13 + theme(legend.position = 'left')
p13 + theme(legend.position = 'none')
