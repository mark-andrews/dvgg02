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
