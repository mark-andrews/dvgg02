library(tidyverse)

data_df <- tibble(var1 = c(1, 3, 7, 11), 
                  var2 = c(5, 7, 10, 12), 
                  var3 = c('a', 'b', 'a', 'b')
)

# ggplot object with a layer of points
ggplot(data_df,
       mapping = aes(x = var1, y = var2)
) + geom_point()
       
# ggplot object with a layer with a line
ggplot(data_df,
       mapping = aes(x = var1, y = var2)
) + geom_line()

p1 <- ggplot(data_df,
             mapping = aes(x = var1, y = var2)
)

# scatterplot (as above)
p1 + geom_point()

# line plot (as above)
p1 + geom_line()

# points and line plot
p1 + geom_point() + geom_line()

# create a ggplot object with x, y, and colour mapping
p2 <- ggplot(data_df,
             mapping = aes(x = var1, y = var2, colour = var3)
)

p2 + geom_point() 
p2 + geom_line()
p2 + geom_point() + geom_line()

# create a ggplot obj with x,y, and shape mapping
p3 <- ggplot(data_df,
             mapping = aes(x = var1, y = var2, shape = var3)
)

p3 + geom_point()
p3 + geom_line()
p3 + geom_point(size = 10)

# create a ggplot obj with x,y, and linetype mapping
p4 <- ggplot(data_df,
             mapping = aes(x = var1, y = var2, linetype = var3)
)

p4 + geom_point()
p4 + geom_line()


# create a ggplot obj with x,y, and linetype, colour, shape mapping
p5 <- ggplot(data_df,
             mapping = aes(x = var1, 
                           y = var2, 
                           linetype = var3,
                           colour = var3,
                           shape = var3)
)

p5 + geom_point()
p5 + geom_line()
p5 + geom_point() + geom_line()

# different versions of the same thing: 
p6 <- ggplot(data_df,
             mapping = aes(x = var1, 
                           y = var2)
)

p7 <- ggplot(data_df, aes(var1, var2))

p7 <- ggplot(data_df, aes(var1, var2, colour = var3))

# aes mapping specific for e.g geom_point 

ggplot(data_df) +
  geom_point(aes(var1, var2)) +
  geom_line(aes(var1, var2))

ggplot(data_df, aes(var1, var2)) +
  geom_point(aes(colour = var3)) +
  geom_line(aes(linetype = var3))


# get data from url
weight_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/dvgg02/master/data/weight.csv")

# histogram of weight,using all the defaults...
ggplot(weight_df, 
       mapping = aes(x = weight)
) + geom_histogram()
