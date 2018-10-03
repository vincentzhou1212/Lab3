library(ggplot2)
library(gapminder)
library(tidyr)
library(dplyr)

## Use base
curve(dnorm, from = -4, to = 4, ylab = "PDF", main = "Pr(X =< -1)")
coord.x = c(-4, seq(-4, -1, by = 0.1), -1)
coord.y = c(0, dnorm(seq(-4, -1, by = 0.1)), 0)
polygon(coord.x, coord.y, col = 2)

## Use ggplot
x = seq(-4, 4, by = 0.1)
y = dnorm(x)
data = data.frame(x, y)
ggplot(data, aes(x = x, y = y)) + 
  geom_line() +
  geom_ribbon(data = data[data$x < -1, ], 
              aes(x, ymin = 0, ymax = y), fill = "red", alpha = 1) +
  theme_classic() +
  labs(y = "Density", title = "Pr(X =< -1)")

#######################################################################################
## Problem 2
data2 = gapminder %>%
  filter(year == 2007)

highGPD = which((data2$continent == "Europe") & (data2$gdpPercap > 40000))

x1 = data2$gdpPercap[highGPD[1]]
y1 = data2$lifeExp[highGPD[1]]
t1 = data2$country[highGPD[1]]
x2 = data2$gdpPercap[highGPD[2]]
y2 = data2$lifeExp[highGPD[2]]
t2 = data2$country[highGPD[2]]
  
ggplot(data2, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  theme_bw() +
  annotate("text", x = x1, y = y1, label = t1, colour = "blue", 
           size = 4.5, hjust = 0.45, vjust = 1.5) +
  annotate("text", x = x2, y = y2, label = t2, colour = "blue", 
           size = 4.5, hjust = 0.45, vjust = 1.3) +
  labs(x = "GDP per capita", y = "life expectancy")

########################################################################################
## HW Question

highestGPD = which(data2$gdpPercap > 39000)

xmin = min(data2$gdpPercap[highestGPD])
xmax = max(data2$gdpPercap[highestGPD])
ymin = min(data2$lifeExp[highestGPD])
ymax = max(data2$lifeExp[highestGPD])

ggplot(data2, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(shape = 1) +
  theme_bw() + 
  annotate("rect", xmin = xmin-400, xmax = xmax+1000, ymin = ymin-2.6, ymax = ymax+2.8, 
           alpha = 0.3, fill = "red") + 
  annotate("text", x = xmin, y = ymin, label = "Countries with \n highest GDP", 
           size = 4.5, hjust = -0.1, vjust = 1.7) +
  labs(x = "GDP per capita", y = "life expectancy")
