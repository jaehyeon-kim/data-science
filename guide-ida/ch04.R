## nominal (categorical)
# identify granularity and dynamic domain

## ordinal
# linear ordering relationship but not equal difference

## numeric
# discrete or continuous
# interval - no absolute zero, no unique measurement | date, temparature | quotient not make sense
# ratio - absolute zero, no unique measurement | weight, height | quotient make sense
# absolute scale - absolute zero, unique measurement | counting procedure


#### visualization
library(ggplot2)
library(grid)
library(gridExtra)

### histogram
## how to select # bins of histogram?
# Sturges' rule (k = [log2(n) + 1]) for moderately-sized normally-distributed variables
set.seed(1237)
x <- c(rnorm(500, 0, 1), rnorm(500, 3, 1))
hist(x, breaks = nclass.Sturges)
hist(x, breaks = nclass.scott)
hist(x, breaks = nclass.FD)

ggplot(data = data.frame(x = x), aes(x)) + geom_histogram(bins = 100) # too many

h <- (3.5 * sd(x))/length(x)^(1/3)
ggplot(data = data.frame(x = x), aes(x)) + geom_histogram(binwidth = h)

h <- (2 * IQR(x))/length(x)^(1/3)
ggplot(data = data.frame(x = x), aes(x)) + geom_histogram(binwidth = h)

# sensitive to outliers - try removing 3% outliers

### boxplot
set.seed(1237)
x <- data.frame(var = "x", val = rnorm(1000))
y <- data.frame(var = "y", val = rnorm(100))
ggplot(data = rbind(x, y), aes(x = var, y = val, fill = var)) + geom_boxplot()

### scatter plot
# if too many points - binning or semi-transparent
set.seed(1237)
df <- data.frame(x = rnorm(5000), y = rnorm(5000))
ggplot(data = df, aes(x = x, y = y)) + geom_point(alpha = 0.3)
ggplot(data = df, aes(x = x, y = y)) + stat_bin_hex()


gg1 <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 2)

gg2 <- ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point(size = 2)


gg3 <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 2) + geom_jitter()

gg4 <- ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point(size = 2) + geom_jitter()

grid.arrange(gg1, gg2, gg3, gg4)

### side note
library(car)
library(gpairs)
library(GGally)
library(ggfortify)

scatterplotMatrix(iris, diagonal = "histogram")
gpairs(iris, upper.pars = list(mosaic = "mosaic"),
       scatter.pars = list(col = as.numeric(iris$Species)),
       stat.pars = list(verbose = FALSE))
gpairs(iris, lower.pars = list(scatter = 'corrgram'),
       upper.pars = list(conditional = 'boxplot', scatter = 'loess'),
       scatter.pars = list(pch = 20))
ggpairs(iris, mapping = aes(color = Species), alpha=0.4)

### PCA
library(nFactors)
iris_comp <- prcomp(iris[,1:4], scale. = TRUE, center = TRUE)

summary(iris_comp)
biplot(iris_comp)
autoplot(iris_comp, data = iris, colour = "Species",
         loadings = TRUE, loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 3)

scree <- nScree(iris[,1:4])
scree$Analysis$Eigenvalues

plotnScree(scree)

predict(iris_comp, newdata = iris[1:4, 1:4])

### MDS
library(cluster)

iris_dist <- dist(scale(iris[-102, -5]))
iris_sammon <- sammon(iris_dist, k = 2)
plot(iris_sammon$points)

## metric scale
iris_dist <- dist(scale(iris[, -5]))

autoplot(iris_dist)
autoplot(cmdscale(iris_dist, eig = TRUE), label = TRUE, label.colour = 'blue', label.size = 3)

## non-metric
iris_dist <- daisy(iris[-102,], metric = "gower")
iris_mds <- isoMDS(iris_dist)

autoplot(iris_mds, shape = FALSE, label.colour = 'blue', label.size = 3)














































