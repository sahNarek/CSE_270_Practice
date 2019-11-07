load("nba11_12.rda")
library(Matrix)
library(glmnet)
library(ggplot2)


player <- unlist(nba11_12[,1:10])
num.p <- length(unique(player))
num.p

prefix = rep(c("off","def"), each = 5 * nrow(nba11_12))
length(prefix)

tag = paste(prefix, player)
tag.factor <- as.factor(tag)

i <- rep(1:nrow(nba11_12), 10)
j <- as.numeric(tag.factor)

i.new <- c(which(nba11_12$Home == 1), i)
j.new <- c(rep(1, sum(nba11_12$Home)), j + 1)

X <- sparseMatrix(i.new, j.new)

y <- nba11_12$Points

model <- cv.glmnet(X, y, alpha = 0, standardize = F)

coef <- coef(model, s = "lambda.min")[,1]
names(coef) <- c("Intercept", "HFA", sort(unique(tag)))

alpha <- coef[1]
theta <- coef[2]
delta <- coef[2 + 1:num.p]
beta  <- coef[2 + num.p + 1:num.p]

head(sort(beta, decreasing = T), n = 10)
head(sort(delta, decreasing = T), n = 10)

off <- c("off Lebron James", "off Norris Cole", "off Udonis Haslem",
         "off Juwan Howard", "off Shane Battier")


def <- c("def Jason Terry", "def Shawn Marion", "def Delonte West",
         "def Brian Cardinal", "def Brandan Wright")
delta[def]

alpha + sum(beta[off],na.rm = T) + sum(delta[def], na.rm = T) + theta*0

def.rating = 100 * (alpha + theta/2 + delta)
off.rating = 100 * (alpha + theta/2 + beta)

pp100 = 100 * (crossprod(X, y) / colSums(X))[,1]
names(pp100) = c('Home', sort(unique(tag)))
def.pp100 = pp100[1 + 1:num.p]
off.pp100 = pp100[1 + num.p + 1:num.p]

head(sort(off.pp100, decreasing = T), n = 10)

ggplot() + 
  geom_point(aes(x = off.rating, y = def.rating))
cor(off.rating, def.rating)

ggplot() + 
  geom_point(aes(x = off.rating, y = off.pp100))
cor(off.rating, off.pp100)

ggplot() + 
  geom_point(aes(x = def.rating, y = def.pp100))
cor(def.rating, def.pp100)

