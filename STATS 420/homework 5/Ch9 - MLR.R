### Parameters ###
set.seed(1337)
n = 100 # sample size
p = 3

beta_0 = 5
beta_1 = -2
beta_2 = 6
sigma  = 4


### Simulate some predictors ###
x0 = rep(1, n)
x1 = sample(seq(1, 10, length = n))
x2 = sample(seq(1, 10, length = n))
X = cbind(x0, x1, x2)


### Simulate random response ###
eps      = rnorm(n, mean = 0, sd = sigma)
y        = beta_0 + beta_1 * x1 + beta_2 * x2 + eps
sim_data = data.frame(x1, x2, y)


### Plotly ###

library(plotly)
library(reshape2)

x = x1
y = x2
z = y

data = sim_data

fig <- plot_ly(data, x = ~x1, y = ~x2, z = ~y,
               marker = list(color = ~y, 
                             colorscale = c('#FFE1A1', '#283531'),
                             showscale = TRUE))

fig


### C Matrix and Beta hat ###
C = solve(t(X) %*% X)
beta_hat = C %*% t(X) %*% y

### Check against summary output ###

model = lm(y ~ x1 + x2, data = sim_data)
summary(model)


### Check residual standard deviation estimate ###

y_hat = X %*% beta_hat

s_e = sqrt(sum((y - y_hat) ^ 2) / (n - p))
summary(model)$sigma


### Simulate betas ###

num_sims = 10000
beta_hat_2 = rep(0, num_sims)
for(i in 1:num_sims) {
  eps           = rnorm(n, mean = 0 , sd = sigma)
  sim_data$y    = beta_0 * x0 + beta_1 * x1 + beta_2 * x2 + eps
  fit           = lm(y ~ x1 + x2, data = sim_data)
  beta_hat_2[i] = coef(fit)[3]
}

### Plot simulated results ###

hist(beta_hat_2, 
     breaks = 20, 
     xlab = expression(hat(beta)[2]), 
     main = "", 
     border = "dodgerblue")


### Cereal Data ###

library(readxl)
cereal <- read_excel("~/R/STAT212/cereal.xlsx")

cereal_mod = lm(calories ~ sugars + carbo, data = cereal)
summary(cereal_mod)


### Plotly again ###

x = cereal$sugars
y = cereal$carbo
z = cereal$calories

data = cereal

fig <- plot_ly(data, x = ~x, y = ~y, z = ~z,
               marker = list(color = ~fat, 
                             colorscale = c('#FFE1A1', '#283531'),
                             showscale = TRUE))

fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'sugars'),
                                   yaxis = list(title = 'carbohydrates'),
                                   zaxis = list(title = 'calories')),
                      annotations = list(
                        x = 1.1,
                        y = 1.1,
                        text = 'calories"',
                        showarrow = TRUE
                      ))

fig


### Conf Intervals for Beta ###

confint(cereal_mod, level = 0.95)


### Predictions and Intervals at X = x ###

predict(cereal_mod, 
        newdata = data.frame(sugars = 5, carbo = 10), 
        interval = "confidence", 
        level = 0.95)

predict(cereal_mod, 
        newdata = data.frame(sugars = 5, carbo = 10), 
        interval = "prediction", 
        level = 0.95)

#Check for multidimensional extrapolation ###

plot(sugars ~ carbo, data = cereal)

