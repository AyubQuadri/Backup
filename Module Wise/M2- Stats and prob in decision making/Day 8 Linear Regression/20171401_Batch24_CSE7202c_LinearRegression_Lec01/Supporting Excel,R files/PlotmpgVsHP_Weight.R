
require(plot3D)

attach(mtcars)

# linear fit
fit <- lm(mpg ~ wt+hp)

# predict on x-y grid, for surface
wt.pred <- seq(1.5, 5.5, length.out = 30)
hp.pred <- seq(65, 230, length.out = 30)
xy <- expand.grid(wt = wt.pred, 
                  hp = hp.pred)

mpg.pred <- matrix (nrow = 30, ncol = 30, 
                    data = predict(fit, newdata = data.frame(xy), interval = "prediction"))

# predicted z-values, fitted points for droplines to surface
fitpoints <- predict(fit) 

scatter3D(z = mpg, x = wt, y = hp, pch = 18, cex = 2, 
          theta = 20, phi = 20, ticktype = "detailed",
          xlab = "wt", ylab = "hp", zlab = "mpg", clab = "mpg", 
          surf = list(x = wt.pred, y = hp.pred, z = mpg.pred, 
                      facets = NA, fit = fitpoints),
          colkey = list(length = 0.8, width = 0.4),            
          main = "mtcars")

detach(mtcars)
