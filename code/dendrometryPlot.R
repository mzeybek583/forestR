

## Forest Dendrometry Plots
##


D <- c(0.565228,
       0.543312,
       0.690734,
       0.653966,
       0.552616,
       0.420684,
       0.397006,
       0.399038,
       0.388556,
       0.391012,
       0.371722,
       0.839868,
       0.583356,
       0.299964,
       0.324718,
       0.542074,
       0.33405,
       0.243276,
       0.325134,
       0.524618
)
H <- c(14.380,
       14.010,
       14.420,
       12.940,
       13.950,
       13.280,
       10.360,
       10.080,
       10.530,
       7.660,
       8.760,
       15.360,
       12.060,
       10.060,
       9.500,
       14.780,
       10.260,
       7.990,
       9.380,
       11.270
)
Area <- c(133.95,
          83.65,
          130.65,
          111.21,
          118.67,
          40.81,
          35.59,
          32.99,
          33.96,
          23.55,
          39.19,
          108.07,
          76.70,
          32.52,
          26.16,
          78.70,
          29.63,
          17.23,
          30.97,
          92.91
)
df <- data.frame(D=D, H=H, Area=Area)
mod1 <- lm(H~D, data = df)
summary(mod1)
mod2 <- lm(Area~D, data = df)
summary(mod2)
mod3 <- lm(Area~H, data = df)
summary(mod3)

library(ggplot2)
library(ggpmisc)
eq <- function(H,D) {
  m <- lm(D ~ H)
  as.character(
    as.expression(
      substitute(italic(D) == a + b %.% italic(H)*","~~italic(r)^2~"="~r2,
                 list(a = format(coef(m)[1], digits = 4),
                      b = format(coef(m)[2], digits = 4),
                      r2 = format(summary(m)$r.squared, digits = 3)))
    )
  )
}

ggplot(df,aes(x = H, y = D)) + 
  geom_point() + 
  geom_smooth(method = "lm", se=TRUE)+
  geom_text(x = 9, y = 0.7, label = eq(df$H,df$D), parse = TRUE)

# Area ~ D ----------------------------------------------------------------


eq <- function(Area,D) {
  m <- lm(D ~ Area)
  as.character(
    as.expression(
      substitute(italic(D) == a + b %.% italic(Area)*","~~italic(r)^2~"="~r2,
                 list(a = format(coef(m)[1], digits = 4),
                      b = format(coef(m)[2], digits = 4),
                      r2 = format(summary(m)$r.squared, digits = 3)))
    )
  )
}

ggplot(df,aes(x = Area, y = D)) + 
  geom_point() + 
  geom_smooth(method = "lm", se=TRUE)+
  geom_text(x = 45, y = 0.7, label = eq(df$Area,df$D), parse = TRUE)


# Area ~ H ----------------------------------------------------------------



eq <- function(Area,H) {
  m <- lm(H ~ Area)
  as.character(
    as.expression(
      substitute(italic(H) == a + b %.% italic(Area)*","~~italic(r)^2~"="~r2,
                 list(a = format(coef(m)[1], digits = 4),
                      b = format(coef(m)[2], digits = 4),
                      r2 = format(summary(m)$r.squared, digits = 3)))
    )
  )
}

ggplot(df,aes(x = Area, y = H)) + 
  geom_point() + 
  geom_smooth(method = "lm", se=TRUE)+
  geom_text(x = 45, y = 15, label = eq(df$Area,df$H), parse = TRUE)
