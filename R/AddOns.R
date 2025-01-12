### Toy Example Pearson vs Spearman
#### Make Data ######
X <- list(x = seq(0, 10, length.out = 1000))
X$Exponential <- exp(X$x)
X$Logarithmus <- log(X$x)
X$Inverse <- 1/(X$x + 1)
X$Linear <- -2 * X$x + 2
X$Linear_verrauscht <- X$x + rnorm(length(X$x), sd = .7)
X$Sinus <- sin(.5*pi*X$x)

##### long format #####
X_ <- as.data.frame(X) %>%
  pivot_longer(-x, names_to = "fun", values_to = "y")

##### plot data #####
X_ %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(alpha = .2) +
  #geom_text(aes(label = mean(r_sp), x = .7, y = ) +
  facet_wrap(~fun, scales = "free_y") +
  theme_light()

##### show r's ######
X_  %>%
  group_by(fun) %>%
  summarize(r_spearman = round(cor(x, y, method = "spearman"), 2),
            r_pearson = round(cor(x, y, method = "pearson"), 2)) %>%
  rename("erzeugende Funktion" = "fun")