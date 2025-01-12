spearman_examples <- function() {
  x_ <- seq(0, 10, length.out = 50)
  y_noise <- x_ + rnorm(length(x_), 0, 1.5)
  y_rand <- rnorm(length(x_), 0, 1.5)
  
  
  p <- ggplot(data = NULL, aes(x = x_)) +
    labs(x = "x", y = "y") +
    guides(x = "none", y = "none")
  
  p_exp <- p + geom_point(aes(y = exp(x_))) +
    geom_label(aes(label = sprintf("r_sp = %.2f", cor(x_, exp(x_), method = "spearman")), x = 5, y = 15000))
  
  p_sin <- p + geom_point(aes(y = sin(pi/3 * x_))) +
    geom_label(aes(label = sprintf("r_sp = %.2f", cor(x_, sin(pi/3 * x_), method = "spearman")), x = 5, y = 1.3)) +
    ylim(-1.5, 1.5)
  
  p_lin <- p + geom_point(aes(y = -1*x_)) +
    geom_label(aes(label = sprintf("r_sp = %.2f", cor(x_, -1*x_, method = "spearman")), x = 6.5, y = -1.5))
  
  p_const <- p + geom_point(aes(y = rep(1, length(x_)))) +
    geom_label(aes(label = sprintf("r_sp = %.2f", cor(x_, rep(1, length(x_)), method = "spearman")), x = 5, y = 1.5)) +
    scale_y_continuous(limits = c(0, 2))
  
  p_noise <- p + geom_point(aes(y = y_noise)) +
    geom_label(aes(label = sprintf("r_sp = %.2f", cor(x_, y_noise, method = "spearman")), x = 4, y = 12)) +
    ylim(-3, 13)
  
  p_rand <- p + geom_point(aes(y = y_rand)) +
    geom_label(aes(label = sprintf("r_sp = %.2f", cor(x_, y_rand, method = "spearman")), x = 5, y = 6)) +
    ylim(-8, 8)
  
  
  p_out <- ((p_exp | p_lin | p_noise) / (p_sin | p_const | p_rand)) + 
    plot_annotation(title = "Spearman Korrelationskoeffizient")
  
  return(p_out)
}








             