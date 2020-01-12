# Sources -----------------------------------------------------------------
source("Code_Library/functions.R")

# Parameters --------------------------------------------------------------

set.seed(123)
B_inc <- rnorm(n = 1000, sd = 0.2)

B <- cumsum(B_inc)

subordinator <- c(
  seq(1:10),
  rep(10,5),
  seq(25, 100),
  seq(150, 200),
  rep(200, 2),
  seq(500, 600),
  rep(600, 50),
  seq(600, 1000)
)

Brownian_path_graph <- ggplot() + geom_line(aes(x = 1:length(B), y = B, color = "Brownian Motion")) + ylab("") + xlab("") +
  theme(
  legend.position = "top"
  ) + 
  scale_color_manual(labels = c("Brownian Motion", "ww"),
                     breaks = c("Brownian Motion", "ww"),
                     values = c("Red", "Blue"),
                     name = "") + scale_x_continuous(breaks = c(0, length(B)),
                                                     labels = c(0, 1))

subordinator_graph <- ggplot() + geom_line(aes(x = 1:length(subordinator), y = subordinator, color = "Subordinator")) +
  ylab("") + xlab("") +
  geom_rect(aes(xmin = 10, xmax = 17, ymin = -Inf, ymax = Inf ), alpha = 0.5) + 
  geom_rect(aes(xmin = 90, xmax = 95, ymin = -Inf, ymax = Inf ), alpha = 0.5) + 
  geom_rect(aes(xmin = 142, xmax = 147, ymin = -Inf, ymax = Inf ), alpha = 0.5) + 
  geom_rect(aes(xmin = 245, xmax = 299, ymin = -Inf, ymax = Inf, alpha = 0.5 )) + 
  theme(
    legend.position = "top"
  )  + 
  scale_color_manual(labels = c("Subordinator"),
                     breaks = c("Subordinator"),
                     values = c("Blue"),
                     name = "") + scale_alpha_continuous(guide = "none") + scale_x_continuous(breaks = c(0, length(subordinator)),
                                                                                              labels = c(0, 1))

Brownian_subordinator <- ggplot() + geom_line(aes(x = 1:length(B[subordinator]),
                                                  y = B[subordinator], color = "Brownian subordination")) +
  ylab("") + xlab("") +
  geom_rect(aes(xmin = 10, xmax = 17, ymin = -Inf, ymax = Inf ), alpha = 0.5) + 
  geom_rect(aes(xmin = 90, xmax = 95, ymin = -Inf, ymax = Inf ), alpha = 0.5) + 
  geom_rect(aes(xmin = 142, xmax = 147, ymin = -Inf, ymax = Inf ), alpha = 0.5) + 
  geom_rect(aes(xmin = 245, xmax = 299, ymin = -Inf, ymax = Inf, alpha = 0.5 )) + 
  theme(
    legend.position = "bottom"
  )  + 
  scale_color_manual(labels = c("Brownian subordination"),
                     breaks = c("Brownian subordination"),
                     values = c("Black"),
                     name = "") + scale_alpha_continuous(guide = "none") + scale_x_continuous(breaks = c(0, length(subordinator)),
                                                                                                labels = c(0, 1))


ggsave(filename = "Brownian_path_graph.png", plot = Brownian_path_graph, , dpi = 200,
              height = 2.3, width = 4, units = "in")

ggsave(filename = "subordinator_graph.png", plot = subordinator_graph, , dpi = 200,
       height = 2.3, width = 4, units = "in")

ggsave(filename = "Brownian_subordinator.png", plot = Brownian_subordinator, , dpi = 200,
       height = 2.3, width = 6, units = "in")
