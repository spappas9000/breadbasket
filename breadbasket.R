
library(tidyverse)

breadbasket <- read_csv("breadbasket.csv") %>%
  mutate(Sum = Yellow + Orange)

breadbasket %>%
  group_by(Yellow) %>%
  reframe(n = n())

breadbasket %>%
  group_by(Orange) %>%
  reframe(n = n())

chisq.test(
  table(factor(breadbasket$Yellow)),
  p = rep(1/6, 6)
)

chisq.test(
  table(factor(breadbasket$Orange)),
  p = rep(1/6, 6)
)

chisq.test(
  table(factor(breadbasket$Sum, levels = 2:12)),
  p = c(1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1)/36
)

plot_die <- breadbasket %>%
  rename(`Yellow Die` = Yellow, `Orange Die` = Orange) %>%
  pivot_longer(c(`Yellow Die`, `Orange Die`), names_to = "Dice", values_to = "Number") %>%
  ggplot(aes(x = Number, fill = Dice)) +
  geom_bar(color = "black") +
  geom_hline(yintercept = 37/6, linetype = "dashed") +
  facet_wrap(~ Dice) +
  scale_fill_manual(values = c("Yellow Die" = "yellow", "Orange Die" = "orange")) +
  scale_x_continuous(
    labels = 1:6, 
    breaks = 1:6
  ) +
  scale_y_continuous(
    limits = c(0, 10), 
    expand = c(0, 0), 
    labels = seq(0, 10, by = 2), 
    breaks = seq(0, 10, by = 2)
  ) +
  labs(
    title = "Distribution of Die Faces",
    x = "Die Face",
    y = "Count",
    fill = NULL,
    caption = "Plot: Steven Pappas (@StevenPappas1)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "bold"),
    legend.position = "none",
    strip.background = element_rect(fill = "white"),
    axis.ticks = element_blank()
  )

breadbasket_sum <- data.frame(
  Sum = 2:12,
  Observed = breadbasket %>%
    mutate(Sum = factor(Sum, levels = 2:12)) %>%
    group_by(Sum, .drop = F) %>%
    reframe(N = n()) %>%
    pull(N),
  Expected = c(1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1)/36*37
) %>%
  pivot_longer(!Sum, names_to = "Type", values_to = "Value")

plot_sum <- breadbasket_sum %>%
  ggplot(aes(x = Sum, y = Value, fill = Type)) +
  geom_col(color = "black", position = "dodge") +
  scale_x_continuous(
    labels = 1:12, 
    breaks = 1:12
  ) +
  scale_y_continuous(
    limits = c(0, 10),
    expand = c(0, 0),
    labels = seq(0, 10, by = 2),
    breaks = seq(0, 10, by = 2)
  ) +
  labs(
    title = "Distribution of Dice Sums",
    x = "Dice Sum",
    y = "Count",
    fill = NULL,
    caption = "Plot: Steven Pappas (@StevenPappas1)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "bold"),
    legend.position = "top",
    strip.background = element_rect(fill = "white"),
    axis.ticks = element_blank()
  )

breadbasket_nbinom <- data.frame(
  x = 4:50
) %>%
  mutate(p = pnbinom(x - 4, size = 4, p = 1/6))

plot_nbinom <- breadbasket_nbinom %>%
  ggplot(aes(x = x, y = p, fill = x == 37)) +
  geom_col(color = "black") +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(3, 49),
    labels = seq(4, 48, by = 4),
    breaks = seq(4, 48, by = 4)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1),
    labels = seq(0.2, 0.8, by = 0.2),
    breaks = seq(0.2, 0.8, by = 0.2)
  ) +
  scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "gray")) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(
    title = "Probability of Four Rolls of the Same Face on Two Dice by Roll",
    x = "Roll",
    y = "Probability of Success",
    fill = NULL,
    caption = "Plot: Steven Pappas (@StevenPappas1)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(face = "bold"),
    legend.position = "none",
    axis.ticks = element_blank()
  )

ggsave(plot = plot_die, filename = "plot_die.jpeg", device = "jpeg")

ggsave(plot = plot_sum, filename = "plot_sum.jpeg", device = "jpeg")

ggsave(plot = plot_nbinom, filename = "plot_nbinom.jpeg", device = "jpeg")
