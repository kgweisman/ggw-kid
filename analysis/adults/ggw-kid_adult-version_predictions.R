# plot by pair ----
plotTable = d1 %>% 
  group_by(predicate, pairCat, pair) %>% 
  summarise(mean = mean(responseNumFlip, na.rm = T),
            sd = sd(responseNumFlip, na.rm = T),
            n = length(responseNumFlip))

plotTableFake = plotTable %>% select(-sd, -n)
plotTableFake$mean = c(-0.2, -0.2, -0.2, 
                       -0.2, -0.2, -0.2,
                       -0.2, -0.2, -0.2, 
                       -0.3, -0.2, -0.1, -0.3, -0.2, -0.1, -0.3, -0.2, -0.1, 
                       -2, -2, -2, -2, -2, -2, -2, -2, -2,
                       -2, -2, -2, -2, -2, -2, -2, -2, -2,
                       -2, -2, -2, -2, -2, -2, -0.2, -0.2, -0.2,
                       -0.2, -0.3, -0.4, 
                       -0.2, -0.3, -0.4,                        
                       -0.2, -0.2, -0.2,
                       -0.4, -0.3, -0.2, -0.7, -0.6, -0.5, -1, -.9, -.8, 
                       -2, -2, -2, -2, -2, -2, -2, -2, -2,
                       -2, -2, -2, -2, -2, -2, -2, -2, -2,
                       -2, -2, -2, -2, -2, -2, -0.2, -0.2, -0.2,
                       -1, -1.1, -1.2, 
                       -1, -1.1, -1.2, 
                       -1, -1.1, -1.2, 
                       -0.4, -0.3, -0.2, -0.7, -0.6, -0.5, -1, -.9, -.8, 
                       -1.4, -1.3, -1.2, -1.7, -1.6, -1.5, -2, -1.9, -1.8,
                       -1.4, -1.3, -1.2, -1.7, -1.6, -1.5, -2, -1.9, -1.8,
                       -2, -2, -2, -2, -2, -2, -0.4, -0.3, -0.2)
                       
                       
ggplot(aes(x = 
             reorder(pair,
                     as.numeric(
                       factor(pairCat,
                              levels = c("human.human",
                                         "animal.animal",
                                         "tech.tech",
                                         "human.animal",
                                         "human.tech",
                                         "animal.tech",
                                         "control")))),
           y = mean, 
           fill = pairCat), 
       data = plotTableFake) +
  facet_grid(predicate ~ .,
             labeller = labeller(predicate = c("hunger" = "...get hungry",
                                               "feelings" = "...have feelings",
                                               "thinking" = "...think"))) +
  geom_bar(stat = "identity", position = "identity") +
#   geom_errorbar(aes(ymin = mean - 2*sd/sqrt(n),
#                     ymax = mean + 2*sd/sqrt(n),
#                     width = 0.1)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme_bw() +
  coord_cartesian(ylim = c(-2, 2)) +
  scale_fill_brewer(type = "qual",
                    palette = 2) +
  theme(text = element_text(size = 20),
        legend.position = "top",
        axis.text.x = element_text(angle = 60,
                                   hjust = 1)) +
  labs(title = "MEAN COMPARISON SCORES\nby character pair\n",
       x = "\nCHARACTER PAIR",
       y = "MEAN RESPONSE\n-2 (-1): 1st character is much (slightly) more likely to...,\n0: characters are both equally likely to...,\n+2 (+1): 2nd character is much (slightly) more likely to...\n",
       fill = "PAIR CATEGORY: ")

# plot by pair category ----
plotTable2 = d1 %>% 
  group_by(predicate, pairCat) %>% 
  summarise(mean = mean(responseNumFlip, na.rm = T),
            sd = sd(responseNumFlip, na.rm = T),
            n = length(responseNumFlip))

plotTable2Fake = plotTable2 %>% select(-sd, -n)
plotTable2Fake$mean = c(-0.1, -0.1, -0.1, -0.1, -2, -2, -2,
                        -0.1, -1, -0.1, -1, -2, -2, -2,
                        -1, -1, -1, -1, -2, -2, -2)

ggplot(aes(x = 
             reorder(pairCat,
                     as.numeric(
                       factor(pairCat,
                              levels = c("human.human",
                                         "animal.animal",
                                         "tech.tech",
                                         "human.animal",
                                         "human.tech",
                                         "animal.tech",
                                         "control")))),
           y = mean, 
           fill = pairCat), 
       data = plotTable2Fake) +
  facet_grid(predicate ~ .,
             labeller = labeller(predicate = c("hunger" = "...get hungry",
                                               "feelings" = "...have feelings",
                                               "thinking" = "...think"))) +
  geom_bar(stat = "identity", position = "identity") +
#   geom_errorbar(aes(ymin = mean - 2*sd/sqrt(n),
#                     ymax = mean + 2*sd/sqrt(n),
#                     width = 0.1)) +
  coord_cartesian(ylim = c(-2, 2)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme_bw() +
  scale_fill_brewer(type = "qual",
                    palette = 2) +
  theme(text = element_text(size = 20),
        legend.position = "none",
        axis.text.x = element_text(angle = 60,
                                   hjust = 1)) +
  labs(title = "MEAN COMPARISON SCORES\nby character pair category\n",
       x = "\nCHARACTER PAIR CATEGORY",
       y = "MEAN RESPONSE\n-2 (-1): 1st character is much (slightly) more likely to...,\n0: characters are both equally likely to...,\n+2 (+1): 2nd character is much (slightly) more likely to...\n")




