### How does the #cs courses affect the programming proficiency 

### Load Packages
library(tidyverse)

### Import Data
background <- read_csv('data/background-clean.csv')
interest <- read_csv('data/interest-clean.csv')

### Variable I: #cs courses 
## We include PSTAT100, 131, and 134 to count the no. cs courses 
background$cs_total <- rowSums(background[ , c("CS9", "CS16", "CS130","CS165","CS5",
                                    "PSTAT100","PSTAT131","PSTAT134")])
head(background$cs_total) #check the first five students
dist_CS <- hist(background$cs_total,
                main = "Distribution of Number of CS Courses Taken",
                xlab = "Number of CS Courses",
                ylab = "Number of Students",
                col = "skyblue",
                border = "white")

### Variable II: programming proficiency level
background$prof <- as.numeric(factor(background$prog.prof,
                              levels = c("beg", "int", "adv"),
                              labels = c(1, 2, 3)))

### Variable III: programming confidence level - Optional

### Analysis I: Simple Linear Model
model <- lm(prof ~ cs_total, background)
summary(model)


plot <- ggplot(background, aes(x = cs_total, y = prof)) +
  geom_jitter(width = 0.2, height = 0.1, aes(color = as.factor(prof)), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_brewer(palette = "Set1", name = "Proficiency Level",
                     labels = c("Beginner", "Intermediate", "Advanced")) +
  labs(
    title = "Relationship Between Number of CS Courses and Programming Proficiency",
    x = "Number of CS Courses Taken",
    y = "Programming Proficiency (1 = Beg, 2 = Int, 3 = Adv)"
  ) +
  theme_minimal(base_size = 14)

### Save Output
ggsave("results/question_2_plot.png", plot, width = 8, height = 6, dpi = 300)




