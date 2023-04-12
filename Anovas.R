# Author: Arti
# Date: April 4, 2023

# Load the necessary libraries
library(car)
library(readxl)

file_path <- file.choose()
data <- read_excel(file_path, sheet = "datalist")

# Change the name of the "BAD" column to BAD
colnames(data)[10] <- "Bad"

# Identify the outliers and remove them manually
boxplot(data$Bad)
data <- data[data$Bad < 0.5, ]

#merge instagram with facebook
#data$Platform[data$Platform=='Intagram']= 'FB'

# function to save the graph
save_plot <- function(plot_name, plot_object, width, height, units, dpi) {
  ggsave(
    filename = plot_name,
    plot = plot_object,
    width = width,
    height = height,
    units = units,
    dpi = dpi
  )
}

#############################################
#          BAD~Platform                 #
#############################################
# Check for normality
library(ggplot2)
ggplot(data, aes(x = Bad)) + geom_histogram() + facet_wrap(~Platform)
library(dplyr)
data %>%
  group_by(Platform) %>%
  summarise(mean_value = median(Bad), sd_value = sd(Bad))

# Use by() to split the data by platform and apply the Shapiro-Wilk test
tapply(data$Bad, data$Platform, function(x) {
  result <- shapiro.test(x)
  paste0("Shapiro-Wilk test: W = ", round(result$statistic, 3), ", p = ", round(result$p.value, 3))
})

library(moments)
skewness(data$Bad, na.rm = TRUE)
kurtosis(data$Bad, na.rm = TRUE)

# Check for homogeneity of variances
ggplot(data, aes(x = Platform, y = Bad)) +
  geom_boxplot() +
  labs(x = "Platform", y = "Bad") +
  ggtitle("Platform ~ Bad")

library(car)
leveneTest(Bad ~ factor(Platform), data = data)
fligner.test(Bad ~ factor(Platform), data = data)


#welch-anova with pairwise comparison graphs
library(dplyr)
library(ggstatsplot)
library(gridExtra)
library(PMCMRplus)
library(forcats)# to order the graphs by means

data$Platform <- with(data, forcats::fct_reorder(Platform, Bad, .fun = median))
set.seed(4)   # for Bayesian reproducibility of 95% CIs

p1<-ggbetweenstats(
  data = data %>% filter(!(Platform %in% c("AMZN", "Intagram"))),
  x    = Platform, 
  y    = Bad, 
  conf.level = 0.95,
  pairwise.comparisons = TRUE,
  pairwise.display = "s",
  type = "parametric",
  var.equal= FALSE,
  bf.message= FALSE,
  ggtheme = ggplot2::theme_grey(),
  ggstatsplot.layer = FALSE)

p2<-ggbetweenstats(
  data = data %>% filter(!(Platform %in% c("AMZN", "Intagram"))),
  x    = Platform, 
  y    = Bad, 
  conf.level = 0.95,
  pairwise.comparisons = TRUE,
  pairwise.display = "s",
  type = "np",
  var.equal= FALSE,
  bf.message = FALSE,
  p.adjust.method = "bonferroni",
  ggtheme = ggplot2::theme_grey(),
  ggstatsplot.layer = FALSE)

save_plot("Platform_np.png", p2, 6, 4.5, "in", 300)
save_plot("Platform_p.png", p1, 6, 4.5, "in", 300)

grid.arrange(plot1, plot2)

##### bootstarpping pairwise comparisons manually

# gameshowell_pairwise_stat <- function(data, indices, ind_var, dep_var) {
#   d <- data[indices, ] # Resample the data with the given indices
#   gameshowell_result <- gamesHowellTest(dep_var~ ind_var, data) # Perform Games-Howell pairwise test
#   
#   # Extract the p-values
#   p_values <- gameshowell_result$p.value
#   print(p_values)
#   
#   # Create a matrix of pairwise comparisons
#   num_groups <- length(unique(d[[ind_var]]))
#   p_matrix <- matrix(NA, nrow = num_groups, ncol = num_groups)
#   p_matrix[lower.tri(p_matrix)] <- p_values
#   p_matrix[upper.tri(p_matrix)] <- t(p_matrix)[upper.tri(p_matrix)]
#    
#   # Apply the Bonferroni adjustment to the lower triangle of the matrix
#   p_matrix[lower.tri(p_matrix)] <- p.adjust(p_matrix[lower.tri(p_matrix)], method = "bonferroni")
#   
#   return(p_matrix)
# }
# 
# # Perform bootstrapping
# set.seed(1234) # Set seed for reproducibility
# nboot = 1000
# boot_result <- boot(data = data %>% filter(!(Platform %in% c("AMZN", "Intagram"))),
#                     statistic = function(data, indices) gameshowell_pairwise_stat(data, indices, "Platform", "Bad"),
#                     R = nboot)
# 
# 



#############################################
#          BAD~type of harm(sector)                 #
#############################################
ggplot(data, aes(x = Bad)) + geom_histogram() + facet_wrap(~sector)
# Use by() to split the data by platform and apply the Shapiro-Wilk test
tapply(data$Bad, data$sector, function(x) {
  result <- shapiro.test(x)
  paste0("Shapiro-Wilk test: W = ", round(result$statistic, 3), ", p = ", round(result$p.value, 3))
})

# Calculate skewness and kurtosis for each level of 'sector'
skew_kurt <- data %>%
  group_by(sector) %>%
  summarise(
    skewness = skewness(Bad),
    kurtosis = kurtosis(Bad)
  )

# Display the results
print(skew_kurt)
# Check for homogeneity of variances
ggplot(data, aes(x = sector, y = Bad)) +
  geom_boxplot() +
  labs(x = "Sector", y = "Bad") +
  ggtitle("Sector ~ Bad")

library(car)
leveneTest(Bad ~ factor(sector), data = data %>% 
             mutate(sector = ifelse(
               sector == "kids", "mental health", sector)))
fligner.test(Bad ~ factor(sector), data = data)

# Homogeneity assumption did not match, thus conduting Welch-ANOVA
set.seed(4)   # for Bayesian reproducibility of 95% CIs
data$sector <- with(data, forcats::fct_reorder(sector, Bad, .fun = mean))
p1<-ggbetweenstats(
  data = data %>% 
    mutate(sector = ifelse(
      sector == "kids", "mental health", sector)),
  x    = sector, 
  y    = Bad, 
  conf.level = 0.95,
  pairwise.comparisons = TRUE,
  type = "parametric",
  nboot = 10,
  pairwise.display = "all",
  var.equal = TRUE,
  bf.message = FALSE,
  ggtheme = ggplot2::theme_grey(),
  ggstatsplot.layer = FALSE)

p2<-ggbetweenstats(
  data = data %>% 
    mutate(sector = ifelse(
      sector == "kids", "mental health", sector)) %>%
    mutate(sector = fct_reorder(sector, Bad, .fun = median)),
  x = sector,
  y = Bad,
  conf.level = 0.95,
  pairwise.comparisons = TRUE,
  type = "nonparametric",
  pairwise.display = "s",
  p.adjust.method = "bonferroni",
  nboot = 10,
  var.equal = TRUE,
  ggtheme = ggplot2::theme_grey(),
  ggstatsplot.layer = FALSE
)

save_plot("sector_np.png", p2, 10, 6, "in", 300)
save_plot("sector_p.png", p1, 10, 6, "in", 300)

grid.arrange(p1,p2)


#some may argue that non-parameteric pairwise comparisons test such as Conovertest is more suitable in this case
conover_test <- conover.test(data$Bad, data$sector, method = "bonferroni")

#############################################
#         Bad~ modality.                    #
#############################################
ggplot(data, aes(x = Bad)) + geom_histogram() + facet_wrap(~KIND)
# Use by() to split the data by platform and apply the Shapiro-Wilk test
tapply(data$Bad, data$KIND, function(x) {
  result <- shapiro.test(x)
  paste0("Shapiro-Wilk test: W = ", round(result$statistic, 3), ", p = ", round(result$p.value, 3))
})
# Check for homogeneity of variances
ggplot(data, aes(x = KIND, y = Bad)) +
  geom_boxplot() +
  labs(x = "Modality", y = "Bad") +
  ggtitle("Bad ~ Modality")

library(car)
leveneTest(Bad ~ factor(KIND), data = data)
fligner.test(Bad ~ factor(KIND), data = data)  #assumption has not violated

oneway.test(Bad ~ KIND, data = data, var.equal = TRUE)

data$KIND <- with(data, forcats::fct_reorder(KIND, Bad, .fun = median))
p1<- ggbetweenstats(
  data = data,
  x    = KIND, 
  y    = Bad, 
  type = "p",
  pairwise.comparisons = T,
  pairwise.display = "s",
  ggtheme = ggplot2::theme_grey(),
  ggstatsplot.layer = FALSE)

p2<- ggbetweenstats(
  data = data,
  x    = KIND, 
  y    = Bad, 
  type = "np",
  pairwise.comparisons = T,
  pairwise.display = "s",
  p.adjust.method = "bonferroni",
  ggtheme = ggplot2::theme_grey(),
  ggstatsplot.layer = FALSE)

save_plot("modality_np.png", p2, 11, 4.5, "in", 300)
save_plot("modality_p.png", p1, 11, 4.5, "in", 300)
grid.arrange(p1,p2)
conover_test <- conover.test(data$Bad, data$KIND, method = "bonferroni")

#############################################
#         Bad~ SOURCE.                      #
#############################################

ggplot(data, aes(x = Bad)) + geom_histogram() + facet_wrap(~SOURCE)
# Use by() to split the data by platform and apply the Shapiro-Wilk test
tapply(data$Bad, data$SOURCE, function(x) {
  result <- shapiro.test(x)
  paste0("Shapiro-Wilk test: W = ", round(result$statistic, 3), ", p = ", round(result$p.value, 3))
})
# Check for homogeneity of variances
ggplot(data, aes(x = SOURCE, y = Bad)) +
  geom_boxplot() +
  labs(x = "SOURCE", y = "Bad") +
  ggtitle("Bad ~ SOURCE")

library(car)
leveneTest(Bad ~ factor(SOURCE), data = data)
fligner.test(Bad ~ factor(SOURCE), data = data)  #assumption has been violated

oneway.test(Bad ~ SOURCE, data = data, var.equal = FALSE)

data$SOURCE <- with(data, forcats::fct_reorder(SOURCE, Bad, .fun = median))
p1<- ggbetweenstats(
  data = data,
  x    = SOURCE, 
  y    = Bad, 
  type = "p",
  title = "Bad ~ SOURCE",
  subtitle = "parameteric",
  pairwise.comparisons = T,
  pairwise.display = "s",
  var.equal= FALSE,
  ggtheme = ggplot2::theme_grey(),
  ggstatsplot.layer = FALSE)

p2<- ggbetweenstats(
  data = data,
  x    = SOURCE, 
  y    = Bad, 
  type = "np",
  subtitle = "parameteric",
  pairwise.comparisons = T,
  pairwise.display = "s",
  var.equal= FALSE,
  p.adjust.method = "bonferroni",
  ggtheme = ggplot2::theme_grey(),
  ggstatsplot.layer = FALSE,
  p.value.label = function(x) round(x, digits = 3))

save_plot("badsource_np.png", p2, 6, 4.5, "in", 300)
save_plot("badsource_p.png", p1, 6, 4.5, "in", 300)
grid.arrange(p1,p2)
conover_test <- conover.test(data$Bad, data$SOURCE, method = "bonferroni")

# Result: insignificant for both parametric and nonparametric

#############################################
#         Good~ SOURCE                      #
#############################################

ggplot(data, aes(x = GOOD)) + geom_histogram() + facet_wrap(~SOURCE)
# Use by() to split the data by platform and apply the Shapiro-Wilk test
tapply(data$GOOD, data$SOURCE, function(x) {
  result <- shapiro.test(x)
  paste0("Shapiro-Wilk test: W = ", round(result$statistic, 3), ", p = ", round(result$p.value, 3))
})
# Check for homogeneity of variances
ggplot(data, aes(x = SOURCE, y = GOOD)) +
  geom_boxplot() +
  labs(x = "SOURCE", y = "GOOD") +
  ggtitle("GOOD ~ SOURCE")

library(car)
leveneTest(GOOD ~ factor(SOURCE), data = data)
fligner.test(GOOD ~ factor(SOURCE), data = data)  #assumption has been violated

oneway.test(GOOD ~ SOURCE, data = data, var.equal = FALSE)

data$SOURCE <- with(data, forcats::fct_reorder(SOURCE, GOOD, .fun = median))
p1<- ggbetweenstats(
  data = data,
  x    = SOURCE, 
  y    = GOOD, 
  type = "p",
  bf.message = FALSE,
  pairwise.comparisons = T,
  pairwise.display = "s",
  ggtheme = ggplot2::theme_grey(),
  ggstatsplot.layer = FALSE)

p2<- ggbetweenstats(
  data = data,
  x    = SOURCE, 
  y    = GOOD, 
  type = "np",
  pairwise.comparisons = T,
  pairwise.display = "s",
  var.equal= TRUE,
  p.adjust.method = "bonferroni",
  ggtheme = ggplot2::theme_grey(),
  ggstatsplot.layer = FALSE)

grid.arrange(p1,p2)

save_plot("goodsource_np.png", p2, 6, 4.5, "in", 300)
save_plot("goodsource_p.png", p1, 6, 4.5, "in", 300)
conover_test <- conover.test(data$GOOD, data$SOURCE, method = "bonferroni")

# Result: significant for both parametric and nonparametric



