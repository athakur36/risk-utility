# Author: Arti
# Date: April 4, 2023

# Load the necessary libraries
library(car)
library(readxl)

file_path <- file.choose()
data <- read_excel(file_path, sheet = "datalist")

# Change the name of the "BAD" column to BAD
colnames(data)[11] <- "Bad"
colnames(data)[9] <- "GOOD"
colnames(data)[6] <- "SeedInput"

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
library(dplyr)
ggplot(data=data %>% dplyr::filter(!(Platform %in% c("AMZN", "Intagram"))), aes(x = Bad)) + geom_histogram() + facet_wrap(~Platform)

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
ggplot(data= data %>% dplyr::filter(!(Platform %in% c("AMZN", "Intagram"))), aes(x = Platform, y = Bad)) +
  geom_boxplot() +
  labs(x = "Platform", y = "Bad") +
  ggtitle("Platform ~ Bad") 

library(car)
leveneTest(Bad ~ factor(Platform), data )
fligner.test(Bad ~ factor(Platform), data )


#welch-anova with pairwise comparison graphs
library(dplyr)
library(ggstatsplot)
library(gridExtra)
library(PMCMRplus)
library(forcats)# to order the graphs by means

data$Platform <- with(data, forcats::fct_reorder(Platform, Bad, .fun = median))
set.seed(4)   # for Bayesian reproducibility of 95% CIs

colnames(data)
p1<-ggbetweenstats(
  data = data %>% dplyr::filter(Platform != "AMZN" & Platform != "Intagram") ,
  x    = Platform, 
  y    = Bad, 
  ylab = "Frequency of ‘bad’ recommendations updated",
  conf.level = 0.95,
  pairwise.comparisons = TRUE,
  pairwise.display = "s",
  type = "parametric",
  var.equal= FALSE,
  bf.message= FALSE,
  ggtheme = ggplot2::theme_grey(),
  ggstatsplot.layer = FALSE)

p2<-ggbetweenstats(
  data = data %>% dplyr::filter(!(Platform %in% c("AMZN", "Intagram"))),
  x    = Platform, 
  y    = Bad, 
  ylab = "Frequency of 'bad' recommendations",
  conf.level = 0.95,
  pairwise.comparisons = TRUE,
  pairwise.display = "s",
  type = "np",
  var.equal= FALSE,
  bf.message = FALSE,
  p.adjust.method = "bonferroni",
  ggtheme = ggplot2::theme_grey(base_size = 15),
  ggstatsplot.layer = FALSE)

save_plot("Platform_np_updated.png", p2, 6, 4.5, "in", 300)
save_plot("Platform_p_updated.png", p1, 6, 4.5, "in", 300)

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
#          BAD~type of harm(Types)                 #
#############################################
ggplot(data, aes(x = Bad)) + geom_histogram() + facet_wrap(~Types)
# Use by() to split the data by platform and apply the Shapiro-Wilk test
tapply(data$Bad, data$Types, function(x) {
  result <- shapiro.test(x)
  paste0("Shapiro-Wilk test: W = ", round(result$statistic, 3), ", p = ", round(result$p.value, 3))
})

# Calculate skewness and kurtosis for each level of 'Types'
skew_kurt <- data %>%
  group_by(Types) %>%
  summarise(
    skewness = skewness(Bad),
    kurtosis = kurtosis(Bad)
  )

# Display the results
print(skew_kurt)
# Check for homogeneity of variances
ggplot(data, aes(x = Types, y = Bad)) +
  geom_boxplot() +
  labs(x = "Types", y = "Bad") +
  ggtitle("Types ~ Bad")

library(car)
leveneTest(Bad ~ factor(Types), data = data %>% 
             mutate(Types = ifelse(
               Types == "kids", "mental health", Types)))
fligner.test(Bad ~ factor(Types), data = data)

# Homogeneity assumption did not match, thus conduting Welch-ANOVA
set.seed(4)   # for Bayesian reproducibility of 95% CIs
data$Types <- with(data, forcats::fct_reorder(Types, Bad, .fun = median))
p1<-ggbetweenstats(
  data = data %>%
    mutate(Types = ifelse(Types == "kids", "mental health", Types)),
  x    = Types, 
  y    = Bad,
  xlab = "Types of harms",
  ylab = "Frequency of 'bad' recommendations",
  conf.level = 0.95,
  pairwise.comparisons = TRUE,
  type = "parametric",
  nboot = 10,
  pairwise.display = "all",
  bf.message = FALSE,
  ggtheme = ggplot2::theme_grey(),
  ggstatsplot.layer = FALSE)

p2<-ggbetweenstats(
  data = data %>%
    mutate(Types = ifelse(Types == "kids", "mental health", Types)) %>%
    mutate(Types = fct_reorder(Types, Bad, .fun = median)),
  x = Types,
  y = Bad,
  xlab = "Types of harms",
  ylab = "Frequency of 'bad' recommendations",
  conf.level = 0.95,
  pairwise.comparisons = TRUE,
  type = "nonparametric",
  pairwise.display = "s",
  p.adjust.method = "bonferroni",
  nboot = 10,
  var.equal = TRUE,
  ggtheme = ggplot2::theme_grey(base_size = 15),
  centrality.label.args = list(size  = 3),
  ggstatsplot.layer = FALSE
)

save_plot("Types_np_updated.png", p2, 10, 6, "in", 300)
save_plot("Types_p.png", p1, 10, 6, "in", 300)

grid.arrange(p1,p2)


#some may argue that non-parameteric pairwise comparisons test such as Conovertest is more suitable in this case
conover_test <- conover.test(data$Bad, data$Types, method = "bonferroni")

#############################################
#         Bad~ modality.                    #
#############################################
ggplot(data, aes(x = Bad)) + geom_histogram() + facet_wrap(~Modalities)
# Use by() to split the data by platform and apply the Shapiro-Wilk test
tapply(data$Bad, data$Modalities, function(x) {
  result <- shapiro.test(x)
  paste0("Shapiro-Wilk test: W = ", round(result$statistic, 3), ", p = ", round(result$p.value, 3))
})
# Check for homogeneity of variances
ggplot(data, aes(x = Modalities, y = Bad)) +
  geom_boxplot() +
  labs(x = "Modality", y = "Bad") +
  ggtitle("Bad ~ Modality")

library(car)
leveneTest(Bad ~ factor(Modalities), data = data)
fligner.test(Bad ~ factor(Modalities), data = data)  #assumption has not violated

oneway.test(Bad ~ Modalities, data = data, var.equal = TRUE)

data$Modalities <- with(data, forcats::fct_reorder(Modalities, Bad, .fun = median))
p1<- ggbetweenstats(
  data = data,
  x    = Modalities, 
  y    = Bad, 
  ylab = "Frequency of 'bad' recommendations",
  type = "p",
  pairwise.comparisons = T,
  pairwise.display = "s",
  ggtheme = ggplot2::theme_grey(),
  ggstatsplot.layer = FALSE)

library(stringr)
data$Modalities <- str_wrap(data$Modalities, width = 10) 
p2<- ggbetweenstats(
  data = data,
  x    = Modalities, 
  y    = Bad, 
  ylab = "Frequency of 'bad' recommendations",
  type = "np",
  pairwise.comparisons = T,
  pairwise.display = "s",
  p.adjust.method = "bonferroni",
  ggtheme = ggplot2::theme_grey(base_size = 15),
  ggstatsplot.layer = FALSE)

save_plot("modality_np_updated.png", p2, 11, 4.5, "in", 300)
save_plot("modality_p.png", p1, 11, 4.5, "in", 300)
grid.arrange(p1,p2)
conover_test <- conover.test(data$Bad, data$Modalities, method = "bonferroni")

#############################################
#         Bad~ SeedInput.                      #
#############################################

ggplot(data, aes(x = Bad)) + geom_histogram() + facet_wrap(~SeedInput)
# Use by() to split the data by platform and apply the Shapiro-Wilk test
tapply(data$Bad, data$SeedInput, function(x) {
  result <- shapiro.test(x)
  paste0("Shapiro-Wilk test: W = ", round(result$statistic, 3), ", p = ", round(result$p.value, 3))
})
# Check for homogeneity of variances
ggplot(data, aes(x = SeedInput, y = Bad)) +
  geom_boxplot() +
  labs(x = "SeedInput", y = "Bad") +
  ggtitle("Bad ~ SeedInput")

library(car)
leveneTest(Bad ~ factor(SeedInput), data = data)
fligner.test(Bad ~ factor(SeedInput), data = data)  #assumption has been violated

oneway.test(Bad ~ SeedInput, data = data, var.equal = FALSE)

data$SeedInput <- with(data, forcats::fct_reorder(SeedInput, Bad, .fun = median))
p1<- ggbetweenstats(
  data = data %>%
    filter(!(SeedInput == "good" & Bad > 0.3)),
  x    = SeedInput, 
  y    = Bad, 
  xlab = "Source input",
  ylab = "Frequency of 'bad' recommendations",
  type = "p",
  title = "Bad ~ SeedInput",
  subtitle = "parameteric",
  pairwise.comparisons = T,
  pairwise.display = "all",
  var.equal= FALSE,
  ggtheme = ggplot2::theme_grey(),
  ggstatsplot.layer = FALSE)

p2<- ggbetweenstats(
  data = data %>%
    filter(!(SeedInput == "good" & Bad > 0.3)),
  x    = SeedInput, 
  y    = Bad,
  xlab = "Source input",
  ylab = "Frequency of 'bad' recommendations", 
  type = "np",
  subtitle = "parameteric",
  pairwise.comparisons = T,
  pairwise.display = "s",
  var.equal= FALSE,
  p.adjust.method = "bonferroni",
  ggtheme = ggplot2::theme_grey(base_size = 15),
  ggstatsplot.layer = FALSE,
  p.value.label = function(x) round(x, digits = 3))

save_plot("badSeedInput_np__with_outliers_updated.png", p2, 6, 4.5, "in", 300)
save_plot("badSeedInput_p.png", p1, 6, 4.5, "in", 300)
grid.arrange(p1,p2)
conover_test <- conover.test(data$Bad, data$SeedInput, method = "bonferroni")

# Result: insignificant for both parametric and nonparametric

#############################################
#         Good~ SeedInput                      #
#############################################

ggplot(data, aes(x = GOOD)) + geom_histogram() + facet_wrap(~SeedInput)
# Use by() to split the data by platform and apply the Shapiro-Wilk test
tapply(data$GOOD, data$SeedInput, function(x) {
  result <- shapiro.test(x)
  paste0("Shapiro-Wilk test: W = ", round(result$statistic, 3), ", p = ", round(result$p.value, 3))
})
# Check for homogeneity of variances
ggplot(data, aes(x = SeedInput, y = GOOD)) +
  geom_boxplot() +
  labs(x = "SeedInput", y = "GOOD") +
  ggtitle("GOOD ~ SeedInput")

library(car)
leveneTest(GOOD ~ factor(SeedInput), data = data)
fligner.test(GOOD ~ factor(SeedInput), data = data)  #assumption has been violated

oneway.test(GOOD ~ SeedInput, data = data, var.equal = FALSE)

data$SeedInput <- with(data, forcats::fct_reorder(SeedInput, GOOD, .fun = median))
p1<- ggbetweenstats(
  data = data,
  x    = SeedInput, 
  y    = GOOD,
  xlab = "`Source input",
  ylab = "Frequency of 'Good' recommendations", 
  type = "p",
  bf.message = FALSE,
  pairwise.comparisons = T,
  pairwise.display = "s",
  ggtheme = ggplot2::theme_grey(base_size = 11),
  ggstatsplot.layer = FALSE)

p2<- ggbetweenstats(
  data = data ,
  x    = SeedInput, 
  y    = GOOD, 
  xlab = "Source input",
  ylab = "Frequency of 'Good' recommendations",
  type = "np",
  pairwise.comparisons = T,
  pairwise.display = "s",
  var.equal= TRUE,
  p.adjust.method = "bonferroni",
  ggtheme = ggplot2::theme_grey(base_size = 15),
  ggstatsplot.layer = FALSE)

grid.arrange(p1,p2)

save_plot("goodSeedInput_np_updated.png", p2, 6, 4.5, "in", 300)
save_plot("goodSeedInput_p.png", p1, 6, 4.5, "in", 300)
conover_test <- conover.test(data$GOOD, data$SeedInput, method = "bonferroni")

# Result: significant for both parametric and nonparametric



