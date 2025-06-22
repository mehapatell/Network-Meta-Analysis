#Import and install necessary packages
install.packages("readxl")
install.packages("netmeta")
install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(netmeta)
library(dplyr)
# Set a consistent theme for all plots for reporting purposes
library(ggplot2)
theme_set(theme_minimal())

#import dataset from excel file in local directory
my_data <- read_excel("/Users/admin/Downloads/Chronic_Migraine_Dataset.xlsx")


# Display the first few rows of the dataset
head(my_data)


# Check the structure of the dataset
str(my_data)

# convert se to numeric since it represents standard error
my_data$se <- as.numeric(my_data$se)

# Check for missing values in the columns of interest
missing_values <- my_data %>%
  summarise(across(c(y, se, trt, study), ~ sum(is.na(.))))
print(missing_values)
# Filter out rows with NA values in the columns of interest
my_data <- my_data %>%
  filter(!is.na(y), !is.na(se), !is.na(trt), !is.na(study))

#check
print(my_data)

#subset cript study from full dataset to run pairwise analysis since it has 3 arms
crisp_data <- my_data %>% filter(study == "Crisp")

print(crisp_data)
#what if I append a new row to the crisp_data to represent the placebo group?
crisp_long <- data.frame(
  study = rep("Crisp", 3),
  trt = c("Galcanezumab", "Eptinezumab", "Placebo"),
  y = c(-4.2, -4.9, 0),
  se = c(0.4758, 0.5669, 0.5) #estimate se value for interaction effect
  #n = c(100, 100, 100)  # Assuming equal sample sizes for simplicity
)

print(crisp_long1)
placebo_row <- data.frame(
  study = "Crisp",
  trt = "Placebo",
  y = 0,         # Placebo is the reference, so y = 0
  se = 0,       # se is usually NA for reference arm
  n = 100        # or the actual sample size for Placebo
)

# Append the new row to crisp_data
crisp_long <- bind_rows(crisp_data, placebo_row)

#check
print(crisp_long)

#run pairwise function from netmeta package on crisp study
pw_crisp <- pairwise(
  treat = trt,
  TE = y,
  seTE = se,
  studlab = study,
  data = crisp_long,
  sm = "MD"
)
#check
print(pw_crisp)

#subset original full dataset into contrast dataset by removing placebo rows and crisp study, will append crisp pairwise later
contrast_data <- my_data %>%
  filter(trt != "Placebo", study != "Crisp") %>%
  mutate(
    trt1 = trt,
    trt2 = "Placebo"
  ) %>%
  select(study, trt1, trt2, y, se, na)

#check
print(contrast_data)
#check for any NA entries
sum(is.na(contrast_data))


# Combine the two data frames, adding crisp pairwise to contrast dataset
final_contrast_data <- bind_rows(
  contrast_data,
  pw_crisp %>% select(study = studlab, trt1 = treat1, trt2 = treat2, y = TE, se = seTE)
)
# Drop any columns not needed for nma
final_contrast_data <- final_contrast_data %>%
  select(study, trt1, trt2, y, se)  

#check
print(final_contrast_data)

# Check for NA or Inf in your columns
summary(final_contrast_data)
str(final_contrast_data)
sum(final_contrast_data$se == 0)
final_contrast_data <- final_contrast_data %>% filter(se > 0)
sapply(final_contrast_data, function(x) sum(!is.finite(x)))  # Should now return 0s for y and se, nothing for character columns
netconnection(final_contrast_data$trt1, final_contrast_data$trt2)
any(duplicated(final_contrast_data[, c("study", "trt1", "trt2")]))

sum(final_contrast_data$se <= 0)

summary(final_contrast_data)
sapply(final_contrast_data, function(x) sum(!is.finite(x)))  # Should all be 0

test_data <- final_contrast_data[1:11, ]
test_data_1 <- final_contrast_data[11:13, ]


# Run network meta-analysis
nma_object <- netmeta(
  TE = y,   # Treatment Effect
  seTE = se, #Standard Error
  treat1 = trt1,
  treat2 = trt2,
  studlab = study,
  data = final_contrast_data,
  sm = "MD", # Standardized Mean Difference
  details.chkmultiarm = TRUE
)   

# View the summary of results
summary(nma_object)

# Key comparisons

# Treatment effects table
nma_object$TE.random  # Mean differences
nma_object$lower.random  # Lower CI bounds  
nma_object$upper.random  # Upper CI bounds


netgraph(nma_object)
ggsave("fig_graph_plot.pdf")
netrank(nma_object)

plot(netrank(nma_object))
ggsave("fig_rank_plot.pdf")
# Create a forest plot for the network meta-analysis
forest(nma_object, reference.group = "Galcanezumab")
# Save forest plot
forest(nma_object, reference.group = "Galcanezumab")
ggsave("fig_forest_plot.pdf")
netleague(nma_object)

decomp.design(nma_object)

netcontrib(nma_object)

netsplit(nma_object)
ggsave("fig_split_plot.pdf")