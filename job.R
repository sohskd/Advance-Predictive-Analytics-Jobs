rm(list=ls())

### Understand the Data ###
jobs_data <- read.csv("E:\\Des UNI\\BC2407\\BC2407\\SAS\\TeamProject\\R code\\generic_jobsdata_v2.csv");
View(jobs_data)
head(jobs_data$presumed_salary)

drops <- c("matched_skills_score", 
           "schemes",
           "is_hide_employer_name",
           "district_ids",
           "company_name_score",
           "job_title_score",
           "min_years_experience",
           "is_hide_salary",
           "exact_company_score",
           "logo_url",
           "exact_string_score",
           "is_posted_on_behalf",
           "categories",
           "hidden_name",
           "job_post_id",
           "hiring_employer_name",
           "job_description_score",
           "X")

jobs_it_new <- jobs_data[, !names(jobs_data) %in% drops];
View(jobs_it_new)

### Create New Columns ###

# Create date elasped (created_on minus date that data was crawled)
date_crawled <- as.Date('2018-3-2')
jobs_it_new$day_elasped <- as.double(date_crawled - as.Date(jobs_it_new$created_on))
View(jobs_it_new)

# Create Popularity Rate Ratio (Number of Application / Day Elasped)
jobs_it_new$popularity_rate <- jobs_it_new$total_number_job_application / jobs_it_new$day_elasped
summary(jobs_it_new$popularity_rate)

# Create Conversion Rate Ratio (Number of Application / Number of Views)
jobs_it_new$conversion_rate <- jobs_it_new$total_number_job_application / jobs_it_new$total_number_of_view
summary(jobs_it_new$total_number_job_application)
summary(jobs_it_new$total_number_of_view)
summary(jobs_it_new$conversion_rate)

# Total number of view == 0. Total number of application may not == 0. Weird?
nrow(jobs_it_new[jobs_it_new$conversion_rate == "Inf",])
# Total number of view == 0. Total number of application may not == 0.
nrow(jobs_it_new[jobs_it_new$conversion_rate == "NaN",])

### Cleaning ###

# Remove obs with conversion_rate == NaN or Inf
jobs_it_new <- jobs_it_new[!jobs_it_new$conversion_rate == "NaN", ]
jobs_it_new <- jobs_it_new[!jobs_it_new$conversion_rate == "Inf", ]
nrow(jobs_it_new)

# Set dummy variables for category 
vals <- get_unique_column_names(jobs_it_new$category)

print_count_category(vals)

dummy_table <- get_dummy_variables(jobs_it_new, vals)
jobs_it_new <- cbind(jobs_it_new, dummy_table)
View(jobs_it_new)

nrow(jobs_it_new)

data <- set_dummy_variables(jobs_it_new, vals)
summary(data)
View(data)

# Drop more columns
colnames(data)

drops <- c("new_posting_date",
           "uuid",
           "employer_name", 
           "original_posting_date",
           "expiry_date",
           "min_monthly_salary",
           "max_monthly_salary",
           "job_title",
           "created_on",
           "modified_on",
           "company_description",
           "is_hide_hiring_employer_name")

data <- data[, !names(data) %in% drops];
View(data)

# Models

colnames(data)
fit <- lm(data$popularity_rate ~ data$presumed_salary , data=data)
summary(fit)

fit1 <- lm(data$popularity_rate ~ ., data=data[ , !(names(data) %in% c('employment_types','position_levels',
                                                                       'job_description', 'salary_type',
                                                                       'category', 'skills',
                                                                       'Banking and Finance'))])
summary(fit1)

print_count_category <- function(vals) {
  
  mylist <- list()
  for (i in vals) {
    mylist[[ i ]] <- sum(grepl(i, jobs_it_new$category))
    # cat(i, ":", sum(grepl(i, jobs_it_new$category)), "\n")
  }
  mylist[order(-unlist(mylist))]
}

set_dummy_variables <- function(data, vals) {
  
  # Loop through the unique column category
  # for (i in vals) {
  #   # Loop through the whole vector for that column
  #   for (j in 1:length(data[[i]])) {
  #     # If column name is in data category, then set that observation's column name to 1
  #     if (grepl(i, data$category[j])) {
  #       data[[i]][j] <- 1
  #     }
  #   }
  # }
  
  check <- 0
  # Loop through individual observations
  for (j in 1:nrow(data)) {
    # Loop through the column category
    for (i in vals) {
        # If column name is in data category, then set that observation's column name to 1
        if (grepl(i, data$category[j])) {
          data[[i]][j] <- 1
          check <- 1
        }
    }
    if (check == 0) {
      data$Others[j] <- 1
    }
    check <- 0
  }
  return(data)
}

get_dummy_variables <- function(data, vals) {
  
  table <- matrix(0, ncol = length(vals), nrow = nrow(data))
  table <- data.frame(table)
  colnames(table) <- vals
  return(table)
}

get_unique_column_names <- function(column_name, threshold=400) {
  
  unique_cats <- unique(column_name)
  as.factor(unique_cats)
  vals <- c()
  for (i in unique_cats) {
    # Split categories delimited by /
    cats <- unlist(strsplit(i, "/"))
    cats <- trimws(cats, which = c("both", "left", "right"))
    
    if(length(cats) > 1) {
      for (k in cats) {
        if(sum(grepl(k, column_name)) > threshold) {
          vals <- append(vals, k)
        }
      }
    } else {
      if(sum(grepl(cats, column_name)) > threshold) {
        vals <- append(vals, cats)
      }
    }
  }
  return(vals)
}


