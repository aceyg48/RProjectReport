library(caret) # Train-Test Split
library(class) # Home of K-Nearest Neighbors and K-Means!!
library(corrplot) # Visualize Correlation Matrix
library(ggplot2) # Graphing

library(readr)
academic_user <- read_csv("ThisPCSchool/DSC-140/R_Folder/OA 11.6 - yelp_academic_dataset_user.json.csv")
academic_business <- read_csv("ThisPCSchool/DSC-140/R_Folder/OA 11.7 - yelp_academic_dataset_business.json.csv")
View(academic_user)
View(academic_business)

# Business
# Print the first 5 rows of the data set
print(head(academic_business, 5))

# Create a histogram or bar chart (decide which one makes the most sense) of 
# the “state” column to get an idea of where the business are located.
ggplot(academic_business) +
  geom_bar(aes(x = state), fill = "red", ) +
  labs(x = "States", y = "Count", title = "Amount of Each State")

# Generate a pie chart of the star ratings of all of the businesses.
cont_table <- table(academic_business$stars)
pie(cont_table,
    main = "Pie Chart of Review Stars",
    col = rainbow(9))

# Explore the relationship between review count and stars by making a 
# box plot of the reviews for each star rating.
academic_business$stars<-as.factor(academic_business$stars)
ggplot(academic_business,
       aes(x = stars, y = review_count, fill = stars)) +
  geom_boxplot(show.legend = FALSE)

# I'm getting rid of MOST outliers
good_review <- subset(academic_business, review_count < 70)
ggplot(good_review,
       aes(x = stars, y = review_count, fill = stars)) +
  geom_boxplot(show.legend = FALSE)

# Perform a chi-squared test between stars = 1.0 and the starts = 5.0 data
# What does the result of the test tell you? 
star_1 <- subset(academic_business, stars == 1)
star_5 <- subset(academic_business, stars == 5)
View(star_1)
View(star_5)

business_cont_table <- table(star_1$state, star_5$review_count[1:911])
chisq.test(business_cont_table)
# states in star 1 are independent from review count in 5 stars
business_cont_table <- table(star_1$open, star_5$review_count[1:911])
chisq.test(business_cont_table)
# star one open is independent from star 5 review count
business_cont_table <- table(star_1$city, star_5$state[1:911])
chisq.test(business_cont_table)
# star one cities and star 5 states are dependent on each other

# User Data
print(colnames(academic_user))
View(academic_user)
# Doing the correlation of the three values
corr_user <- cor(academic_user$cool_votes, academic_user$funny_votes)
print(corr_user)
corr_user <- cor(academic_user$useful_votes, academic_user$funny_votes)
print(corr_user)
corr_user <- cor(academic_user$cool_votes, academic_user$useful_votes)
print(corr_user)
# They are all very correlated, positively :)

# Visualizing the linear regression model
ggplot(academic_user) +
  geom_point(aes(x = cool_votes,
                 y = funny_votes),
             color = "forestgreen",
             size = 2,
             shape = "triangle") +
  labs(x = "Cool Votes",
       y = "Funny Votes",
       title = "Cool Votes vs. Funny Votes")

# Finding the slope and y-int of line of best fit
linear_model <- lm(academic_user$cool_votes ~ academic_user$funny_votes)

coefs <- coef(linear_model)
lm_y_intercept <- coefs[1]
lm_slope <- coefs[2]
cat("Slope:", lm_slope, "Y-Intercept:", lm_y_intercept)

# Plot line of best fit
ggplot(academic_user) +
  geom_point(aes(x = cool_votes,
                 y = funny_votes),
                color = "forestgreen",
                size = 2,
                shape = "triangle") +
  geom_smooth(aes(x = cool_votes, y = funny_votes), method = "lm", se = F) +
  labs(x = "Cool Votes",
       y = "Funny Votes",
       title = "Cool Votes vs. Funny Votes")

# Doing it again but for reviews and fans
linear_model <- lm(academic_user$review_count ~ academic_user$fans)

coefs <- coef(linear_model)
lm_y_intercept <- coefs[1]
lm_slope <- coefs[2]
cat("Slope:", lm_slope, "Y-Intercept:", lm_y_intercept)

# Plot line of best fit
ggplot(academic_user) +
  geom_point(aes(x = review_count,
                 y = fans),
             color = "royalblue",
             size = 2,
             shape = "triangle") +
  geom_smooth(aes(x = review_count, y = fans), method = "lm", se = F, color = "red") +
  labs(x = "Review Count",
       y = "Amount of Fans",
       title = "Review Count vs. Fan amount")
corr_user <- cor(academic_user$review_count, academic_user$fans)
print(corr_user)
# Not very correlated r = 0.5849059
# Writing the reviews does not bring in more fans because the two are not
# correlated due to the r equaling 0.5849059

# Trying to see if useful_votes bring in fans
ggplot(academic_user) +
  geom_point(aes(x = useful_votes,
                 y = fans),
             color = "royalblue",
             size = 2,
             shape = "triangle") +
  geom_smooth(aes(x = useful_votes, y = fans), method = "lm", se = F, color = "red") +
  labs(x = "Useful Votes",
       y = "Fan Amount",
       title = "Useful Votes vs. Fan Amount")

linear_model <- lm(academic_user$useful_votes ~ academic_user$fans)

coefs <- coef(linear_model)
lm_y_intercept <- coefs[1]
lm_slope <- coefs[2]
cat("Slope:", lm_slope, "Y-Intercept:", lm_y_intercept)

corr_user <- cor(academic_user$useful_votes, academic_user$fans)
print(corr_user)
# Useful Votes has a greater r correlation than before so it is more correlated
# than review_count vs fans but this corr coe is 0.7899782 which is greater than
# before

# K-Means TIME
academic_cluster <- kmeans(academic_user[c(3,11)], 4)
# I started with 2 Clusters
# but according to the elbow plot, find below, it shows that 4 is actually the best
cont_table <- table(academic_cluster$cluster)
academic_user$cluster <- academic_cluster$cluster
View(cont_table)
print(cont_table)
View(academic_user)
# The k-means algorithm splits the columns chosen into the number of clusters, 
# groups, we want so column 3 and 11 being review_count and fans respectively.

# The Graph of the data
ggplot(academic_user) +
  geom_point(aes(x = review_count, y = fans, color = academic_user$cluster)) +
  labs(x = "Review Count",
       y = "Fans",
       title = "Review Count vs. Fans")

# The Elbow Plot
user_x <- academic_user[c(3,11)]
wcss <- function(k){
  kmeans(user_x, centers = k)$tot.withinss
}
k_values <- 1:10
wcss_values <- sapply(k_values, wcss)
elbow_plot <- data.frame(k = k_values, wcss = wcss_values)
View(elbow_plot)
ggplot(elbow_plot, aes(x = k, y = wcss)) +
  geom_line() + geom_point()


# Doing it all over again for different columns, cool votes and funny votes
academic_cluster <- kmeans(academic_user[c(5,6)], 4)
academic_user$cluster <- academic_cluster$cluster
# I started with 2 Clusters
# but according to the elbow plot, find below, it also shows that 4 is actually the best
cont_table <- table(academic_cluster$cluster)
View(cont_table)
print(cont_table)
View(academic_user)
# The k-means algorithm splits the columns chosen into the number of clusters, 
# groups, we want so column 5 and 6 being cool_votes and funny_votes respectively.

# The plot
ggplot(academic_user) +
  geom_point(aes(x = cool_votes, y = funny_votes, color = academic_user$cluster)) +
  labs(x = "Cool Votes",
       y = "Funny Votes",
       title = "Cool Vs. Funny Votes and Kmeans")

# The Elbow Plot
user_x <- academic_user[c(5,6)]
wcss <- function(k){
  kmeans(user_x, centers = k)$tot.withinss
}
k_values <- 1:10
wcss_values <- sapply(k_values, wcss)
elbow_plot <- data.frame(k = k_values, wcss = wcss_values)
View(elbow_plot)
ggplot(elbow_plot, aes(x = k, y = wcss)) +
  geom_line() + geom_point()