library(tidyverse)
library(mosaic)
library(moderndive)
library(effectsize)

solder <- read.csv("solder.csv")
groceries <- read.csv("groceries.csv")

ggplot(solder, aes(x=Opening, y=skips)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Opening Size") + ylab("Skips") + ggtitle("Opening Size vs Number of Skips") + theme(plot.title = element_text(hjust = 0.5))

ggplot(solder, aes(x=Solder, y=skips)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Thickness") + ylab("Skips") + ggtitle("Thickness of Alloy vs Number of Skips") + theme(plot.title = element_text(hjust = 0.5))

solder_model <- lm(skips ~ Opening + Solder + Opening:Solder, data = solder)
solder_model
get_regression_table(solder_model, conf.level = 0.95, digits=2)

store_price <- groceries %>%
  group_by(Store) %>%
  summarise(avg_price = mean(Price))

ggplot(store_price, aes(x = avg_price, y = Store)) +
  geom_bar(stat = "identity") +
  labs(title = "Store vs. Average Price", x = "Average Price", y = "Store") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

product_numberOfStores <- groceries %>%
  group_by(Product) %>%
  summarise(number_of_stores = length(Store))

ggplot(product_numberOfStores, aes(x = number_of_stores, y = Product)) +
  geom_bar(stat = "identity") +
  labs(title = "Product vs Number of Stores Selling the Product", x = "Number of Stores", y = "Product") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

lm_price <- lm(Price ~ Product + Type, data = groceries)

lm_price_table <- get_regression_table(lm_price, conf.level = 0.95, digits=2)

lm(Price ~ Product + Store, data = groceries)


income_groceries <- groceries %>%
  mutate(Income10k = Income/10000)

lm_income10k <- lm(Price ~ Product + Income10k, data = income_groceries)
standardize_parameters(lm_income10k)
