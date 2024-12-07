# 1. Loading and displaying the data

df <- read.csv('pierce_co_house_sales.csv')
head(df)

# 2. Separating houses with sale_price under/over 1,000,000

under_mil <- df[df$sale_price < 1000000,]
over_mil <- df[df$sale_price >= 1000000,]

head(under_mil)
head(over_mil)

nrow(over_mil)
nrow(under_mil)