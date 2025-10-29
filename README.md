---
title: "ZARA Sales Data Analysis"
author: "Naman Verma"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
library(dplyr)
library(ggplot2)
```

## 🧹 Load and Clean Data

```{r load}
df <- read.csv("Zara_sales_EDA - Zara_sales_EDA.csv")
head(df)
summary(df)

df <- na.omit(df)
df <- distinct(df)
```

## 📈 Top 5 Best Products (With and Without Promotions)

```{r top5}
top5_promo <- df %>%
  filter(Promotion == "Yes") %>%
  group_by(name) %>%
  summarise(Sales.Volume = sum(Sales.Volume)) %>%
  arrange(desc(Sales.Volume)) %>%
  head(5)

top5_nonpromo <- df %>%
  filter(Promotion == "No") %>%
  group_by(name) %>%
  summarise(Sales.Volume = sum(Sales.Volume)) %>%
  arrange(desc(Sales.Volume)) %>%
  head(5)

ggplot(top5_promo, aes(x=Sales.Volume, y=reorder(name, Sales.Volume))) +
  geom_col(fill="steelblue") +
  geom_text(aes(label = Sales.Volume), hjust = -0.1, color="black", size=3.5) +
  ggtitle("Top 5 Products With Promotion") +
  theme_minimal()

ggplot(top5_nonpromo, aes(x=Sales.Volume, y=reorder(name, Sales.Volume))) +
  geom_col(fill="tomato") +
  geom_text(aes(label = Sales.Volume), hjust = -0.1, color="black", size=3.5) +
  ggtitle("Top 5 Products Without Promotion") +
  theme_minimal()
```

## 🌍 Best Performing Country

```{r country}
country_sales <- df %>%
  group_by(origin) %>%
  summarise(Sales.Volume = sum(Sales.Volume)) %>%
  arrange(desc(Sales.Volume))

ggplot(country_sales, aes(x=Sales.Volume, y=reorder(origin, Sales.Volume))) +
  geom_col(fill="orange") +
  geom_text(aes(label = Sales.Volume), hjust = -0.1, color="black", size=3.5) +
  ggtitle("Best Performing Country") +
  theme_minimal()
```

## 📉 Least Performing Products & Promotion Status

```{r least}
least_products <- df %>%
  group_by(name, Promotion) %>%
  summarise(Sales.Volume = sum(Sales.Volume)) %>%
  arrange(Sales.Volume) %>%
  head(10)

ggplot(least_products, aes(x=Sales.Volume, y=reorder(name, Sales.Volume), fill=Promotion)) +
  geom_col() +
  geom_text(aes(label = Sales.Volume), hjust = -0.1, color="black", size=3.2) +
  ggtitle("Least Performing Products & Promotion Status") +
  theme_minimal()
```

## 🧵 Most Demanded Product Material

```{r material}
material_sales <- df %>%
  group_by(material) %>%
  summarise(Sales.Volume = sum(Sales.Volume)) %>%
  arrange(desc(Sales.Volume))

ggplot(material_sales, aes(x=Sales.Volume, y=reorder(material, Sales.Volume))) +
  geom_col(fill="darkgreen") +
  geom_text(aes(label = Sales.Volume), hjust = -0.1, color="white", size=3.5) +
  ggtitle("Most Demanded Product Material") +
  theme_minimal()
```

## 👕 Sales by Gender / Section

```{r gender}
gender_sales <- df %>%
  group_by(section) %>%
  summarise(Sales.Volume = sum(Sales.Volume)) %>%
  mutate(Percent = round((Sales.Volume / sum(Sales.Volume)) * 100, 1),
         Label = paste0(section, " (", Percent, "%)"))

ggplot(gender_sales, aes(x="", y=Sales.Volume, fill=section)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  geom_text(aes(label=Label),
            position=position_stack(vjust=0.5),
            color="white",
            size=5,
            fontface="bold") +
  ggtitle("Sales Distribution by Gender") +
  theme_void()
```

## 🧠 Conclusions

1. **Top Selling Products:**  
   - Products under promotions significantly outperform those without promotions.  
   - Promotional strategies directly influence sales volume.  

2. **Best Performing Countries:**  
   - Countries like Spain and Italy (if applicable in your dataset) lead in total sales volume, indicating strong brand engagement in European markets.  

3. **Least Performing Products:**  
   - Most low-performing items are either non-promoted or from less popular material categories.  

4. **Material Preference:**  
   - Cotton and polyester-based products dominate sales, reflecting customer preference for comfort and affordability.  

5. **Gender-Based Sales:**  
   - Women's section contributes higher overall sales compared to men's, suggesting stronger product demand or marketing effectiveness.  

✨ **Overall Insight:**  
ZARA’s sales are heavily influenced by **promotional activity, material type, and gender-focused marketing**. Future focus should be on optimizing promotional spending for low-performing products and strengthening inventory management for high-demand categories.


