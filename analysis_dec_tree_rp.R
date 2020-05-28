---
title: "analysis_dec_tree_rp"
output:
  html_document:
    df_print: paged
df_print: paged
---

```{r}
#SCC461 Analysis
library(tidyverse)
library(corrplot)
library(effects)
```

```{r}
df_by_hand <- read_csv("df_by_hand.csv")
df_scikit <- read_csv("df_scikit.csv")
df_depth_focus <- read_csv("df_depth_focus.csv")
df_sci_depth_focus <- read_csv("df_sci_depth_focus.csv")

df_by_hand$dataset <- as.factor(df_by_hand$dataset)
df_scikit$dataset <- as.factor(df_scikit$dataset)

df_by_hand$depth <- as.factor(df_by_hand$depth)
df_scikit$depth <- as.factor(df_scikit$depth)
```

Quick summary stats. The scikit version has performed very marginally better. The difference in median AUC score is very small, only 0.0011 for instance. The run times are larger for the from scratch version also.
```{r}
summary(df_scikit)
summary(df_by_hand)
```
EDA
```{R}
ggplot(df_scikit) +
  geom_boxplot(aes(x=dataset, y=df_scikit$AUC ,fill=dataset)) +
  scale_fill_brewer(palette="Set1") +
  labs(title = "AUC scores for different datasets, Scikit Decision Tree",
       x = "Depth", y = "AUC", color = "Legend\n")

ggplot(df_by_hand) +
  geom_boxplot(aes(x=dataset, y=df_by_hand$AUC,fill=dataset)) +
  scale_fill_brewer(palette="Set1") +
  labs(title = "AUC scores for different datasets, From Scratch Decision Tree ",
       x = "Depth", y = "AUC", color = "Legend\n")

```

```{r}
ggplot(df_scikit) +
  geom_boxplot(aes(x=depth, y=AUC,fill=depth)) +
  scale_fill_brewer(palette="Set2") +
  labs(title = "AUC scores for different depths, Scikit Decision Tree",
       x = "Depth", y = "AUC", color = "Legend\n")

ggplot(df_by_hand) +
  geom_boxplot(aes(x=depth, y=AUC,fill=depth)) +
  scale_fill_brewer(palette="Set2") +
  labs(title = "AUC scores for different depths, From Scratch Decision Tree ",
       x = "Depth", y = "AUC", color = "Legend\n")
```



```{r}
ggplot(df_scikit) + 
geom_jitter(aes(x=`run time(s)`,y=AUC,col=dataset)) + 
scale_fill_brewer(palette="Set1") +
labs(title = "Run Time Against Final AUC, Scikit",
x = "Run time (seconds)", y = "Final Averaged AUC Score", color = "Legend\n")

ggplot(df_by_hand) + 
geom_jitter(aes(x=`run time(s)`,y=AUC,col=dataset)) + 
scale_fill_brewer(palette="Set1") +
labs(title = "Run Time Against Final AUC, From Scratch",
x = "Run time (seconds)", y = "Final Averaged AUC Score", color = "Legend\n")
```

```{r}
ggplot(df_sci_depth_focus) + 
geom_jitter(aes(x=depth,y=AUC,col=dataset)) + 
scale_fill_brewer(palette="Set1") +
labs(title = "Depth Against Final AUC, Scikit",
x = "Depth", y = "Final Averaged AUC Score", color = "Legend\n")

ggplot(df_depth_focus) + 
geom_jitter(aes(x=depth,y=AUC,col=dataset)) + 
scale_fill_brewer(palette="Set1") +
labs(title = "Depth Against Final AUC, From Scratch",
x = "Depth", y = "Final Averaged AUC Score", color = "Legend\n")

ggplot(df_sci_depth_focus) + 
geom_jitter(aes(x=depth,y=runtime,col=dataset)) + 
scale_fill_brewer(palette="Set1") +
labs(title = "Depth Against Runtime, Scikit",
x = "Depth", y = "Runtime (seconds)", color = "Legend\n")

ggplot(df_depth_focus) + 
geom_jitter(aes(x=depth,y=runtime,col=dataset)) + 
scale_fill_brewer(palette="Set1") +
labs(title = "Depth Against Runtime, From Scratch",
x = "Depth", y = "Runtime (seconds)", color = "Legend\n")
```



```{r}
df_sci_copy <- data.frame(df_scikit)
df_sci_copy$dataset <- as.numeric(df_sci_copy$dataset)
df_sci_copy$depth <- as.numeric(df_sci_copy$depth)
df_sci_mat <- cor(df_sci_copy)
corrplot(df_sci_mat,order="hclust",fig='Figure 1')

df_scratch_copy <- data.frame(df_by_hand)
df_scratch_copy$dataset <- as.numeric(df_scratch_copy$dataset)
df_scratch_copy$depth <- as.numeric(df_scratch_copy$depth)
df_scratch_mat <- cor(df_scratch_copy)
corrplot(df_scratch_mat,order="hclust",fig='Figure 2')
```

```{r}
plot(density(df_scikit$AUC))
plot(density(df_by_hand$AUC))

plot(density(df_scikit$`run time(s)`))
plot(density(df_by_hand$`run time(s)`))

qqnorm(df_scikit$AUC)
qqline(df_scikit$AUC)

qqnorm(df_by_hand$AUC)
qqline(df_by_hand$AUC)
```

```{r}
#using dplyr filter function to filter the datasets, then checking the mean AUC scores for each set
df_wine_sci <- filter(df_scikit,df_scikit$dataset == 'wine')
df_iris_sci <- filter(df_scikit,df_scikit$dataset == 'iris')
df_cancer_sci <- filter(df_scikit,df_scikit$dataset == 'cancer')

df_wine_scratch <- filter(df_by_hand,df_by_hand$dataset == 'wine')
df_iris_scratch <- filter(df_by_hand,df_by_hand$dataset == 'iris')
df_cancer_scratch <- filter(df_by_hand,df_by_hand$dataset == 'cancer')

mean(df_wine_sci$AUC)
mean(df_iris_sci$AUC)
mean(df_cancer_sci$AUC)

mean(df_wine_scratch$AUC)
mean(df_iris_scratch$AUC)
mean(df_cancer_scratch$AUC)
```

```{r}
mean(df_scikit$AUC)
mean(df_by_hand$AUC)
#significant difference in AUC?
t.test(df_scikit$AUC,df_by_hand$AUC)

#significant difference in run time?
t.test(df_scikit$`run time(s)`,df_by_hand$`run time(s)`)
```

```{r}

df_d1_sci <- filter(df_scikit,df_scikit$depth == 1)
df_d2_sci <- filter(df_scikit,df_scikit$depth == 2)
df_d4_sci <- filter(df_scikit,df_scikit$depth == 4)
df_d8_sci <- filter(df_scikit,df_scikit$depth == 8)

df_d1_scra <- filter(df_by_hand,df_by_hand$depth == 1)
df_d2_scra <- filter(df_by_hand,df_by_hand$depth == 2)
df_d4_scra <- filter(df_by_hand,df_by_hand$depth == 4)
df_d8_scra <- filter(df_by_hand,df_by_hand$depth == 8)

max(df_d1_sci$AUC)
max(df_d2_sci$AUC)
max(df_d4_sci$AUC)
max(df_d8_sci$AUC)

max(df_d1_scra$AUC)
max(df_d2_scra$AUC)
max(df_d4_scra$AUC)
max(df_d8_scra$AUC)

```

```{r}
#test to see if there is significant difference between depths for both sets of data
t.test(df_d1_sci$AUC,df_d1_scra$AUC)
t.test(df_d2_sci$AUC,df_d2_scra$AUC)
t.test(df_d4_sci$AUC,df_d4_scra$AUC)
t.test(df_d8_sci$AUC,df_d8_scra$AUC)
```


```{r}
#null = the AUC is the same for all depths , alt = the AUC is not the same for all depths
by_hand_anova_auc_depth <- kruskal.test(df_by_hand$AUC~df_by_hand$depth)
scikit_anova_auc_depth <- kruskal.test(df_scikit$AUC~df_scikit$depth)

by_hand_anova_auc_depth
scikit_anova_auc_depth
```

```{r}

names(df_scikit)[names(df_scikit) == 'run time(s)'] <- "runtime"
names(df_by_hand)[names(df_by_hand) == 'run time(s)'] <- "runtime"

auc_by_depth <- glm(AUC ~depth + runtime + dataset ,data=df_by_hand)
sci_auc_by_depth <- glm(AUC ~depth + runtime + dataset ,data=df_scikit)

summary(auc_by_depth)
summary(sci_auc_by_depth)

main_plots = allEffects(auc_by_depth)
main_plots_scr = allEffects(sci_auc_by_depth)

plot(main_plots)
plot(main_plots_scr)
print(sci_auc_by_depth$coefficients)
print(auc_by_depth$coefficients)
```

Interactions
```{r}
auc_by_depth <- glm(AUC ~runtime:depth + depth:dataset + dataset:runtime   ,data=df_by_hand)
sci_auc_by_depth <- glm(AUC ~runtime:depth + depth:dataset + dataset:runtime   ,data=df_scikit)

summary(auc_by_depth)
summary(sci_auc_by_depth)

main_plots = allEffects(auc_by_depth)
main_plots_scr = allEffects(sci_auc_by_depth)

plot(main_plots)
plot(main_plots_scr)
```


```{r}
auc_by_depth_trans <- glm(AUC ~depth + log10(runtime) + dataset ,data=df_by_hand)
sci_auc_by_depth_trans <- glm(AUC ~depth + log10(runtime) + dataset ,data=df_scikit)
summary(auc_by_depth_trans)
summary(sci_auc_by_depth_trans)
```
The AIC scores are slightly better for the model with the transformed variable. This is because the best AIC is the number with the largest absolute value.


