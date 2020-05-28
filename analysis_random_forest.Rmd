---
title: "analysis_random_forest"
output:
  html_document:
    df_print: paged
---

```{r}
library(tidyverse)
library(corrplot)
library(effects)
```

```{r}
df_by_hand <- read_csv("df_forest_by_hand.csv")
df_scikit <- read_csv("df_forest_scikit.csv")

df_ntree_focus <- read_csv("df_ntree_focus.csv")
df_sci_ntree_focus <- read_csv("df_sci_ntree_focus.csv")


df_by_hand$dataset <- as.factor(df_by_hand$dataset)
df_scikit$dataset <- as.factor(df_scikit$dataset)

df_by_hand$no_trees <- as.factor(df_by_hand$no_trees)
df_scikit$no_trees <- as.factor(df_scikit$no_trees)
```

```{r}
ggplot(df_scikit) +
  geom_boxplot(aes(x=dataset, y=df_scikit$AUC ,fill=dataset)) +
  scale_fill_brewer(palette="Set1") +
  labs(title = "AUC scores for different datasets, Scikit Decision Tree",
       x = "dataset", y = "AUC", color = "Legend\n")

ggplot(df_by_hand) +
  geom_boxplot(aes(x=dataset, y=df_by_hand$AUC,fill=dataset)) +
  scale_fill_brewer(palette="Set1") +
  labs(title = "AUC scores for different datasets, From Scratch Decision Tree ",
       x = "dataset", y = "AUC", color = "Legend\n")
```

```{r}

ggplot(df_scikit) +
  geom_boxplot(aes(x=no_trees, y=AUC,fill=no_trees)) +
  scale_fill_brewer(palette="Set2") +
  labs(title = "AUC scores for different number of trees, Scikit Decision Tree",
       x = "no_trees", y = "AUC", color = "Legend\n")

ggplot(df_by_hand) +
  geom_boxplot(aes(x=no_trees, y=AUC,fill=no_trees)) +
  scale_fill_brewer(palette="Set2") +
  labs(title = "AUC scores for different number of trees, From Scratch Decision Tree ",
       x = "no_trees", y = "AUC", color = "Legend\n")

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
ggplot(df_sci_ntree_focus) + 
geom_jitter(aes(x=no_trees,y=AUC,col=dataset)) + 
scale_fill_brewer(palette="Set1") +
labs(title = "Number of trees Against Final AUC, Scikit",
x = "number of trees", y = "Final Averaged AUC Score", color = "Legend\n")

ggplot(df_ntree_focus) + 
geom_jitter(aes(x=no_trees,y=AUC,col=dataset)) + 
scale_fill_brewer(palette="Set1") +
labs(title = "Number of trees Against Final AUC, From Scratch",
x = "number of trees", y = "Final Averaged AUC Score", color = "Legend\n")

ggplot(df_sci_ntree_focus) + 
geom_jitter(aes(x=no_trees,y=runtime,col=dataset)) + 
scale_fill_brewer(palette="Set1") +
labs(title = "Number of trees Against runtime, Scikit",
x = "number of trees", y = "runtime (seconds)", color = "Legend\n")

ggplot(df_ntree_focus) + 
geom_jitter(aes(x=no_trees,y=runtime,col=dataset)) + 
scale_fill_brewer(palette="Set1") +
labs(title = "Number of trees Against runtime, From Scratch",
x = "number of trees", y = "runtime (seconds)", color = "Legend\n")
```


```{r}
df_sci_copy <- data.frame(df_scikit)
df_sci_copy$dataset <- as.numeric(df_sci_copy$dataset)
df_sci_copy$no_trees <- as.numeric(df_sci_copy$no_trees)
df_sci_mat <- cor(df_sci_copy)
corrplot(df_sci_mat,order="hclust",fig='Figure 1')

df_scratch_copy <- data.frame(df_by_hand)
df_scratch_copy$dataset <- as.numeric(df_scratch_copy$dataset)
df_scratch_copy$no_trees <- as.numeric(df_scratch_copy$no_trees)
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
df_d1_sci <- filter(df_scikit,df_scikit$no_trees == 2)
df_d2_sci <- filter(df_scikit,df_scikit$no_trees == 5)
df_d4_sci <- filter(df_scikit,df_scikit$no_trees == 10)
df_d8_sci <- filter(df_scikit,df_scikit$no_trees == 20)

df_d1_scra <- filter(df_by_hand,df_by_hand$no_trees == 2)
df_d2_scra <- filter(df_by_hand,df_by_hand$no_trees == 5)
df_d4_scra <- filter(df_by_hand,df_by_hand$no_trees == 10)
df_d8_scra <- filter(df_by_hand,df_by_hand$no_trees == 20)

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
mean(df_scikit$AUC)
mean(df_by_hand$AUC)
#significant difference in AUC?
t.test(df_scikit$AUC,df_by_hand$AUC)

#significant difference in run time?
t.test(df_scikit$`run time(s)`,df_by_hand$`run time(s)`)
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
by_hand_anova_auc_ntrees <- kruskal.test(df_by_hand$AUC~df_by_hand$no_trees)
scikit_anova_auc_ntrees <- kruskal.test(df_scikit$AUC~df_scikit$no_trees)

by_hand_anova_auc_ntrees
scikit_anova_auc_ntrees
```

```{r}
names(df_scikit)[names(df_scikit) == 'run time(s)'] <- "runtime"
names(df_by_hand)[names(df_by_hand) == 'run time(s)'] <- "runtime"

auc_by_ntree <- glm(AUC ~no_trees + runtime + dataset ,data=df_by_hand)
sci_auc_by_ntree <- glm(AUC ~no_trees + runtime + dataset ,data=df_scikit)

summary(auc_by_ntree)
summary(sci_auc_by_ntree)

main_plots = allEffects(auc_by_ntree)
main_plots_scr = allEffects(sci_auc_by_ntree)

plot(main_plots)
plot(main_plots_scr)
print(sci_auc_by_ntree$coefficients)
print(auc_by_ntree$coefficients)
```

