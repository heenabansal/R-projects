require(FactoMineR)
require(ggplot2)
# load data tea
data(tea)

# select these columns
newtea = tea[, c("Tea", "How", "how", "sugar", "where", "always")]

# take a peek
head(newtea)

cats = apply(newtea, 2, function(x) nlevels(as.factor(x)))

cats

mca1 = MCA(newtea, graph = FALSE)

# list of results
mca1

mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(cats), cats))

# data frame with observation coordinates
mca1_obs_df = data.frame(mca1$ind$coord)

# plot of variable categories
ggplot(data=mca1_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR")

