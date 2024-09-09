# ===========================================================
# Example: mushrooms
# Use this script to follow mushroom eda lecture
# ===========================================================

# link to documentation: https://archive.ics.uci.edu/ml/datasets/mushroom
# this data set is a random sample of the full data
mushroom <- read.csv("mushrooms.csv")
dim(mushroom)
names(mushroom)

# univariate summaries
class(mushroom$cap.color)
mushroom$cap.color <- as.factor(mushroom$cap.color)
table(mushroom$cap.color)
cap.color.t <- table(mushroom$cap.color)
barplot(cap.color.t)

edible.t <- table(mushroom$edible)
edible.t
barplot(edible.t)
prop.table(edible.t)

# association between cap color and edible
edible.capcolor.t <- table(mushroom$cap.color, mushroom$edible)
edible.capcolor.t

# give the table more informative names
dimnames(edible.capcolor.t)
dimnames(edible.capcolor.t) <- list(capcolor=c("buff","cinnamon","red","gray",
                                               "brown","pink","purple","white","yellow"),
                                    ed=c("edible","poisonous"))
edible.capcolor.t

mosaicplot(edible.capcolor.t, color=TRUE)


edible.capcolor.t2 <- table( mushroom$edible,mushroom$cap.color)
dimnames(edible.capcolor.t2) <- list(ed=c("edible","poisonous"),capcolor=c("buff","cinnamon","red","gray",
                                                                           "brown","pink","purple","white","yellow"))
mosaicplot(edible.capcolor.t2, color=TRUE)
barplot(edible.capcolor.t)
barplot(edible.capcolor.t2)


# proportion tables
# compare interpretation of row proportions vs column proportions
prop.table(edible.capcolor.t,margin=1) # row proportions

prop.table(edible.capcolor.t,margin=2)

# what is a list?
empty.list <- list()
empty.list[[1]] <- c(2,5,1,9)
empty.list
empty.list[[2]] <- mushroom

# use a for loop to make a list of tables
n.variables <- ncol(mushroom)
names(mushroom)
list.of.tables <- list()

for( j in 2:n.variables)
{
  list.of.tables[[j]] <- table(mushroom$edible, mushroom[,j])
}

names(mushroom)[12]
list.of.tables[[12]]
