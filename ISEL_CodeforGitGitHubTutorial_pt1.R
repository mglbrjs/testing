# GMRI Integrated Systems Ecology Lab: Git, GitHub and You example code

#####
## You, working by yourself
#####
# Data prep and exploration -----------------------------------------------

## Data prep using preloaded "trees" dataset in R.  As a clean dataset, there really isn't much prep other than loading in the dataset and checking out its structure.

# Load it in
data(trees)

# Preliminary inspection: format, structure, dimensions
head(trees)
str(trees)
dim(trees)
colnames(trees)

# Little tree ecology for us marine folks: tree height and girth are commonly measured, while measuring tree volume is more difficult and less appealing as it requires either cutting down the tree, or climing all over it and taking a lot of precise measurements. Ideally, we'd like to be able to build a model that relates height and girth to tree volume.  

## Exploration
# For this we are going to go outside base R functions and use a lot of functions from libraries packaged in the "tidyverse" library. So, let's install it (if we need to) and then load it.
library_check<- function(libraries) {
  ## Details
  # This function will check for and then either load or install any libraries needed to run subsequent functions
  
  # Args:
  # libraries = Vector of required library names
  
  # Returns: NA, just downloads/loads libraries into current work space.
  
  ## Start function
  lapply(libraries, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
  ## End function
}

library_check(c("tidyverse", "cowplot", "GGally", "plotly", "broom"))

# Hopefully that all worked okay. A billion different ways to visualize these data, so just a few here...
# Variable histogram plots
girth.hist<- ggplot(data = trees) +
  geom_histogram(mapping = aes(x = Girth))
height.hist<- ggplot(data = trees) +
  geom_histogram(mapping = aes(x = Height))
vol.hist<- ggplot(data = trees) +
  geom_histogram(mapping = aes(x = Volume))
plot_grid(girth.hist, height.hist, vol.hist, labels = c("Girth", "Height", "Volume"), nrow = 1, align = "hv")

# Covariation plot
ggpairs(trees)


# Data modeling: linear regression ----------------------------------------

## Simple linear regression model for volume as a function of girth
# Fit the model
girth.mod<- lm(Volume ~ Girth, data = trees)

# Diagnostics
plot(girth.mod)

# Inferences and evaluation
summary(girth.mod)

# Visualize the model
ggplot(data = trees, aes(x = Girth, y = Volume)) +
  geom_point()  +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")





# Add something short of your own! ----------------------------------------
## Do whatever you want, save it locally, stage, commit, push the changes. 
