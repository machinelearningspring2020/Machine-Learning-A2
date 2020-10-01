# BEGIN

# Delete any existing variables/work on your machine
rm(list=ls())


# Create a function to check if the data is made up of one class
Uniqueness <- function(unq) {
  
  length(unique(unq[,ncol(unq)])) == 1
  
}


# Create a function to measure the uniqueness of the data
Entropy <- function(data) {
  
  # Use formula to find entropy
  ent <- data/sum(data) * log2(data/sum(data))
  ent[data == 0] <- 0
  -sum(ent)
}


# Create a function to calculate the information gain
InfoGain <- function(subset) {
  
  # Create subset dataset for information gain
  subset <- as.data.frame.matrix(subset)
  
  # Calculate entropy before the split
  ent1 <- Entropy(colSums(subset))
  
  split <- rowSums(subset)
  # Calculate entropy after the split
  ent2 <- sum (split / sum(split) * apply(subset,
                                          MARGIN = 1, 
                                          FUN = Entropy ))
  
  # Calculate information gain by finding the difference of the entropies
  IG <- ent1 - ent2
  
  return (IG)
}


# Provide examples of information gain
ex1 <- table(iris[,c('Sepal.Length', 
                        'Petal.Length')])
InfoGain(ex1)

ex2 <- table(iris[,c('Petal.Length', 
                        'Petal.Width')])
InfoGain(ex2)


# Create a function to train the algorithm
ID3 <- function(root, Dataset) 
  
{
  # Count and insert the number of each observation into a dataframe 
  root$obsCount <- nrow(Dataset)
  
  # If the dataset only contains one class, create a 'leaf' of that class 
  if (Uniqueness(Dataset)) 
    
  {
    # Create dataframe 'child' containing the class
    child <- root$AddChild(unique(Dataset[,ncol(Dataset)]))
    root$variable <- tail(names(Dataset), 1)
    # Add the count of observations
    child$obsCount <- nrow(Dataset)
    # Add the feature of that class in another column
    child$variable <- ''
    
  } 
  
  # If the dataset contains more than one class, 
  # choose the feature with the highest IG
  else {
    
    info.gain <- sapply(colnames(Dataset)[-ncol(Dataset)], 
                        
# Use InfoGain function to find the information gains and insert them into a table               
                        function(x) InfoGain(
                          
                          table(Dataset[,x], 
                                Dataset[,ncol(Dataset)])
                          
                        )
    )
    # Choose the highest IG in the table using 'max'  
    variable <- names(info.gain)[info.gain == max(info.gain)][1]
    
    root$variable <- variable
    
    # Split the dataset by subsetting according to the feature with the highest IG   
    subsetdataset <- split(Dataset[,!(names(Dataset) %in% variable)], 
                           Dataset[,variable], 
                           drop = TRUE)
    
    # Create a children-node for that feature    
    for(i in 1:length(subsetdataset)) {
      child <- root$AddChild(names(subsetdataset)[i])
      
# Use the function-inside-the-function method on the feature using the subsetted data       
      ID3(child, subsetdataset[[i]])
    }
    
  }
  
}


# Create a function to predict the class in the dataset
Predict <- function(dectree, variables) {
  
  # If the dataset contains a leaf, return the leaf/class name    
  if (dectree$children[[1]]$isLeaf) 
    return (dectree$children[[1]]$name)
  
  # Develop the children nodes by branching off the variables in the dataset
  child <- dectree$children[[variables[[dectree$variable]]]]
  
  # Return the leaf node 
  return (Predict(child, variables))
}


# Install and load the data.tree package which allows us to use the Node function
install.packages('data.tree') # Run this line if package is not installed yet
library(data.tree)


# Load the iris dataset into a dataframe called "iris"
data("iris")
iris


#split the data with 90% to be trained and 10% to be used for prediction
library(caTools) # Ensure package is installed first
partition <- sample.split(iris$Species, SplitRatio = 0.9)
iristrain <- subset(iris, partition == TRUE)
iristest <- subset(iris, partition == FALSE)


# We can now use the ID3 Algorithm on our chosen dataset: The Iris Dataset 
# Create the root of the tree using the Node function on the Iris Dataset 
DecisionTree <- Node$new("iristrain")

# Implement the ID3 algorithm on the root of that tree 
ID3(DecisionTree, iristrain)

# Show the tree including the variable names and the count of each observation
print(DecisionTree, "variable", "obsCount")

# Predict the class of the following test data using the Predict function
testdata = c(Sepal.Length = '4.8', 
             Sepal.Width = '3.4', 
             Petal.Length = '1.6', 
             Petal.Width = '0.2')

Predict(DecisionTree, testdata)

# Predict the class of the following test data using the Predict function
testdata = c(Sepal.Length = '5.0', 
             Sepal.Width = '2.0', 
             Petal.Length = '3.5', 
             Petal.Width = '1.0')

Predict(DecisionTree, testdata)

# Predict the class of the following test data using the Predict function
testdata = c(Sepal.Length = '6.7', 
             Sepal.Width = '3.1', 
             Petal.Length = '5.6', 
             Petal.Width = '2.4')

Predict(DecisionTree, testdata)

# END