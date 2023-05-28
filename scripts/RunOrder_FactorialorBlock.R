# Creating three factors (or blocking factors)
# and listing their levels
Factor1 <- c("A1","A2","A3")
Factor2 <- c("B1","B2","B3")
Factor3 <- c("C1","C2")

# expand.grid is an easy way to get all combinations of factor levels
Design <- expand.grid(Factor1,Factor2,Factor3)
names(Design) <- c("Factor1","Factor2","Factor3")

# If you want to print out what we've got so far, you can
# uncomment the line "Design" below and see that we've got
# a list of all possible treatments 
# Design

# We can create replicates by using rbind
Design <- rbind(Design,Design) 

# Now we need to decide how/when to do each of these treatments.

# Typically we'll want to randomise the run order, which can
# be done like this:
Design$RunOrder <- sample(1:nrow(Design),replace=FALSE)

# You can randomise other things by duplicating and adjusting
# the line above.

# The line below shows a preview of the design
head(Design)
