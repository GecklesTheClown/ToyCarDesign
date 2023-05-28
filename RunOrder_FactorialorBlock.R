# Creating three factors (or blocking factors)
# and listing their levels
Factor1 <- c("Carpet","Marble")
Factor2 <- c("15","30","45")
Factor3 <- c("0","10","20")

# expand.grid is an easy way to get all combinations of factor levels
Design <- expand.grid(Factor1,Factor2,Factor3)
names(Design) <- c("Surface_Type","Angle","A.Weight")

# We can create replicates by using rbind
Design2 <- rbind(Design,Design) 
Design3 <- rbind(Design2,Design) 

# Now we need to decide how/when to do each of these treatments.

# Typically we'll want to randomise the run order, which can
# be done like this:
Design3$RunOrder <- sample(1:nrow(Design3),replace=FALSE)

# You can randomise other things by duplicating and adjusting
# the line above.

# The line below shows a preview of the design
head(Design3)
