library(tidyverse)
library(rpart)

fit <- rpart(Species ~ ., data = iris, method = "class", )


plot(fit, uniform=TRUE,
     main="Classification Tree for Iris")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
