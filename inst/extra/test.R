
library(tensorflow)
# use_condaenv("r-tensorflow", required = TRUE)
sess = tf$Session()
hello <- tf$constant('Hello, TensorFlow!')
sess$run(hello)
