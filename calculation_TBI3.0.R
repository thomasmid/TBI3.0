#install.packages("readxl")
#install.packages("writexl")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("rstudioapi")
library(readxl)
library(writexl)
library(ggplot2)
library(dplyr)
library(rstudioapi)

# set working directory
setwd(dirname(getActiveDocumentContext()$path)) 

#create a list to hold output dataframes
result_list<-vector(mode="list", length=6)

#for every site
for(s in 1:6){
  
# import data
input <- read_excel("data_input_TBI3.0.xlsx", sheet = s+1)

#check if there are input parameters in every data column
if(all(colSums(input[,3:6])==0)){ #if no input parameters, skip
  next
  }else{ #if sheet has input parameters, execute loop
input <- input[, colSums(input != 0, na.rm = TRUE) > 0]

# define target parameters
parameters <- c("a","a_SE","k","k_SE")

# define model parameters' start values
start_values <- list(a = 0.6, k = 0.05)
upper_limit <- c(a=1,k=Inf)

# initiate loop
x <- colnames(subset(input, select = - c(1,2)))
TBI_model <- data.frame(parameters)
for (i in seq(x)) {
  TBI_fit <- nls(input[[paste0(x[[i]])]] ~ a*exp(-k*duration)+(1-a),
                 data = input,
                 start = start_values,
                 upper = upper_limit,
                 algorithm = "port",
                 nls.control(warnOnly = TRUE,maxiter=50))
  
# summarize model
  TBI_sum <- summary(TBI_fit)
  a <- TBI_sum$coefficients[[1]]
  a_SE <- TBI_sum$coefficients[[3]]
  k <- TBI_sum$coefficients[[2]]
  k_SE <- TBI_sum$coefficients[[4]]
  
# define function
  TBI_function <- 
    function(duration){a * exp(-k*duration) + (1-a)}
  
# define results
  d30 <- TBI_function(30)
  d60 <- TBI_function(60)
  d90 <- TBI_function(90)
  
# get results
  TBI_model[,i+1] <- c(a,a_SE,k,k_SE)
# close loop
}

# rearrange results
#names(TBI_model) <- c("parameter",x)
TBI_model_A <- t(subset(TBI_model[,-1]))
colnames(TBI_model_A) <- c("a", "a_SE","k","k_SE")
TBI_model_A <- as.data.frame(TBI_model_A)
TBI_model_A$site <- colnames(input[,1])
TBI_model_A$tea_product <- x

#write dataframe into results list
result_list[[s]]<-TBI_model_A
  }
}

#write all list items in one data.frame
TBI_results<-do.call(rbind, result_list)

# export results as a data frame
write_xlsx(TBI_results,"results_TBI3.0.xlsx")

# export results as a plot
Fig_results <- ggplot(TBI_results, aes(x = a, y = k, colour = site, shape = tea_product))+
  geom_errorbar(aes(ymin = k - k_SE, ymax = k + k_SE), colour = "grey2", width = 0, alpha = 0.6)+
  geom_errorbar(aes(xmin = a - a_SE, xmax = a + a_SE), colour = "grey2", width = 0, alpha = 0.6)+
  geom_point(size = 5, stroke = 1, alpha = 0.8)+
  theme_classic()+
  ggtitle("Modeled parameters of TBI 3.0: *a* and *k* with standard errors")+
  xlab("*a*")+
  ylab("*k*")+
  theme(plot.title = ggtext::element_markdown(), axis.title.y = ggtext::element_markdown(),axis.title.x = ggtext::element_markdown())+
  theme(legend.position = "bottom")
ggsave(filename="results_TBI3.0.png",Fig_results, width = 8,height =6,dpi = 2000)
