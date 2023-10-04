install.packages("np", dependencies = T)
library(np)

np_model <- npreg(tx.pcarpitacorreta ~ + Saúde + Educação + Administração + Custeio + Seguridade.Social, data = panel.teste, gradients = TRUE
                  , index = c("i", "t"))


summary(np_model)


plot(np_model, which = "bw")

plot(np_model, which = "txcorrente.correta")


# Print the summary of the model
summary(np_model)

# Extract the bandwidth
bw <- np_model$bw
print(bw)

# Extract the kernel type
kernel_type <- np_model$kernel
print(kernel_type)

y = m(x) using [kernel_type] kernel with bandwidth = [bw]



# Extract the bandwidth
bw <- np_model$bw

# Extract the kernel type
kernel_type <- np_model$kernel

# Print the model description
cat("y = m(x) using", kernel_type, "kernel with bandwidth =", bw, "\n")
