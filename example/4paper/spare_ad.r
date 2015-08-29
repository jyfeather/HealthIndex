library(ggplot2)

coefs <- c("Amygdala_L", "Rectus_L", "Postcentral_L", "Precuneus_R", "Occipital_Inf_L",
           "Hipppocampus_L", "Precentral_L", "Temporal_Mid_R", "Frontal_Med_Orb_R", "Hippocampus_R")
coefs <- data.frame(name = coefs)
coefs <- cbind(coefs, value = c(7, 6.5, 5.8, 4.2, 3.8, 3.7, 3.5, 3.3, 3.2, 3.0))

ggplot(data = coefs, aes(x = name, y = value)) + 
  geom_bar(stat = "identity") + theme_bw() + xlab("feature name") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), text = element_text(size = 18))
