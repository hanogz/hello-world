setwd("C:/Users/hande/Desktop/FE418/project")

loc <- "C:/Users/hande/Desktop/FE418/project/healthcare-dataset-stroke-data.csv"
dataset <- read.table(file = loc, header = TRUE, sep = ",")

class(dataset)
head(dataset)
summary(dataset)


as.factor(dataset$gender)
dataset <- dataset[!(dataset$gender == "Unknown"), ]
as.factor(dataset$gender)

as.factor(dataset$age)
as.factor(dataset$hypertension)
as.factor(dataset$heart_disease)
as.factor(dataset$work_type)
as.factor(dataset$Residence_type)
as.factor(dataset$avg_glucose_level)

as.factor(dataset$bmi)
# there is data as N/A
dataset[dataset == " "] <- NA
dataset[dataset == "N/A"] <- NA
head(dataset)
as.factor(dataset$bmi)

as.factor(dataset$smoking_status)
as.factor(dataset$stroke)


dataset$weightstatus <- ifelse(dataset$bmi <= 18.5, "underweight", 
                        ifelse(dataset$bmi <=24.9, "normal", 
                        ifelse(dataset$bmi <= 29.9, "overweight", 
                        ifelse(dataset$bmi >= 30, "obese", " "))))
head(dataset)

# violin plot (age/gender)
require(ggplot2)
require(ggpubr)
vga <- ggplot(dataset, aes(x=gender, y=age, fill=gender)) + 
       labs(title="Violin Plot of Ages",x="Gender", y = "Ages") + 
       geom_violin(trim=FALSE)+ geom_boxplot(width=0.1, fill="white") 
       
vga

dev.new()

# age/gender two-sample t-test
# first variance
ansari.test(age~gender, dataset)
Ansari-Bradley test

data:  age by gender
AB = 3934835, p-value = 2.585e-05
alternative hypothesis: true ratio of scales is not equal to 1
# variances are not equal
# normal distribution, shapiro cannot be applied since the sample size is greater than 5000

require(ggpubr)
ggdensity(dataset$age, 
          main = "Density Plot of Ages",
          xlab = "Ages")

# no normal distribution
# ANOVA test without equal variances to see if there is any difference in their means

oneway.test(age~gender, data = dataset)
One-way analysis of means (not assuming equal variances)

data:  age and gender
F = 3.8469, num df = 1.0, denom df = 4358.7, p-value = 0.0499

#glucose/gender
ansari.test(avg_glucose_level~gender, dataset)
Ansari-Bradley test

data:  avg_glucose_level by gender
AB = 3856564, p-value = 0.2328
alternative hypothesis: true ratio of scales is not equal to 1
#variances are not equal
require(ggpubr)
ggdensity(dataset$avg_glucose_level, 
          main = "Density Plot of Glucose Levels",
          xlab = "Glucose level")
#no normal distribution, so ANOVA
oneway.test(avg_glucose_level~gender, data = dataset)
One-way analysis of means (not assuming equal variances)

data:  avg_glucose_level and gender
F = 14.9, num df = 1.0, denom df = 4306.2, p-value = 0.000115

ggplot(dataset, aes(x=gender, y=avg_glucose_level, fill=gender)) + 
  geom_boxplot() +
  labs(x="Gender", y = "Average glucose level") + 
  geom_signif(comparisons = list(c("Female", "Male")), map_signif_level=TRUE) + 
  stat_compare_means(method = "anova")



#bmi/gender

dataset <- na.omit(dataset)
head(dataset)

require(ggplot2)
ggplot(dataset, aes(x=gender, y=weightstatus, colour = weightstatus)) + 
  geom_count() + 
  theme_light()

# weight status pie chart
ws <- dataset$weightstatus
ws
wstable <- table(ws)
wstable
sum(is.na(dataset$weightstatus))
ws1 <- as.data.frame(wstable)
ws1
ws2 <- c(1231, 1920, 1409, 349, 201)
labels <- c("normal", "obese", "overweight", "underweight", "NA")
colours <- c('#4286f4','#bb3af2','#ed2f52','#efc023', "#ff8000ff")
pie(ws2, labels, main='Weight Status', col=colours, 
    init.angle=180, clockwise=TRUE)

# individuals suffering from stroke

require(plyr)
df_stroke <- as.data.frame(dataset$stroke)
df_stroke
syes <- count(df_stroke, vars = 1)
class(syes)
syes


syes1 <- c(4861, 249)

percentage_stroke <- function(x)
{
 sumdata <- syes1[1] + syes1[2]
 y <- 100*x/sumdata
 print(y)
}

l1 <- percentage_stroke(4861)
[1] 95.1272

l2 <- percentage_stroke(249)
[1] 4.872798
 
labels_s <- c("no stroke (95.13%)", "stroke (4.87%)")

colours_s <- c('#4286f4','#bb3af2')

pie_stroke <- pie(syes1, labels_s, main='Stroke Occurance', 
              col=colours_s, init.angle=180, clockwise=TRUE)
pie_stroke

# stroke & gender

sgender_df <- as.data.frame(cbind(dataset$gender, dataset$stroke))
sgender_df
head(sgender_df)
require(plyr)
sgender_count <- as.data.frame(table(sgender_df))
sgender_count
class(sgender_count)
sum(sgender_count$Freq)
sgender_percentage <- function(x)
{
 x <- 100*x/sum(sgender_count$Freq)
 x
}
snof <- sgender_percentage(2853)
snof
snom <- sgender_percentage(2007)
snom
syesf <- sgender_percentage(141)
syesf
syesm <- sgender_percentage(108)
syesm

sgen <- c("Female", "Male", "Female", "Male")
sgenper <- c(snof, snom, syesf, syesm)
sgenper <- round(sgenper, digits = 2)

sgentable <- data.frame(sgen, sgenper)


colnames(sgentable) <- c("Gender", "Percentage")
sgentable

require(gt)
gt_tab <- gt(data = sgentable)
gt_tab1 <-
  gt_tab %>%
  tab_header(title = "Percentages of the stroke occurance according to genders")
gt_tab2<- 
  gt_tab1 %>%
  tab_row_group(label = "No stroke",
    rows = 1:2) %>%
  tab_row_group(label = "With stroke", rows = 3:4)
gt_tab2




# glucose & stroke
dataset$stroke <- as.character(dataset$stroke)
require(ggplot2)
require(ggpubr)
ggplot(dataset, aes(stroke, avg_glucose_level)) + 
       geom_boxplot() + 
       stat_summary(fun.y=mean, geom="point", shape=20, size=14, color="red", 
       fill="red") + 
       ylab("Average glucose level") + 
       facet_grid(cols=vars(gender)) + 
       theme_grey() + 
       stat_compare_means(method = "anova") +  
       geom_signif(comparisons = list(c("0", "1")), map_signif_level=TRUE)


# bmi & stroke
dataset1 <- na.omit(dataset)
dataset1$stroke <- as.numeric(dataset1$stroke)

means <- aggregate(stroke ~  weightstatus, dataset1, mean)
class(means)
means$stroke <- round(means$stroke, digits = 2)
means

require(ggplot2)
require(ggpubr)

ggplot(dataset1, aes(weightstatus, stroke)) + 
       geom_violin((aes(fill = weightstatus))) + 
       geom_jitter(height = 0, width = 0.1) + 
       stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", 
       fill="red") + 
       ylab("Stroke risk") + 
       theme_grey() + 
       geom_text(data = means, aes(label = stroke, y = stroke + 0.15))







#glucose level & bmi

glucose1 <- dataset$avg_glucose_level
bmi1 <- as.numeric(dataset$bmi)
head(bmi1)
glucose_bmi <- data.frame(glucose1, bmi1)
head(glucose_bmi)
colnames(glucose_bmi) <- c("average glucose level", "bmi")
head(glucose_bmi)
class(glucose_bmi)
cor(glucose_bmi, use = "pairwise.complete.obs")
                      average glucose level       bmi
average glucose level             1.0000000 0.1756717
bmi                               0.1756717 1.0000000

relation <- lm(glucose1~bmi1, data = glucose_bmi)
summary(relation)
plot(glucose1, bmi1)
abline(relation)


# positive correlation between average glucose level and bmi is found.

# stroke/hypertension



require(ggplot2)
require(ggpubr)



dataset$hypertension <- as.character(dataset$hypertension)
dataset$stroke <- as.character(dataset$stroke)
hyper_df <- as.data.frame(cbind(dataset$stroke, dataset$hypertension))
colnames(hyper_df) <- c("Stroke", "Hypertension")
hyper_df$Hypertension[hyper_df$Hypertension == "0"] <- "No"
hyper_df$Hypertension[hyper_df$Hypertension == "1"] <- "Yes"
hyper_df$Stroke[hyper_df$Stroke == "0"] <- "No Stroke"
hyper_df$Stroke[hyper_df$Stroke == "1"] <- "With Stroke"
head(hyper_df)
gender <- dataset$gender
ggplot(hyper_df, aes(Hypertension, fill = gender)) + 
       geom_bar(position=position_dodge(), colour = "black") + 
       facet_grid(cols=vars(Stroke)) + 
       theme_light()
       
#stroke/heart disease

require(ggplot2)
require(ggpubr)
dataset$heart_disease <- as.character(dataset$heart_disease)
dataset$stroke <- as.character(dataset$stroke)
heart_df <- as.data.frame(cbind(dataset$stroke, dataset$heart_disease))
colnames(heart_df) <- c("Stroke", "HeartDisease")
heart_df$HeartDisease[heart_df$HeartDisease == "0"] <- "No"
heart_df$HeartDisease[heart_df$HeartDisease == "1"] <- "Yes"
heart_df$Stroke[heart_df$Stroke == "0"] <- "No Stroke"
heart_df$Stroke[heart_df$Stroke == "1"] <- "With Stroke"
head(heart_df)
gender <- dataset$gender
ggplot(heart_df, aes(HeartDisease, fill = gender)) + 
       geom_bar(position=position_dodge(), colour = "black") + 
       facet_grid(cols=vars(Stroke)) + 
       theme_light()

#stroke/ smoking
 
require(ggplot2)
require(ggpubr)

dataset$stroke <- as.numeric(dataset$stroke)
smoking_df <- as.data.frame(cbind(dataset$stroke, dataset$smoking_status))
colnames(smoking_df) <- c("Stroke", "SmokingStatus")
smoking_df$Stroke[smoking_df$Stroke == "1"] <- "Yes"
head(smoking_df)
gender <- dataset$gender
ggplot(smoking_df, aes(Stroke)) + 
       geom_bar(position=position_dodge(), colour = "black") + 
       facet_grid(cols=vars(SmokingStatus)) + 
       theme_light() + 
       scale_x_discrete(limits = c("Yes"))




