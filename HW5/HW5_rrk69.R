library(readr)
Gun_Control <- read_csv("Gun_Control.csv")

#Precaution: law of small numbers warns us to be careful...
table(Gun_Control$Gun_Laws)


#Aggregating mean monetary damage by Gun Control level
tapply(Gun_Control$Monetary_Damage,Gun_Control$Gun_Laws,mean)
barplot(tapply(Gun_Control$Monetary_Damage,Gun_Control$Gun_Laws,mean), main="Average Cost by Gun Policy")

#partion data based on gun control regulation
loose <- subset(Gun_Control, Gun_Control$Gun_Laws=="Loose_Gun_Laws")
medium <- subset(Gun_Control, Gun_Control$Gun_Laws=="Medium_Gun_Laws")
strict <- subset(Gun_Control, Gun_Control$Gun_Laws=="Strict_Gun_Laws")
barplot(table(Gun_Control$Gun_Laws),main="Frequency of Gun Laws")


#top 100 most expensive 
top <- subset(Gun_Control[order(Gun_Control$Monetary_Damage),], Monetary_Damage > 10000.526)

sum(top$Gun_Laws == "Loose_Gun_Laws") # 61 of top 100 are loose gun laws
sum(top$Gun_Laws == "Strict_Gun_Laws") # 0 of the top 100 are strict gun laws

#for top 100, sum of cost by gun policy group
tapply(top$Monetary_Damage,top$Gun_Laws,sum)

#Permutation test for top100 subset, between loose and medium
devtools::install_github("devanshagr/PermutationTestSecond")
PermutationTestSecond::Permutation(top, "Gun_Laws", "Monetary_Damage",5000,"Loose_Gun_Laws", "Medium_Gun_Laws")
# result = 8e-04, 