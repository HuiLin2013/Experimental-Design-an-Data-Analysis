###Exercise 2 of Assignment 3
##Test whether two populations in clouds are equal
clouds <- read.table("clouds.txt", skip=1)
print(clouds)

seeded <- sapply(c(clouds[1]), as.numeric)
seeded <- seeded[,1]
print(seeded)
 
unseeded <- sapply(c(clouds[2]), as.numeric)
unseeded <- unseeded[,1]
print(unseeded)

#Two sample t-test
t.test(seeded, unseeded)
#Mann Whitney U test
wilcox.test(seeded, unseeded)
#Kolmogorov SmimovS
ks.test(seeded, unseeded)


##function to return converted values in clouds
converted <- function (data, type){
  B=length(data)
  converted=numeric(B)
  if(type == "sqrt")
  {
    for(i in 1:B)
    {
      converted[i]=sqrt(data[i])
    }
  }
  else if(type == "sqrt2")
  {
    for(i in 1:B)
    {
      converted[i]=sqrt(sqrt(data[i]))
    }
  }
  return(converted) 
}


##Repeat the same procedure on the sqrt of values in clouds
sqrt_seeded <- converted(seeded, "sqrt")
sqrt_unseeded <- converted(unseeded, "sqrt")
#Two sample t-test
t.test(sqrt_seeded, sqrt_unseeded)
#Mann Whitney U test
wilcox.test(sqrt_seeded, sqrt_unseeded)
#Kolmogorov SmimovS
ks.test(sqrt_seeded, sqrt_unseeded)


##Repeat the same procedure on the sqrt of the sqrt of values in clouds
sqrt2_seeded <- converted(seeded, "sqrt2")
sqrt2_unseeded <- converted(unseeded, "sqrt2")
#Two sample t-test
t.test(sqrt2_seeded, sqrt2_unseeded)
#Mann Whitney U test
wilcox.test(sqrt2_seeded, sqrt2_unseeded)
#Kolmogorov SmimovS
ks.test(sqrt2_seeded, sqrt2_unseeded)

