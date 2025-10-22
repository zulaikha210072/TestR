#fall 2025/ITC 255
dftips = read.csv("tips.csv")
View(dftips)

#to identify our varibales - that which are our varibales 
names(dftips)
View(dftips)

mean(dftips$total_bill)
median(dftips$total_bill)
sd(dftips$total_bill)

AbsFreq = table(dftips$time)
AbsFreq
prop.table(AbsFreq)

AbsFreq = table(dftips$size)
AbsFreq
prop.table(AbsFreq)
RelFreq = round(prop.table(AbsFreq), 2)

FDTQL = function(x){
  AbsFreq = table(x)
  RelFreq = round(prop.table(AbsFreq), 3)
  CUMFreq = cumsum(RelFreq)
  FDTx = cbind(AbsFreq, RelFreq, CUMFreq)
  return(FDTx)
  
  FDTQL(dftips $sex)
  FDTQL(dftips $time)
}