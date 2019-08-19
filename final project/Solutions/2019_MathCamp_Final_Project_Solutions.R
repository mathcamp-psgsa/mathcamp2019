# 2019 Math Camp Project Solutions

# Note that the solutions implemented here use only simple base R functions/syntax, and exact implementation by students will vary.
# The format of this solution set is given according to each heading in the 2019_MathCamp_Final_Project.pdf document,
# with numbering matching the numbering given per-section.


# "Exploring the Data Set"

# 1

minwage=read.csv("C:\\Users\\zeralesaar\\Desktop\\minwage_final_project.csv")

attach(minwage)

dim(minwage) # Obviously one can observe in the RStudio data pane that 358 observations of 8 variables exist, but dim() is also fine

names(minwage) # Check variable names

sum(is.na(minwage)) # Check for any missing entries

minwage$state=0 # Initialize new vector in minwage to contain observations by state
minwage$state[minwage$location=="PA"]="PA"
minwage$state[minwage$location!="PA"]="NJ" # Fill the vector appropriately, here using strings to simplify the next step

attach(minwage) # Reattach after adding the state vector

table(minwage$state) # Output a simple table, where the string values make it more readable than just using a 0/1 indicator

# 2

boxplot(wageBefore ~ chain,
        ylab = "Wage before Min. Increase (USD)",
        xlab = "Fast Food Chain",
        names = c("Burger King", "KFC", "Roy Rogers", "Wendy's")) # Simple boxplot of wages prior to the change

# Seems like Roy Roger's and Wendy's both pay better than KFC and BK, and that BK's wages are distributed more tightly about the
# BK mean than KFC.

boxplot(wageAfter ~ chain,
        xlab = "Fast Food Chain",
        ylab = "Wage after Min. Increase (USD)",
        names = c("Burger King", "KFC", "Roy Rogers", "Wendy's"))

# The actual wageAfter values display very tight means and very low SD (globally 4.99, 0.258, with inter-chain variation being small)
# So, the boxplot is ugly, but not necessarily that unreasonable considering that 259/358 obs. have wageAfter=5.05
# We could make it more useful, but it actually suggests pretty reasonably that wages clustered pretty tightly and evened out across
# chains, so I would accept that. A different form of plot could also be useful here.

# "Manipulating the Data"

# 1

PABA=c(mean(wageBefore[state=="PA"]), # Calculating mean wages by state before/after and storing them as vectors
mean(wageAfter[state=="PA"]))

NJBA=c(mean(wageBefore[state=="NJ"]),
mean(wageAfter[state=="NJ"]))

diff=c(PABA[2]-PABA[1], NJBA[2]-NJBA[1]) # Calculate mean differences by state

rel.diff=c(100*(diff[1]/PABA[1]), 100*(diff[2]/NJBA[1])) # Calculate relative difference as percentage

BA.mean.compare=rbind(PABA, NJBA)
BA.mean.compare=cbind(BA.mean.compare, diff, rel.diff) # Put it all in one place, can feed into stargazer() in .rmd for labeling/etc.

# NJ experienced the larger wage change, as expected, but here we see for the first time that there seems to be a negative effect
# on PA wages. Interesting. Table above is not strictly necessary, but stylistically desirable.

# 2

PA.PT=c(mean(partBefore[state=="PA"]), # means of part-time, full-time before/after
        mean(partAfter[state=="PA"]))
        
NJ.PT=c(mean(partBefore[state=="NJ"]), 
        mean(partAfter[state=="NJ"]))

PA.FT=c(mean(fullBefore[state=="PA"]),
         mean(fullAfter[state=="PA"]))

NJ.FT=c(mean(fullBefore[state=="NJ"]),
  mean(fullAfter[state=="NJ"]))

PTFT.mean.compare=rbind(PA.PT, PA.FT, NJ.PT, NJ.FT)

# Looks like full-time employment in PA dropped a bit, while part-time increased *very* slightly; 
# NJ sees a small drop in PT, increase in FT. Naively, this suggests that increasing the minimum wage improved full-time
# employment in NJ, but again it is strange to see the drop in FT in PA

# 3

cNJ.diffs=vector(mode="numeric", length=3)
nNj.diffs=vector(mode="numeric", length=3)
PA.diffs=vector(mode="numeric", length=3)
shNJ.diffs=vector(mode="numeric", length=3)
sNJ.diffs=vector(mode="numeric", length=3)

regions.diffs=cbind(PA.diffs, cNJ.diffs, nNj.diffs, shNJ.diffs, sNJ.diffs)

for(i in 1:length(unique(location))){
  regions.diffs[1,i]=mean(wageAfter[location==unique(location)[i]]) - mean(wageBefore[location==unique(location)[i]])
  regions.diffs[2,i]=mean(fullAfter[location==unique(location)[i]]) - mean(fullBefore[location==unique(location)[i]])
  regions.diffs[3,i]=mean(partAfter[location==unique(location)[i]]) - mean(partBefore[location==unique(location)[i]])
}

# The solution to this one can vary greatly, but the above *should* be implementable with what they learned about indexing and
# for-loops. Simpler methods of doing this exist, but I don't know exactly what they would have done. As long as their
# resultant comparison includes the values that fill regions.diffs as above, the implementation is probably
# reasonable.

# 4

PTFT.diff=PTFT.mean.compare[,2]-PTFT.mean.compare[,1] # Calc./store differences in before/after PT, FT


# "Hypothesis Testingadn Linear Regression Modeling"

# 1

# As long as their "hypotheses" are specific and directional in form (e.g. "increasing the minimum wage causes FT to increase")
# the responses should be fine. If they didn't bother actually listing their own responses, it's fairly inconsequential -
# the project highlights just those two samples anyway.

# 2

minwage$treatment=0
minwage$treatment[state=="NJ"]=1 # Create treatment indicator for NJ
attach(minwage) 

# 3

wages.diff=wageAfter-wageBefore
FT.diff=fullAfter-fullBefore # Construct data for outcomes

lmemp=lm(FT.diff ~ treatment + chain)
lmwage=lm(wages.diff ~ treatment + chain) # SImple OLS models based on Jae and Rebeca's model for coefficient interpretation

# 4 

# The results of the two regression models should be presented in a nice stargazer() output format, preferably in a single table

# "Interpreting Results and Drawing Conclusions

# 1

# There seems to be some support for H_1 that increasing the minimum wage will increase full-time employment (although they should
# ideally catch that these models do not actually offer information specific to either state in this default form).

# 2 

# Technically, since the models do not estimate the interactions treatment X chain or treatment X wageBefore, attempting to
# answer this question based on the above models is unwise. Again, if they catch this, that's great. If not, we might
# tentatively say that different effect sizes/directions across chains in lmemp are suggestive of some variation, but since none
# of the estimators are even close to significant and they don't estimate the right things, there is *no* support for H_2.

# 3

# It seems that increasing the minimum wage had the effect of increasing full-time employment in NJ. Interestingly, the early
# explorations seem to suggest that Pennsylvania's wages/FT employment suffered in the same time period, but there is not a clear
# mechanism at work linking the NJ minimum wage change to the time-variation in PA data (at least, not here).