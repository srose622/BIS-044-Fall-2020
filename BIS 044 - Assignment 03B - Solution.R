load("prrace08.rda")
# Set n equal to the number of elements in the name vector
n<-length(prrace08$state)

o_counter <-0
m_counter <-0
# Loop to print out the ages of all values where name="Lucy"

for (i in 1:n) {
  if(prrace08$p_obama[i] >= prrace08$p_mc_cain[i]) {
    o_counter <- o_counter + prrace08$el_votes[i]
  } else {m_counter <- m_counter + prrace08$el_votes[i]}
}
o_counter <- o_counter + 1
m_counter <- m_counter - 1

cat(" Obama:",o_counter,"\n","McCain:",m_counter)

