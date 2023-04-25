
# if (percentage of 0's > 60%)
#   Select random row between 1-n
#   if cpue_01 == 0
#       delete row

import pandas as pd

# select random row
df = df.sample()

skate = pd.read_csv("cefas_4.csv")
df = skate.drop(skate[skate['cpue_01'] == 0].sample(frac=0.35).index)

df[df.cpue_01 == 0].count

df.to_csv('dat14_thinned.csv')
(59/67)*100

skate1 = pd.read_csv("dat14_thinned.csv")
skate1[skate1.cpue_01 == 0].count
df1 = skate1.drop(skate1[skate1['cpue_01'] == 0].sample(frac=.7).index)
df1[df1.cpue_01 == 0].count
df1.to_csv('dat1_thinned.csv')