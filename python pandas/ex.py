import pandas as pd

#General utility copy file
df = pd.read_csv('vish_final1.csv', index_col=False)
df2 = pd.read_csv('taps.csv', index_col = False)

list1 = df.district
list2 = df2.District

for i in range(len(list2)):
    for j in range(len(list1)):
        if(list2[i] == list1[j]):
            df.taps[j] = df2.percentage[i]

for i in range(len(list1)):
    print(str(list1[i]) + ": "+ str(list2[i]) +": " + str(df.taps[i]))

df.to_csv('final.csv')