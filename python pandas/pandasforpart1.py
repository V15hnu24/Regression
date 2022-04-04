import pandas as pd


df = pd.read_csv('main.csv', index_col=False)

list1 = df.state
list2 = df.year

# Code or part1 state and gdp
 
for i in range(2011,2020):
    df2 = pd.read_csv(str(i)+'.csv', index_col=False)
    list3 = df2.state
    for j in range(len(list3)):
        for k in range(len(list1)):
            if(list1[k] == list3[j] and list2[k] == i):
                df.gdp[k] = df2.gdp[j]

for i in range(len(list1)):
    print(str(list1[i]) + ": "+ str(list2[i]) +": " + str(df.gdp[i]))

# Code for part2 beds upto 2020

df2 = pd.read_csv('beds.csv', index_col = False)

list1 = df2.state
list2 = df.state
ans = [0]*len(list2)

for i in range(len(list1)):
    for j in range(len(list2)):
        if(list1[i]==list2[j]):
            df.beds[j]=df2.beds[i]

# Final output file
df.to_csv('main_from_pandaspython.csv')