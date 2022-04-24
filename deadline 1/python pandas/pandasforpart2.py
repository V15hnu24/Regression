import pandas as pd

df = pd.read_csv('main.csv', index_col=False)
df2 = pd.read_csv('beds.csv', index_col = False)

list1 = df2.state
list2 = df.state
ans = [0]*len(list2)

for i in range(len(list1)):
    for j in range(len(list2)):
        if(list1[i]==list2[j]):
            df.beds[j]=df2.beds[i]

df.to_csv('main_from_pandaspython.csv')