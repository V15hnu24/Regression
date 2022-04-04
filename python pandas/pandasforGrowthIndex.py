import pandas as pd

df = pd.read_csv('vish_final1.csv', index_col=False)

def growth_rate(a, b):
 
    result = (((b - a)) / a) 
    return result

def bin_search_first(arr, x, n):
     
    low = 0
    high = n - 1
    res = -1
     
    while (low <= high):
         
        # Normal Binary Search Logic
        mid = (low + high) // 2
         
        if (arr[mid] > x):
            high = mid - 1
        elif (arr[mid] < x):
            low = mid + 1
        else:
            res = mid
            high = mid - 1

    return res


list1 = df.year

for i in range(len(list1)):
    if(df.year[i]==2011):
        continue
    else:
        a = bin_search_first(df.districtlgdcode,df.districtlgdcode[i],len(df.districtlgdcode))
        
        while( a<len(list1) and df.districtlgdcode[a]==df.districtlgdcode[i]):
            if(df.year[a]==df.year[i]-1 and df.season[i]==df.season[a] and df.index[a]!=0):
                df.g_index[i] = growth_rate(df.index[a],df.index[i])
            a = a + 1

df.to_csv("N_vishnu.csv")