import pandas as pd
import numpy as np

data = pd.read_csv("ExcerciseData.txt", header=None,sep="\t" )
print(data)
combination = data[[0,1]]
print(combination)

#combination1 = combination.drop_duplicates()

combination1 =pd.Series(np.unique(data[[0,1]].values.ravel()))
#combination1['new_col'] = range(1, len(combination1) + 1)

combination1 =range(1, len(combination1) + 1)

print(combination1)
print(len(combination))
print(len(combination1))