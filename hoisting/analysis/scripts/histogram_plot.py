# Import library and dataset
import matplotlib.pyplot as plt
import seaborn as sns
import pandas  as pd
import os

files = [name for name in os.listdir('.') if name.startswith("exprs.histogram")]

frames = []
for filename in files:
    df = pd.read_csv(filename)
    df[2] = int(filename.split('.')[2])
    df.columns = ['type','count','total']
    frames.append(df)

concatenated = pd.concat(frames, ignore_index = True)
concatenated = concatenated.sort_values(by = ['total'])
ax = sns.catplot(x = 'type', y = 'count', hue = 'total', kind = 'bar', data = concatenated)
sns.set()
plt.show()
