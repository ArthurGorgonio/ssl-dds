import pandas as pd

data = pd.read_csv("avg-acc-ug.dat", sep='\t')
data.head

data.columns

data['N Frequency'] = data['Drift – N'].cumsum() / sum(data['Drift – N'])
data['FT Frequency'] = data['Drift – FT'].cumsum() / sum(data['Drift – FT'])
data['W Frequency'] = data['Drift – W'].cumsum() / sum(data['Drift – W'])
data.head()

data.to_csv('data-ug2c3d-cumfeq.dat', sep='\t', index=False)
