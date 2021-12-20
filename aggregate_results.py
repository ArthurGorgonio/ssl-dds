import pandas as pd
import glob
import numpy as np

base_path = 'results/detailed/'


base_name = ['DyDaSL N - ', 'DyDaSL NFT - ', 'DyDaSL W - ', 'DyDaSL H - ']
base_name.sort()
base_batch = [100, 250, 500, 750, 1000, 2500, 5000]
drift_names = ['Drift – N', 'Drift – NFT', 'Drift – W', 'Drift – H']
drift_names.sort()


files = glob.glob(base_path + '*.txt')
data_name = [i.split('/')[-1].split('_')[0][6:] for i in files]
datasets = list(set(data_name))

header = ['DyDaSL N', 'DyDaSL FT', 'DyDaSL W', 'Hinkley', 'Mean N', 'Mean FT', 'Mean W', 'Mean H']


def save_data(path: str, values: list, column_names: list):
    df = pd.DataFrame(values).transpose()
    df.columns = column_names
    df.to_csv(path, sep='\t', header=False)


def cumulative_sum(path: str, values: list, column_names: list):
    data = pd.DataFrame(values).transpose()
    data.columns = column_names
    data['Drift – H'] = np.where(data['Drift – H'], 1, 0)
    data['Drift – N'] = np.where(data['Drift – N'], 1, 0)
    data['Drift – NFT'] = np.where(data['Drift – NFT'], 1, 0)
    data['Drift – W'] = np.where(data['Drift – W'], 1, 0)
    data['H Frequency'] = data['Drift – H'].cumsum() / (sum(data['Drift – H']) if sum(data['Drift – H']) > 0 else 1)
    data['N Frequency'] = data['Drift – N'].cumsum() / (sum(data['Drift – N']) if sum(data['Drift – N']) > 0 else 1)
    data['NFT Frequency'] = data['Drift – NFT'].cumsum() / \
        (sum(data['Drift – NFT']) if sum(data['Drift – NFT']) > 0 else 1)
    data['W Frequency'] = data['Drift – W'].cumsum() / (sum(data['Drift – W']) if sum(data['Drift – W']) > 0 else 1)
    data.to_csv(path, sep='\t', header=False)


approach_name = [i + str(j) for j in base_batch for i in base_name]
step = len(base_name)

for pattern in datasets:
    acc_mean = []
    f1_mean = []
    kappa_mean = []
    drift_count = []
    files = glob.glob(base_path + '*' + pattern + '*.txt')
    data_name = [i.split('/')[-1].split('_')[0][6:] for i in files]

    for batch in base_batch:
        files = glob.glob(base_path + '*' + pattern +
                          '*_' + str(batch) + '.txt')
        files.sort()
        print('\n', files)
        acc_batch = [[0] for _ in range(len(base_name)*2)]
        f1_batch = [[0] for _ in range(len(base_name)*2)]
        kappa_batch = [[0] for _ in range(len(base_name)*2)]
        drift_batch = [[0] for _ in range(len(base_name))]

        for i, file_name in zip(range(len(files)), files):
            print(i, '\tFile: ', file_name)
            data = pd.read_csv(file_name, sep='\s+', skiprows=15)
            acc_mean.append(round(data.describe().iloc[1, 2], 4))
            f1_mean.append(round(data.describe().iloc[1, 3], 4))
            kappa_mean.append(round(data.describe().iloc[1, 4], 4))
            drift_count.append(sum(data.iloc[:, 5]))
            acc_batch[i] = np.array(data.iloc[:, 2])
            f1_batch[i] = np.array(data.iloc[:, 3])
            kappa_batch[i] = np.array(data.iloc[:, 4])
            drift_batch[i] = np.array(data.iloc[:, 5])
            acc_batch[i + step] = np.array(round(
                data.iloc[:, 2].expanding().mean(), 4))
            f1_batch[i + step] = np.array(round(
                data.iloc[:, 3].expanding().mean(), 4))
            kappa_batch[i + step] = np.array(round(
                data.iloc[:, 4].expanding().mean(), 4))
        save_data('plots/acc/' + pattern + '_acc-' +
                  str(batch) + '.csv', acc_batch, header)
        save_data('plots/fscore/' + pattern + '_f1-' +
                  str(batch) + '.csv', f1_batch, header)
        save_data('plots/kappa/' + pattern + '_kappa-' +
                  str(batch) + '.csv', kappa_batch, header)
        cumulative_sum('plots/drift/' + pattern + '_drift-' + str(batch) +
                       '.csv', drift_batch, drift_names)
    print('Acc: ', len(acc_mean), '\nF1S: ', len(f1_mean), '\nKap: ', len(
        kappa_mean), '\nDri: ', len(drift_count), '\nNam: ', len(data_name))
    pd.DataFrame(zip(data_name, acc_mean, f1_mean, kappa_mean, drift_count),
                 columns=['Data', 'Acc', 'F1', 'Kappa', 'Drifts'],
                 index=approach_name).to_csv('plots/medias' + pattern + '.csv')
