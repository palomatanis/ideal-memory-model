import os
import numpy as np
import matplotlib.pyplot as plt
import copy
import os.path

directory = "./adaptive/srrip_initial/"
filesToPlot = "_tests_initial_states_traces_"

my_list = os.listdir(directory)

files = [x.split(filesToPlot, 1)[1] for x in my_list if (filesToPlot in x)]

print (files)

def num_accesses (eviction_strategy):
    c = int(eviction_strategy[0])
    d = int(eviction_strategy[2])
    l = int(eviction_strategy[4])
    if (c*d*l == 0):
        return [0 for x in rang]
    else:
        return [(x//l)*c*d if x else 0 for x in rang]

def num_accesses_long (eviction_strategy):
    thelist = eviction_strategy.split('_')
    c1 = thelist[1]
    d1 = thelist[2]
    l1 = thelist[3]
    n1 = thelist[4]
#    l1, n1 = thelist[3][0], thelist[3][1:]
    c2 = thelist[6]
    d2 = thelist[7]
    l2 = thelist[8]
    n2 = thelist[9]
    return (num_accesses (c1 + "_" +  d1 + "_" + l1))[int(n1) - 1 ] + (num_accesses (c2 + "_" +  d2 + "_" + l2))[int(n2) - 1]

def numCongruent (n):
    return n - 1

plt.rcParams.update({'figure.max_open_warning': 0})

rows = files
# policies = ["srrip_hp", "srrip_fp", "brrip_hp", "brrip_fp", "lru", "bip", "plru"]
policies = ["srrip_hp", "lru", "bip", "plru"]
cols = [[(x + " eviction"), (x + " max misses")] for x in policies]
cols = [item for sublist in cols for item in sublist] # flatten list


saveDirectory = "./adaptive/figures/srrip_initial/all_initial_traces"

colEviction = []
colMisses = []

allData = []

for fl in files:
    dataRow = []
    for pol in policies:
        openDir = directory + pol + filesToPlot + fl
        if (os.path.exists(openDir)):
            with open (openDir) as f:
                line = f.readline()[:-1]
                dataRow.append (line.split(None, 1)[0])
                dataRow.append (line.split(None, 1)[1])
        else:
            print(openDir)
            dataRow.append(0)
            dataRow.append(0)
    allData.append(dataRow)
    
saveDir = saveDirectory


# vals = np.array(copy.deepcopy(allData)).astype(np.float)
 
vals = copy.deepcopy(allData)
for i in range(len(vals)):
    for j in range(len(vals[i])):
        if(int(vals[i][j]) < 256):
            vals[i][j] = 0
vals = np.array(vals).astype(np.float)

normal = plt.Normalize(vals.min()+0, vals.max()+1)

fig, ax = plt.subplots()
# Hide axes
ax.xaxis.set_visible(False) 
ax.yaxis.set_visible(False)
ax.axis('off')

clust_data = allData

myTable = ax.table(cellText=clust_data,rowLabels=rows, colLabels=cols, loc='center', colWidths=[0.2 for x in cols], cellColours=plt.cm.YlGnBu(normal(vals))) ## colormaps: pink, bone, cool, hot

for key, cell in myTable.get_celld().items():
    cell.set_height(0.1)  
    
plt.tight_layout()
fig.savefig(saveDir, bbox_inches='tight', dpi = fig.dpi)
