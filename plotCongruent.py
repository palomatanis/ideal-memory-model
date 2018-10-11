import os
import numpy as np
import matplotlib.pyplot as plt
import copy
import os.path

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

policies = ('rplru', 'plrur', 'plru', 'lru', 'fifo', 'srrip_hp', 'srrip_fp', 'bip', 'brrip_hp', 'brrip_fp', 'mru', 'rr')

patterns = [(str(c) + "_rep_" + str(d) + "_addresses") for d in range (1,17) for c in range (1,5)]

rows = patterns
cols = policies

directory = "/home/paloma.pedregal/memory-model/adaptive/assoc8/"
filesToPlot = "congruent_deep_eviction_5000it_"
saveDirectory = "/home/paloma.pedregal/memory-model/adaptive/figures/assoc8/congruent_deep_eviction"

rowsDataEviction = []

for n in range(1,17):
    elem = 0
    for rep in range(1,5):
        rowElemEviction = []
        for pol in policies:
            openDir = directory + filesToPlot + pol + "_pattern_" + str(rep) + "_rep_" + str(n) + "_congruent_addresses"
            if (os.path.exists(openDir)):
                with open (openDir) as f:
                    rowElemEviction.append ([float('%.3f'%(float(x))) for x in f.read().splitlines()][elem])
            else:
                print(openDir)
                rowElemEviction.append(0.0)
        rowsDataEviction.append(rowElemEviction)


saveDir = saveDirectory



list(map(list, zip(*rowsDataEviction)))

# vals = np.array(copy.deepcopy(rowsDataEviction))
 

vals = copy.deepcopy(rowsDataEviction)

for i in range(len(vals)):
    for j in range(len(vals[i])):
        if(vals[i][j] < 8):
            vals[i][j] = 0
            
vals = np.array(vals)

normal = plt.Normalize(vals.min()+0, vals.max()+1)

fig, ax = plt.subplots()
# Hide axes
ax.xaxis.set_visible(False) 
ax.yaxis.set_visible(False)
ax.axis('off')

clust_data = rowsDataEviction
myTable = ax.table(cellText=clust_data,rowLabels=rows, colLabels=cols, loc='center', colWidths=[0.1 for x in cols], cellColours=plt.cm.YlGnBu(normal(vals))) ## colormaps: pink, bone, cool, hot

# for key, cell in myTable.get_celld().items():
#     if key in [(0,n) for n in (list(range (0,4)) + list(range(8, 12)) + list(range(16, 20)) + list(range(24, 28)) + list(range(32, 36)) + list(range(40, 44)) + list(range(48, 52)) + list(range(56, 60)))] :
#         #cell.set_linewidth(2)
#         cell.set_facecolor("paleVioletRed")
#     elif key in [(0,n) for n in (list(range (4, 8)) + list(range(12,16)) + list(range(20,24)) + list(range(28,32)) + list(range(36,40)) + list(range(44,48)) + list(range(52, 56)) + list(range(60, 64)))] :
#         cell.set_facecolor("Gold")
#     elif ((key[0] in [1,5,9]) and not (str(cell.get_text())[10] == "8")) :
#         cell.set_facecolor("peachpuff")        
#     elif ((key[0] in [2,6,10]) and not (str(cell.get_text())[10] == "8")) :
#         cell.set_facecolor("thistle")
#     elif ((key[0] in [3,7,11]) and not (str(cell.get_text())[10] == "8")) :
#         cell.set_facecolor("lightgreen")     
#     elif (not (str(cell.get_text())[10] == "8")):
#         cell.set_facecolor("lightsteelblue")
    # elif key in [(n,-1) for n in (list(range (1,23)) + list(range(45, 67)))] :
    #     cell.set_facecolor("paleVioletRed")
    # elif key in [(n,-1) for n in (list(range (23, 45)))] :
    #     cell.set_facecolor("Gold")
    
    # cell.set_height(0.15)  
    
plt.tight_layout()
fig.savefig(saveDir, bbox_inches='tight', dpi = fig.dpi)
