import os
import numpy as np
import matplotlib.pyplot as plt

import os.path




rang = np.arange(6, 33, 1).tolist()
# rang = np.arange(30, 33, 1).tolist()

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
    return n - 6

plt.rcParams.update({'figure.max_open_warning': 0})

policies = ('lru', 'plru', 'rplru', 'plrur', 'bip', 'rr', 'srrip', 'brrip')


patterns = [(str(c) + "_" + str(d) + "_" + str(l)) for c in range (1,7) for d in range (1,7) for l in range (1,7) if (d >= l)]

# rows = [("a_" + str(c) + "_1_1" + str(n) + "_b_" + str(c2) + "_1_1" + str(n2)) for c in range (1,7) for c2 in range (1,7) for n in range (1,23) for n2 in range (1,23) if ((c != c2) and (n != n2))]

# repetitions = [1,2]

# rows = [("a_" + str(c) + "_1_1_" + str(n)) for c in range (1,4) for n in range (1,23)]
# cols = [("b_" + str(c) + "_1_1_" + str(n) + "_" + str(rep)) for rep in repetitions for c in range (1,4) for n in range (1,23)]

rows = policies
cols = range(6,30)

directory = "/home/paloma.pedregal/memory-model/adaptive/count/"
filesToPlot = "congruent_adaptive_eviction_test_100it_"
saveDirectory = "/home/paloma.pedregal/memory-model/adaptive/figures/extra_patterns/congruent_eviction_strategies_"

for pol in policies:
    elem = 0
    rowsDataEviction = []
    for c in cols:
        i = numCongruent(c)
        for pat in patterns:
            openDir = directory + filesToPlot + pol + "_count_" + pat
            if (os.path.exists(openDir)):
                with open (openDir + "_hits") as f:
                    rowElemMisses.append (([float('%.3f'%(num_accesses_long(fileToOpen) - (float(x)))) for x in f.read().splitlines()][elem]))
                with open (openDir) as f:
                    rowElemEviction.append ([float('%.3f'%(float(x))) for x in f.read().splitlines()][elem])
            else:
                print(openDir)
                rowElemMisses.append(0.0)
                rowElemEviction.append(0.0)
        rowsDataMisses.append(rowElemMisses)
        rowsDataEviction.append(rowElemEviction)        

    # for i in rang:
    # elem = numCongruent (i)
    # # Number of misses
    # rowsDataMisses = []
    # rowsDataEviction = []
    # for s in rows:
    #     rowElemMisses = []
    #     rowElemEviction = []
    #     for pol in policies:
    #         openDir = directory + filesToPlot +  pol + "_count_" + s
    #         print (openDir)
    #         with open (openDir + "_hits") as f:
    #             rowElemMisses.append (([float('%.3f'%(num_accesses_long(s) - (float(x)))) for x in f.read().splitlines()][elem]))
    #         with open (openDir) as f:
    #             rowElemEviction.append ([float('%.3f'%(float(x))) for x in f.read().splitlines()][elem])
    #     rowsDataMisses.append(rowElemMisses)
    #     rowsDataEviction.append(rowElemEviction) 
        
        
    # vals = [[int(j) for j in i] for i in rowsData]    
    # normal = plt.Normalize(vals.min()-1, vals.max()+1)  
    # saveDir = directory + saveDirectory + str(i)
    saveDir = saveDirectory + pol

    print ("a")
    
    vals = np.around(np.array(rowsDataMisses))
    normal = plt.Normalize(vals.min()-1, vals.max()+1)

    fig, ax = plt.subplots()
    # Hide axes
    ax.xaxis.set_visible(False) 
    ax.yaxis.set_visible(False)
    ax.axis('off')

    clust_data = rowsDataMisses
    myTable = ax.table(cellText=clust_data,rowLabels=rows, colLabels=cols, loc='center', colWidths=[0.2 for x in cols], cellColours=plt.cm.YlGnBu(normal(vals))) ## colormaps: pink, bone, cool, hot
    #plt.title(str(i) + " congruent addresses")
    # plt.show()
    
    for key, cell in myTable.get_celld().items():
        if key in [(0,n) for n in (list(range (0,22)) + list(range(44, 66)) + list(range(88, 110)))] :
            #cell.set_linewidth(2)
            cell.set_facecolor("paleVioletRed")
        elif key in [(0,n) for n in (list(range (22, 44)) + list(range(66,88)) + list(range(110, 132)))] :
            cell.set_facecolor("Gold")
        elif key in [(n,-1) for n in (list(range (1,23)) + list(range(45, 67)))] :
            cell.set_facecolor("paleVioletRed")
        elif key in [(n,-1) for n in (list(range (23, 45)))] :
            cell.set_facecolor("Gold")
        cell.set_height(0.15)

    
    plt.tight_layout()
    fig.savefig(saveDir + "_misses", bbox_inches='tight', dpi = fig.dpi)

    print ("b")
    vals = np.array(rowsDataEviction)
###    normal = plt.Normalize(vals.min()-1, vals.max()+1)
    normal = plt.Normalize(vals.min()+0, vals.max()+1)

    fig, ax = plt.subplots()
    # Hide axes
    ax.xaxis.set_visible(False) 
    ax.yaxis.set_visible(False)
    ax.axis('off')

    clust_data = rowsDataEviction
    myTable = ax.table(cellText=clust_data,rowLabels=rows, colLabels=cols, loc='center', colWidths=[0.2 for x in cols], cellColours=plt.cm.YlGnBu(normal(vals))) ## colormaps: pink, bone, cool, hot
    # plt.title(str(i) + " congruent addresses")
    # plt.show()
    for key, cell in myTable.get_celld().items():
        if key in [(0,n) for n in (list(range (0,22)) + list(range(44, 66)) + list(range(88, 110)))] :
            #cell.set_linewidth(2)
            cell.set_facecolor("paleVioletRed")
        elif key in [(0,n) for n in (list(range (22, 44)) + list(range(66,88)) + list(range(110, 132)))] :
            cell.set_facecolor("Gold")
        elif key in [(n,-1) for n in (list(range (1,23)) + list(range(45, 67)))] :
            cell.set_facecolor("paleVioletRed")
        elif key in [(n,-1) for n in (list(range (23, 45)))] :
            cell.set_facecolor("Gold")
        cell.set_height(0.15)
        
    plt.tight_layout()
    fig.savefig(saveDir, bbox_inches='tight', dpi = fig.dpi)
