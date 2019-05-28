"""
    plot graphs
"""

import para
import matplotlib.pyplot as plt


def count_changes(vals):
    """
        input a list of values and check the trend 
    """
    counter = 0
    num = len(vals)
    for i in range(1, num-1):
        if (vals[i]-vals[i-1])*(vals[i]-vals[i+1])<0:
            counter =  counter + 1
    
    return counter







def plt_od_cost(mp:para.ParaClass(),cases):

    all_od = []
    
    for w in range(0, para.ParaClass.num_od):
        od = []
        x = []
        for s in cases:
            if len(s.od)==0:
                continue
            map_od =[sw for sw in s.od if sw.id == w][0] 
            od.append(map_od.demand*map_od.mincost)
            x.append(s.id)
        # plt.figure("OD "+str(w))
        plt.plot(x, od)
        all_od.append(od)
        # plt.show()
        # tpt.plot(od)



def main(mp:para.ParaClass(), cases):

    plt_od_cost(mp, cases)
    pass