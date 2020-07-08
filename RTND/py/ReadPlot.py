import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns
import statistics

def get_one_pareto(_file):
    """
        pass
    """
    df = pd.read_csv(_file,header=None).drop_duplicates()
    # print (df)
    return df

def get_one_cpu(_file):
    """
        get cpu data
    """
    df = pd.read_csv(_file)

    return(statistics.mean(df["CpuTime"]))


if __name__ == "__main__":

    txt_name = "/all_seed_pareto.txt"

    data = []
    for i in range(0,9):
        filename = "M:\BTNDP_Results/Test_SiouxFall/Summary/" +str((i+1)*10)+txt_name
        data.append(get_one_pareto(filename))
    
    # for d in data:
        # print(d)

    cpudata = []
    for i in range(0,9):
        # filename = "M:\BTNDP_Results/Test_SiouxFall/Summary/NoWriteArc_" +str((i+1)*10)+"//cpu_abc.txt"
        filename = "M:\BTNDP_Results/Test_SiouxFall/Summary/" +str((i+1)*10)+"/cpu_abc.txt"
        cpudata.append(get_one_cpu(filename)) 

    f, ax = plt.subplots(ncols=3, nrows=3, figsize=(9, 7.5))

    for i in range(0,3):
        for j in range(0,3):
            dataindex = i*3+j
            sns.scatterplot(data[dataindex][0],data[dataindex][1],ax=ax[i,j])
            # ax[i, j].set_title(str(data[dataindex].shape[0]))
            # ax[i,j].set_xlabel("$\epsilon="+str((dataindex+1)*10)+"$")
            xtick = ax[i,j].get_xticks()
            ytick = ax[i,j].get_yticks()
            xmajorFormatter = plt.FormatStrFormatter('%.0f')
            ymajorFormatter = plt.FormatStrFormatter('%.1f')
            ax[i,j].set_xticklabels(xtick, fontsize=8,fontname='Times New Roman')
            ax[i,j].set_yticklabels(ytick, fontsize=8,fontname='Times New Roman')
            ax[i,j].yaxis.set_major_formatter(ymajorFormatter)
            ax[i,j].xaxis.set_major_formatter(xmajorFormatter)
            ax[i,j].set_title("$\mathit{\epsilon}^{eff}={\epsilon}^{fair}=$"+str((dataindex+1)*10), fontsize = 9,fontname='Times New Roman')
            txt1 = "No.of Points $=$ "+str(data[dataindex].shape[0])
            txt2 = "Cpu Time $=$ "+'{:.1f}'.format(cpudata[dataindex])+" seconds"
            ax[i,j].text(538000,-0.18+0.03,txt1,fontsize=9,fontname ='Times New Roman')
            ax[i,j].text(538000,-0.205+0.03,txt2,fontsize=9,fontname = 'Times New Roman')

            ax[i,j].set_ylabel("")
            ax[i,j].set_xlabel("")
    f.text(0.5, 0, 'Total Effective Travel Cost', ha='center',fontname='Times New Roman',fontsize=9)
    f.text(0, 0.5, 'Fairness Objective', va='center', rotation='vertical',fontname='Times New Roman',fontsize=9)
    plt.tight_layout()
    plt.savefig('AllPareto.eps',dpi=600,format='eps')
    plt.savefig('AllPareto.png',dpi=600)
    plt.show()
    # ax[0, 0].set_title("Linear")
    # ax[0, 1].plot(X, np.log(X))
    # ax[0, 1].set_title("Log")
    # ax[1, 0].plot(X, np.exp(X))
    # ax[1, 0].set_title("Exp")
    # ax[1, 1].plot(X, np.sin(X))
    # ax[1, 1].set_title("Sin")





