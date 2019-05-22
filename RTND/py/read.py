"""
    read link and od class
"""

import myclass as mc
import para
import pandas as pd


def rd_link_sol(pa,nwk:mc.NwkClass):
    """
        read link
    """
    file = pa.output_folder +'\\fortran_output_link.txt'
    df = pd.read_csv(file)
    num_row = df.shape[0]
    print(df)
    sols = []
    for i in range(num_row):
        s = mc.SolClass()
        sols.append(s)
        sols[-1].id  = i
    id = 0

    for i in range(num_row):
        id = df["link"][i]
        sols[i].link = nwk.links[id]
        nwk.links[id].tail = df["tail"][i]
        nwk.links[id].head = df["head"][i]
        nwk.links[id].cost =  df["lt"][i]
        nwk.links[id].flow = df["flow"][i]
        sols[i].x = df["xprob"][i]
        sols[i].fx = df["fx"][i]
        sols[i].logit = df["logitprob"][i]
        sols[i].dest = df["dest"][i]
        # print(nwk.links[id].__dict__)
        print(sols[i].link.__dict__)
    return sols


    pass

def rd_node(pa,nwk:mc.NwkClass):
    file = pa.output_folder +'\\fortran_output_node.txt'
    df = pd.read_csv(file)
    num_row = df.shape[0]
    for i in range(num_row):
        id = df["node"][i]
        dest = df["dest"][i]
        nwk.nodes[id].label[dest] = df["label"][i]
        fout = df["fout"][i]
        lout = df["lout"][i]
        for l in range(fout,lout+1):
            nwk.nodes[id].outlinks.append(nwk.links[l])
        print("node = {0},numlinks = {1}".format(id,len(nwk.nodes[id].outlinks)))
        # print(nwk.nodes[id].__dict__)
    print(df)


def pr_nwk(pa:para.ParaClass,nwk:mc.NwkClass,sols):
    """
        print network after read
    """
    file = pa.output_folder + '\\nwk.txt'
    with open(file,'w') as f:
        print("node,link,head,cost,headlabel,pie,fx,x,logitprob,dest")
        for nr in nwk.dest:
            for nn in nwk.nodes:
                nid = nn.id
                if nid == 0:
                    continue
                vc = [x for x in sols if x.dest ==nr and x.link.tail == nid]
                for s in vc:
                    hn = s.link.head
                    hl = nwk.nodes[hn].label[nr] 
                    print("{0},{1},{2},{3},{4},{5},{6},{7},{8},{9}".format(
                        nid,s.link.id,s.link.head, s.link.cost,
                        hl,s.link.cost+hl, s.fx,s.x,s.logit,nr))

def main(mypara:para.ParaClass,nwk:mc.NwkClass):
    
    sols = rd_link_sol(mypara,nwk)
    rd_node(mypara,nwk)
    # print(mypara.input_folder)
    pr_nwk(mypara,nwk,sols)
