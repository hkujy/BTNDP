"""
    read link and od class
"""

import myclass as mc
import para
import pandas as pd

def rd_get_fair_obj(cs):
    """
    cs: cases list
    bid: base case id
    """
    for c in cs:
        if c.id!=mc.CaseClass.base_case_id:
            improve = []
            for w in c.od:
                improve.append(cs[mc.CaseClass.base_case_id].od[w.id].mincost-w.mincost)
                # c.fair=min(c.fair, w.mincost-cs[mc.CaseClass.base_case_id].od[w.id].mincost )
            c.fair = min(improve)
        else:
            c.fair=0
        pass

def rd_link_sol(pa,cases):
    """
        read link
    """
    file = pa.output_folder +'\\fortran_output_link.txt'
    df = pd.read_csv(file)
    num_row = df.shape[0]
    # print(df)
    sols = []
    for i in range(num_row):
        s = mc.SolClass()
        sols.append(s)
        sols[-1].id  = i
    id = 0
    
    now = 0
    solid = 0
    for i in range(num_row):
        cid = df["case"][i]
        id = df["link"][i]
        if cid != now:
            now = cid
            solid = 0
        cases[cid].sols.append(mc.SolClass())
        cases[cid].nwk.links[id].tail = df["tail"][i]
        cases[cid].nwk.links[id].head = df["head"][i]
        cases[cid].nwk.links[id].cost =  df["lt"][i]
        cases[cid].nwk.links[id].flow = df["flow"][i]
        cases[cid].sols[-1].id = solid
        cases[cid].sols[-1].link = cases[cid].nwk.links[id]
        cases[cid].sols[-1].x = df["xprob"][i]
        cases[cid].sols[-1].fx = df["fx"][i]
        cases[cid].sols[-1].logit = df["logitprob"][i]
        cases[cid].sols[-1].dest = df["dest"][i]
        # print(cases[cid].__dict__)
        # print(cases[cid].__dict__)
        solid = solid + 1
    pass

def rd_node(pa,cases):
    file = pa.output_folder +'\\fortran_output_node.txt'
    df = pd.read_csv(file)
    num_row = df.shape[0]
    for i in range(num_row):
        cid = df["case"][i]
        nid = df["node"][i]
        dest = df["dest"][i]
        cases[cid].nwk.nodes[nid-1].label[dest] = df["label"][i]
        fout = df["fout"][i]
        lout = df["lout"][i]
        for l in range(fout,lout+1):
            cases[cid].nwk.nodes[nid].outlinks.append(cases[cid].nwk.links[l])
        # print("node = {0},numlinks = {1}".format(id,len(cases[cid].nwk.nodes[nid].outlinks)))
        # print(nwk.nodes[id].__dict__)
    # print(df)

def pr_nwk(pa:para.ParaClass,nwk:mc.NwkClass,sols):
    """
        print network after read
    """
    file = pa.output_folder + '\\cases.txt'
    with open(file,'a') as f:
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
                        hl,s.link.cost+hl, s.fx,s.x,s.logit,nr),file = f)


def rd_od(pa,cases):
    file = pa.output_folder +'\\fortran_output_od.txt'
    df = pd.read_csv(file)
    num_row = df.shape[0]
    now = 0
    wid = 0
    for i in range(0, num_row):
        cid = df["case"][i]
        if cid != now:
            wid = 0 
            now = cid
        cases[cid].od.append(mc.ODClass())
        cases[cid].od[-1].id = wid
        cases[cid].od[-1].origin = df["origin"][i]
        cases[cid].od[-1].dest = df["dest"][i]
        cases[cid].od[-1].demand = df["demand"][i]
        cases[cid].od[-1].mincost = df["y"][i]
        wid = wid + 1
        cases[cid].od[-1].process

def main(mypara:para.ParaClass,cases):
    
    rd_link_sol(mypara,cases)
    rd_node(mypara,cases)
    rd_od(mypara,cases)
    rd_get_fair_obj(cases)
    # print(mypara.input_folder)
    file = mypara.output_folder + '\\cases.txt'
    with open(file,'w') as f:
        print("node,link,head,cost,headlabel,pie,fx,x,logitprob,dest",file=f)
    for c in cases:
        pr_nwk(mypara,c.nwk,c.sols)
        print("caseid = {0}, fair = {1}".format(c.id,c.fair))