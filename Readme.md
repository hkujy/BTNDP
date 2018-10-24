# Check folder "BaseInputOuput" for the original data setting


# How to use 
1. In the script, there is an indicator " test_fleet", set it to be false for the normal tests

# remark
1. i decide to go back the origin network 
because if i use the Part B network, there is only one trend in the changes of
OD pair 2 cost, which is not that interesting 

2. the section in the graph is different from the section in the program
    - for the network map refer to excel file "MapSection"

3. this version dose not consider the issue of company profit
    - since the focus bi-objective and solution algorithm

# Branch - revise 
1. only use the projection method, so the sam subroutino delelte
2. however, the parameters may still need to be tuned

# Code Remarks 
## Notations
1. SLC: section line count: number of lines in one section
2. SL: section line
> SL(i,k) the kth line in secion i
3. AN:/ BN: Section A node / section B node

## Readdata funciton 
1. seems i have two types of links. one is section, and the other it recreated links

# File index
7.	OPEN(7,FILE='..\RESULTS\DPCONVERGE.TXT')
