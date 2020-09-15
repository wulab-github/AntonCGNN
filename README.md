# AntonCGNN
This package contains:
all the datasets and source codes that used to analyze the MD simulation trajectories 
generated from Anton2 for MHC/PD-L1 bispecific biologics with all six types of linkers.

The datasets of the structural ensembles with the vector-based representation converted from Anton2 MD simulations are listed as follows:
    1: TwoModuleInterCoord_GS15.dat
    2: TwoModuleInterCoord_GS30.dat
    3: TwoModuleInterCoord_PLP15.dat
    4: TwoModuleInterCoord_PLPII15.dat
    5: TwoModuleInterCoord_PLPII30.dat
    6: TwoModuleInterCoord_PLrigid.dat  
    
The source codes that uses the back-propagation neural network algorithm to classify the dynamics between different linkers are in the Fortran77 format.
They are listed as follows:
    1: LinkerInternalDyn_BRtest_main.f is the main program for neural-network-based classification using the six datasets listed above as inputs
    
    2: sub_BP_learn_v2.f is the subroutine for neural network training process
    
    3: sub_BP_recall_v2.f is the subroutine for the testing after neural netowork training
    
This program also provide the compiled excutable file: BPNNtest

An sample output from the program is also provided:  BPNNLinkerInterDyn_output.dat
It contains the classification results for all pairs of six linkers using the datasets listed above as the input.

The format of this output file are shown as:

 T_sc lk_i lk_j   TP   TN   FP   FN   SS   SP   PC   AC

The second and third columes stand for the linker index:
 1: GS15
 2: GS30
 3: PLP15
 4: PLPII15
 5: PLPII30
 6: PLrigid
 
The 4th colume (TP) is the overall true-positive values calculated from the cross-validation. 
The 5th colume (TN) is the overall true-negative values calculated from the cross-validation.
The 6th colume (FP) is the overall false-positive values calculated from the cross-validation.
The 7th colume (FN) is the overall false-negative values calculated from the cross-validation.
The 8th colume (SS) is the overall sensitivity calculated from the cross-validation.
The 9th colume (SP) is the overall specificity calculated from the cross-validation.
The 10th colume (PC) is the overall precision calculated from the cross-validation.
The 12th colume (AC) is the overall accuracy calculated from the cross-validation.
