The script designed for modelling internal tides on the Malin Shelf.
An extra block is included for calculation tidally induced residual currents.
The package includes two Fortran scripts: Input-Shelf.f90 generates initial fields. Tide-Main-Shelf.f90 is the main code for calculation internal tides. 
The input model parameters are set in thhre files: PPARM1BEAM.DAT, PPARM2BEAM.DAT, and PPARM.BEAM.DAT
For running the model the input parameters should be copiet it the folder INPUT;
For input files a separate directory INPUT.DAT should be organized.
Output files are copied in the OUTPUT.DAT directory
