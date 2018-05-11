# lammps-data
  Using single molecule data file build bulk system data file. Inspired by gromcas .itp file and .top.  
  Up to now, single2data.f90 can deal with all-atom and coarse-grain initial data file.  
  
  First, we need get the single molecule data file, define atom/bond/angle/dihedral/impropers information.  
  Second, packing molecules into box size.  
  Last, get the packed configurations data file.  

  Usage: gfortran -o single2data.exe single2data.f90  
        ./single2data.exe < input.inp  
       
  PACKMOL: https://github.com/mcubeg/packmol  
  
  We use packmol packing molecules in defined regions of space.(default box size: 0. 0. 0. 40. 40. 40.)  

  input.inp: defined the variable used in single2data.f90  
  
    water.data       # single molecule data file  
    mixture.pdb      # packmol output file  
    2000             # number of molecules in simulation box  
    7                # atom_style = full(7), atom_style=bond,molecular(6)  
    3                # mask =1   (atoms),mask=2(atoms,bonds),mask=3(atoms,bonds,angles),mask=4(atoms,bonds,angles,dihedrals),mask=5(atoms,bonds,angles,dihedrals,impropers)  
  0. 0. 0. 40. 40. 40.  # box size   
  
  nve.in  
  You can test your data file by nve.in file.  
