# lammps-data
  Using single molecule data file build bulk system data file. Inspired by gromcas .itp file and .top.
  
  water.data     # single molecule data file  
  mixture.pdb    # packmol output file  
  2000           # number of molecules  
  7              # atom_style = full(7), atom_style=bond(6)  
  3              # mask =1     (atoms),mask=2(atoms,bonds),mask=3(atoms,bonds,angles),mask=4(atoms,bonds,angles,dihedrals),mask=5(atoms,bonds,angles,dihedrals,impropers)  
  0. 0. 0. 40. 40. 40. # box size  
