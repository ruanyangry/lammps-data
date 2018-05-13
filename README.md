# lammps-data
  Using single molecule data file build bulk system data file. Inspired by gromcas .itp file and .top.  
  
  Updata 1.0: single2data.f90 read can fixed format data file and deal with all-atom and coarse-grain initial data file.  
  
  Update 2.0: can read free format .data file and get the pair/bond/angle/dihedral/improprts coefficients.  
  Default force field amber:
  
    bond_style harmonic
    angle_style harmonic
    dihedral_style harmonic
    improper_style harmonic
  
  Input file:  
   1) The single molecule data file, define atom/bond/angle/dihedral/impropers information.  
   2) packing molecules into box size.  
   3) get the packed configurations data file.  

  Usage: 
  
     gfortran -o single2data.exe single2data.f90  
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
    on           # coeffes = on, represet pair/bond/angle/dihedral/improprts coefficients defined in single molecule data fiel.
  
  nve.in : lammps input file, used to test output chain.data files.
  
    lmp_mpi < nve.in
