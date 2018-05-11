  program single2data
    implicit none
    integer i,j,k,l
    integer tatoms,tbonds,tangles,tdihedrals,timpropers
    integer atomtypes,bondtypes,angletypes,dihedraltypes,impropertypes
    integer serial
    real(kind=8) xlo,xhi,ylo,yhi,zlo,zhi
    real(kind=8),allocatable::mass(:,:),atoms(:,:),bonds(:,:),angles(:,:),dihedrals(:,:),impropers(:,:)
    character*80 filename,pdbfile,CRYST,ATOM,resname,atomname,resid,string
    integer nchain,columns,mask
    real(kind=8),allocatable::box(:),pdbcoord(:,:)
    allocate(box(6))
    box=0.0
    
    open(unit=10,file='input.inp',status='old')
    read(10,*)filename
    read(10,*)pdbfile
    read(10,*)nchain
    read(10,*)columns
	read(10,*)mask
	read(10,*)box(:)
    close(10)
	
	write(*,*)'#------------------------------------------------------------------#'
	write(*,*)'#                        Author Ruan Yang                          #'
	write(*,*)'#                  Email: ruanyang_njut@163.com                    #'
	write(*,*)'#        Compile: gfortran -o single2data.exe single2data.f90      #'
	write(*,*)'#                    ./single2data.exe < input.inp                 #'
	write(*,*)'#------------------------------------------------------------------#'
  
! Get information from single chain

    open(unit=12,file=filename)
    
    read(12,*)
    read(12,*)
    
	if(mask.eq.(1))then
    read(12,1010)tatoms
	end if
	if(mask.eq.(2))then
	read(12,1010)tatoms
	read(12,1010)tbonds
	end if
	if(mask.eq.(3))then
	read(12,1010)tatoms
	read(12,1010)tbonds
	read(12,1010)tangles
	end if
	if(mask.eq.(4))then
	read(12,1010)tatoms
	read(12,1010)tbonds
	read(12,1010)tangles
	read(12,1010)tdihedrals
	end if
	if(mask.eq.(5))then
	read(12,1010)tatoms
	read(12,1010)tbonds
	read(12,1010)tangles
	read(12,1010)tdihedrals
	read(12,1010)timpropers
	end if
	
	read(12,*)
	
	if(mask.eq.(1))then
	read(12,1010)atomtypes
	end if
	if(mask.eq.(2))then
	read(12,1010)atomtypes
	read(12,1010)bondtypes
	end if
	if(mask.eq.(3))then
	read(12,1010)atomtypes
	read(12,1010)bondtypes
	read(12,1010)angletypes
	end if
	if(mask.eq.(4))then
	read(12,1010)atomtypes
	read(12,1010)bondtypes
	read(12,1010)angletypes
	read(12,1010)dihedraltypes
	end if
	if(mask.eq.(5))then
	read(12,1010)atomtypes
	read(12,1010)bondtypes
	read(12,1010)angletypes
	read(12,1010)dihedraltypes
	read(12,1010)impropertypes
	end if
	
	allocate(mass(atomtypes,2))
	if(mask.eq.(1))then
	allocate(atoms(tatoms,columns))
	end if
	if(mask.eq.(2))then
	allocate(atoms(tatoms,columns))
	allocate(bonds(tbonds,4))
	end if
	if(mask.eq.(3))then
	allocate(atoms(tatoms,columns))
	allocate(bonds(tbonds,4))
	allocate(angles(tangles,5))
	end if
	if(mask.eq.(4))then
	allocate(atoms(tatoms,columns))
	allocate(bonds(tbonds,4))
	allocate(angles(tangles,5))
	allocate(dihedrals(tdihedrals,6))
	end if
	if(mask.eq.(5))then
	allocate(atoms(tatoms,columns))
	allocate(bonds(tbonds,4))
	allocate(angles(tangles,5))
	allocate(dihedrals(tdihedrals,6))
	allocate(impropers(timpropers,6))
	end if
	
	read(12,*)
	
	read(12,1011)xlo,xhi
	read(12,1011)ylo,yhi
	read(12,1011)zlo,zhi
	
	read(12,*)
	
	read(12,*)
	read(12,*)
	
1010 format(I12)
1011 format(2f14.3)

! mass

    do i=1,atomtypes
	    read(12,*)mass(i,:)
	end do
	
	read(12,*)
	read(12,*)
	read(12,*)
	
! atoms
    if(mask.eq.(1))then
    do i=1,tatoms
	  read(12,*)atoms(i,:)
	end do
	end if
	
	if(mask.eq.(2))then
	
! atoms

	do i=1,tatoms
	  read(12,*)atoms(i,:)
	end do
	
! bonds

    read(12,*)
	read(12,*)
	read(12,*)
	
	do i=1,tbonds
	    read(12,*)bonds(i,:)
	end do
	
	end if
	
	if(mask.eq.(3))then
	
! atoms

	do i=1,tatoms
	  read(12,*)atoms(i,:)
	end do
	
! bonds

    read(12,*)
	read(12,*)
	read(12,*)
	
	do i=1,tbonds
	    read(12,*)bonds(i,:)
	end do
	
! angles

    read(12,*)
	read(12,*)
	read(12,*)
	
	do i=1,tangles
	    read(12,*)angles(i,:)
	end do
	
	end if
	
	if(mask.eq.(4))then
	
! atoms

	do i=1,tatoms
	  read(12,*)atoms(i,:)
	end do
	
! bonds

    read(12,*)
	read(12,*)
	read(12,*)
	
	do i=1,tbonds
	    read(12,*)bonds(i,:)
	end do
	
! angles

    read(12,*)
	read(12,*)
	read(12,*)
	
	do i=1,tangles
	    read(12,*)angles(i,:)
	end do
	
! dihedrals

    read(12,*)
	read(12,*)
	read(12,*)
	
	do i=1,tdihedrals
	    read(12,*)dihedrals(i,:)
	end do
	
	end if
	
	if(mask.eq.(5))then
	
! atoms

	do i=1,tatoms
	  read(12,*)atoms(i,:)
	end do
	
! bonds

    read(12,*)
	read(12,*)
	read(12,*)
	
	do i=1,tbonds
	    read(12,*)bonds(i,:)
	end do
	
! angles

    read(12,*)
	read(12,*)
	read(12,*)
	
	do i=1,tangles
	    read(12,*)angles(i,:)
	end do
	
! dihedrals

    read(12,*)
	read(12,*)
	read(12,*)
	
	do i=1,tdihedrals
	    read(12,*)dihedrals(i,:)
	end do
	
! impropers

    read(12,*)
	read(12,*)
	read(12,*)
	
	do i=1,timpropers
	    read(12,*)impropers(i,:)
	end do
	
	end if
	
	close(12)
	
! Read packed pdb file get coordination and box size
    allocate(pdbcoord(tatoms*nchain,3))
	pdbcoord=0.0
	
	open(unit=13,file=pdbfile,status='old')
	
! Take care about the pdb file format
     read(13,*)
	 read(13,*)
     read(13,*)
	 read(13,*)
	 read(13,*)
	 
	 do i=1,tatoms*nchain
	    read(13,1013)string,pdbcoord(i,:)
	 end do
	 close(13)
1013 format(A30,3f8.3)
	 
! Write out packed data file

     open(unit=11,file='chain.data')
	
	 write(11,*)'This file generated by code single2data.f90. Author Ruan Yang'
	 write(11,*)
	 
	 if(mask.eq.(1))then
	 write(11,1001)tatoms*nchain,'atoms'
	 end if
	 if(mask.eq.(2))then
	 write(11,1001)tatoms*nchain,'atoms'
	 write(11,1001)tbonds*nchain,'bonds'
	 end if
	 if(mask.eq.(3))then
	 write(11,1001)tatoms*nchain,'atoms'
	 write(11,1001)tbonds*nchain,'bonds'
	 write(11,1001)tangles*nchain,'angles'
	 end if
	 if(mask.eq.(4))then
	 write(11,1001)tatoms*nchain,'atoms'
	 write(11,1001)tbonds*nchain,'bonds'
	 write(11,1001)tangles*nchain,'angles'
	 write(11,1001)tdihedrals*nchain,'dihedrals'
	 end if
	 if(mask.eq.(5))then
	 write(11,1001)tatoms*nchain,'atoms'
	 write(11,1001)tbonds*nchain,'bonds'
	 write(11,1001)tangles*nchain,'angles'
	 write(11,1001)tdihedrals*nchain,'dihedrals'
	 write(11,1001)timpropers*nchain,'impropers'
	 end if
	 
	 write(11,*)
	 
	 if(mask.eq.(1))then
	 write(11,1001)atomtypes,'atom types'
	 end if
	 if(mask.eq.(2))then
	 write(11,1001)atomtypes,'atom types'
	 write(11,1001)bondtypes,'bond types'
	 end if
	 if(mask.eq.(3))then
	 write(11,1001)atomtypes,'atom types'
	 write(11,1001)bondtypes,'bond types'
	 write(11,1001)angletypes,'angle types'
	 end if
	 if(mask.eq.(4))then
	 write(11,1001)atomtypes,'atom types'
	 write(11,1001)bondtypes,'bond types'
	 write(11,1001)angletypes,'angle types'
	 write(11,1001)dihedraltypes,'dihedral types'
	 end if
	 if(mask.eq.(5))then
	 write(11,1001)atomtypes,'atom types'
	 write(11,1001)bondtypes,'bond types'
	 write(11,1001)angletypes,'angle types'
	 write(11,1001)dihedraltypes,'dihedral types'
	 write(11,1001)impropertypes,'improper types'
	 end if
	 
	 write(11,*)
	 
	 xlo=box(1)
	 ylo=box(2)
	 zlo=box(3)
	 xhi=box(4)
	 yhi=box(5)
	 zhi=box(6)
	 write(11,1002)xlo,xhi,'xlo xhi'
	 write(11,1002)ylo,yhi,'ylo yhi'
	 write(11,1002)zlo,zhi,'zlo zhi'
	 
	 write(11,*)
	 
	 write(11,*)'Masses'
	 write(11,*)
	 
	 do i=1,atomtypes
	    write(11,1003)int(mass(i,1)),mass(i,2)
	 end do
	 write(11,*)
	 
	 if(mask.eq.(1))then
	 write(11,*)'Atoms'
	 write(11,*)
	 
	 if(columns.eq.(7))then
	 do i=1,nchain
	    do j=1,tatoms
		    write(11,1004)int(atoms(j,1))+(i-1)*tatoms,int(atoms(j,2))+(i-1),int(atoms(j,3)),&
			atoms(j,4),pdbcoord(j+(i-1)*tatoms,:)
		end do
	 end do
	 write(11,*)
	 end if
	 if(columns.eq.(6))then
	 do i=1,nchain
	    do j=1,tatoms
		    write(11,1004)int(atoms(j,1))+(i-1)*tatoms,int(atoms(j,2))+(i-1),int(atoms(j,3)),&
			pdbcoord(j+(i-1)*tatoms,:)
		end do
	 end do
	 end if
	 end if
	 
	 if(mask.eq.(2))then
	 write(11,*)'Atoms'
	 write(11,*)
	 
	 if(columns.eq.(7))then
	 do i=1,nchain
	    do j=1,tatoms
		    write(11,1004)int(atoms(j,1))+(i-1)*tatoms,int(atoms(j,2))+(i-1),int(atoms(j,3)),&
			atoms(j,4),pdbcoord(j+(i-1)*tatoms,:)
		end do
	 end do
	 write(11,*)
	 end if
	 if(columns.eq.(6))then
	 do i=1,nchain
	    do j=1,tatoms
		    write(11,1004)int(atoms(j,1))+(i-1)*tatoms,int(atoms(j,2))+(i-1),int(atoms(j,3)),&
			pdbcoord(j+(i-1)*tatoms,:)
		end do
	 end do
	 write(11,*)
	 end if
	 
	 write(11,*)'Bonds'
	 write(11,*)
	 
	 do i=1,nchain
	    do j=1,tbonds
		    write(11,1005)int(bonds(j,1))+(i-1)*tbonds,int(bonds(j,2)),int(bonds(j,3:))+(i-1)*tatoms
		end do
	 end do
	 write(11,*)
	 end if
	 
	 if(mask.eq.(3))then
	 write(11,*)'Atoms'
	 write(11,*)
	 
	 if(columns.eq.(7))then
	 do i=1,nchain
	    do j=1,tatoms
		    write(11,1004)int(atoms(j,1))+(i-1)*tatoms,int(atoms(j,2))+(i-1),int(atoms(j,3)),&
			atoms(j,4),pdbcoord(j+(i-1)*tatoms,:)
		end do
	 end do
	 write(11,*)
	 end if
	 
	 if(columns.eq.(6))then
	 do i=1,nchain
	    do j=1,tatoms
		    write(11,1004)int(atoms(j,1))+(i-1)*tatoms,int(atoms(j,2))+(i-1),int(atoms(j,3)),&
			pdbcoord(j+(i-1)*tatoms,:)
		end do
	 end do
	 write(11,*)
	 end if
	 
	 write(11,*)'Bonds'
	 write(11,*)
	 
	 do i=1,nchain
	    do j=1,tbonds
		    write(11,1005)int(bonds(j,1))+(i-1)*tbonds,int(bonds(j,2)),int(bonds(j,3:))+(i-1)*tatoms
		end do
	 end do
	 write(11,*)
	 
	 write(11,*)'Angles'
	 write(11,*)
			
	 do i=1,nchain
	    do j=1,tangles
		    write(11,1006)int(angles(j,1))+(i-1)*tangles,int(angles(j,2)),int(angles(j,3:))+(i-1)*tatoms
		end do
	 end do
	 write(11,*)
	 end if
	 
	 if(mask.eq.(4))then
	 write(11,*)'Atoms'
	 write(11,*)
	 
	 if(columns.eq.(7))then
	 do i=1,nchain
	    do j=1,tatoms
		    write(11,1004)int(atoms(j,1))+(i-1)*tatoms,int(atoms(j,2))+(i-1),int(atoms(j,3)),&
			atoms(j,4),pdbcoord(j+(i-1)*tatoms,:)
		end do
	 end do
	 write(11,*)
	 end if
	 
	 if(columns.eq.(6))then
	 do i=1,nchain
	    do j=1,tatoms
		    write(11,1004)int(atoms(j,1))+(i-1)*tatoms,int(atoms(j,2))+(i-1),int(atoms(j,3)),&
			pdbcoord(j+(i-1)*tatoms,:)
		end do
	 end do
	 write(11,*)
	 end if
	 
	 write(11,*)'Bonds'
	 write(11,*)
	 
	 do i=1,nchain
	    do j=1,tbonds
		    write(11,1005)int(bonds(j,1))+(i-1)*tbonds,int(bonds(j,2)),int(bonds(j,3:))+(i-1)*tatoms
		end do
	 end do
	 write(11,*)
	 
	 write(11,*)'Angles'
	 write(11,*)
			
	 do i=1,nchain
	    do j=1,tangles
		    write(11,1006)int(angles(j,1))+(i-1)*tangles,int(angles(j,2)),int(angles(j,3:))+(i-1)*tatoms
		end do
	 end do
	 write(11,*)
	 
	 write(11,*)'Dihedrals'
	 write(11,*)
	 
	 do i=1,nchain
	    do j=1,tdihedrals
		    write(11,1007)int(dihedrals(j,1))+(i-1)*tdihedrals,int(dihedrals(j,2)),int(dihedrals(j,3:))+(i-1)*tatoms
		end do
	end do
	write(11,*)
	end if
	
	if(mask.eq.(5))then
	write(11,*)'Atoms'
	 write(11,*)
	 
	 if(columns.eq.(7))then
	 do i=1,nchain
	    do j=1,tatoms
		    write(11,1004)int(atoms(j,1))+(i-1)*tatoms,int(atoms(j,2))+(i-1),int(atoms(j,3)),&
			atoms(j,4),pdbcoord(j+(i-1)*tatoms,:)
		end do
	 end do
	 write(11,*)
	 end if
	 
	 if(columns.eq.(6))then
	 do i=1,nchain
	    do j=1,tatoms
		    write(11,1004)int(atoms(j,1))+(i-1)*tatoms,int(atoms(j,2))+(i-1),int(atoms(j,3)),&
			pdbcoord(j+(i-1)*tatoms,:)
		end do
	 end do
	 write(11,*)
	 end if
	 
	 write(11,*)'Bonds'
	 write(11,*)
	 
	 do i=1,nchain
	    do j=1,tbonds
		    write(11,1005)int(bonds(j,1))+(i-1)*tbonds,int(bonds(j,2)),int(bonds(j,3:))+(i-1)*tatoms
		end do
	 end do
	 write(11,*)
	 
	 write(11,*)'Angles'
	 write(11,*)
			
	 do i=1,nchain
	    do j=1,tangles
		    write(11,1006)int(angles(j,1))+(i-1)*tangles,int(angles(j,2)),int(angles(j,3:))+(i-1)*tatoms
		end do
	 end do
	 write(11,*)
	 
	 write(11,*)'Dihedrals'
	 write(11,*)
	 
	 do i=1,nchain
	    do j=1,tdihedrals
		    write(11,1007)int(dihedrals(j,1))+(i-1)*tdihedrals,int(dihedrals(j,2)),int(dihedrals(j,3:))+(i-1)*tatoms
		end do
	end do
	write(11,*)
	
	write(11,*)'Impropers'
	write(11,*)
	
	do i=1,nchain
	    do j=1,timpropers
		    write(11,1008)int(impropers(j,1))+(i-1)*timpropers,int(impropers(j,2)),int(impropers(j,3:))+(i-1)*tatoms
		end do
	end do
	write(11,*)
	end if
	
	close(11)
	
1001 format(I12,A16)
1002 format(2f14.3,A8)
1003 format(I8,f12.5)
1004 format(2I8,I4,f8.4,3f16.10)
1005 format(4I8)
1006 format(5I8)
1007 format(6I8)
1008 format(6I8)

     end program single2data
	