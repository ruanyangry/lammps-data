  program single2data
    implicit none
    integer i,j,k,l
    integer tatoms,tbonds,tangles,tdihedrals,timpropers
    integer atomtypes,bondtypes,angletypes,dihedraltypes,impropertypes
    integer serial
    real(kind=8) xlo,xhi,ylo,yhi,zlo,zhi
    real(kind=8),allocatable::mass(:,:),atoms(:,:),bonds(:,:),angles(:,:),dihedrals(:,:),impropers(:,:)
	real(kind=8),allocatable::paircoeffs(:,:),bondcoeffs(:,:),anglecoeffs(:,:),dihedralcoeffs(:,:),impropercoeffs(:,:)
    character*80 filename,pdbfile,CRYST,ATOM,resname,atomname,resid,string,CRYST1,coeffes
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
	read(10,*)coeffes
    close(10)
	
	write(*,*)'#------------------------------------------------------------------#'
	write(*,*)'#                        Author Ruan Yang                          #'
	write(*,*)'#                  Email: ruanyang_njut@163.com                    #'
	write(*,*)'#      If variable coeffes ="on", default force file amber         #'
	write(*,*)'#                      bond_style harmonic                         #'
	write(*,*)'#                     angle_style harmonic                         #'
	write(*,*)'#                     dihedral_style harmonic                      #'
	write(*,*)'#                     improper_style harmonic                      #'
	write(*,*)'#        Compile: gfortran -o single2data.exe single2data.f90      #'
	write(*,*)'#                    ./single2data.exe < input.inp                 #'
	write(*,*)'#------------------------------------------------------------------#'
  
! Get information from single chain

    open(unit=12,file=filename)
    
    read(12,*)
    read(12,*)
    
	if(mask.eq.(1))then
    read(12,*)tatoms,string
	end if
	if(mask.eq.(2))then
	read(12,*)tatoms,string
	read(12,*)tbonds,string
	end if
	if(mask.eq.(3))then
	read(12,*)tatoms,string
	read(12,*)tbonds,string
	read(12,*)tangles,string
	end if
	if(mask.eq.(4))then
	read(12,*)tatoms,string
	read(12,*)tbonds,string
	read(12,*)tangles,string
	read(12,*)tdihedrals,string
	end if
	if(mask.eq.(5))then
	read(12,*)tatoms,string
	read(12,*)tbonds,string
	read(12,*)tangles,string
	read(12,*)tdihedrals,string
	read(12,*)timpropers,string
	end if
	
	read(12,*)
	
	if(mask.eq.(1))then
	read(12,*)atomtypes,string
	end if
	if(mask.eq.(2))then
	read(12,*)atomtypes,string
	read(12,*)bondtypes,string
	end if
	if(mask.eq.(3))then
	read(12,*)atomtypes,string
	read(12,*)bondtypes,string
	read(12,*)angletypes,string
	end if
	if(mask.eq.(4))then
	read(12,*)atomtypes,string
	read(12,*)bondtypes,string
	read(12,*)angletypes,string
	read(12,*)dihedraltypes,string
	end if
	if(mask.eq.(5))then
	read(12,*)atomtypes,string
	read(12,*)bondtypes,string
	read(12,*)angletypes,string
	read(12,*)dihedraltypes,string
	read(12,*)impropertypes,string
	end if
	
	allocate(mass(atomtypes,2))
	if(mask.eq.(1))then
	allocate(atoms(tatoms,columns))
	allocate(paircoeffs(atomtypes,3))
	end if
	if(mask.eq.(2))then
	allocate(atoms(tatoms,columns))
	allocate(bonds(tbonds,4))
	allocate(paircoeffs(atomtypes,3))
	allocate(bondcoeffs(bondtypes,3))
	end if
	if(mask.eq.(3))then
	allocate(atoms(tatoms,columns))
	allocate(bonds(tbonds,4))
	allocate(angles(tangles,5))
	allocate(paircoeffs(atomtypes,3))
	allocate(bondcoeffs(bondtypes,3))
	allocate(anglecoeffs(angletypes,3))
	end if
	if(mask.eq.(4))then
	allocate(atoms(tatoms,columns))
	allocate(bonds(tbonds,4))
	allocate(angles(tangles,5))
	allocate(dihedrals(tdihedrals,6))
	allocate(paircoeffs(atomtypes,3))
	allocate(bondcoeffs(bondtypes,3))
	allocate(anglecoeffs(angletypes,3))
	allocate(dihedralcoeffs(dihedraltypes,4))
	end if
	if(mask.eq.(5))then
	allocate(atoms(tatoms,columns))
	allocate(bonds(tbonds,4))
	allocate(angles(tangles,5))
	allocate(dihedrals(tdihedrals,6))
	allocate(impropers(timpropers,6))
	allocate(paircoeffs(atomtypes,3))
	allocate(bondcoeffs(bondtypes,3))
	allocate(anglecoeffs(angletypes,3))
	allocate(dihedralcoeffs(dihedraltypes,4))
	allocate(impropercoeffs(impropertypes,3))
	end if
	
	read(12,*)
	
	read(12,*)xlo,xhi,string
	read(12,*)ylo,yhi,string
	read(12,*)zlo,zhi,string
	
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
	
	if(coeffes.eq."on")then
	do i=1,atomtypes
	   read(12,*)paircoeffs(i,:)
	end do
	
	read(12,*)
	read(12,*)
	read(12,*)
	end if
	
    do i=1,tatoms
	  read(12,*)atoms(i,:)
	end do
	end if
	
	if(mask.eq.(2))then
	
! atoms
    if(coeffes.eq."on")then
	do i=1,atomtypes
	   read(12,*)paircoeffs(i,:)
	end do
	
	read(12,*)
	read(12,*)
	read(12,*)
	
	do i=1,bondtypes
	    read(12,*)bondcoeffs(i,:)
	end do
	
	read(12,*)
	read(12,*)
	read(12,*)
	
	end if

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
    if(coeffes.eq."on")then
	do i=1,atomtypes
	   read(12,*)paircoeffs(i,:)
	end do
	
	read(12,*)
	read(12,*)
	read(12,*)
	
	do i=1,bondtypes
	    read(12,*)bondcoeffs(i,:)
	end do
	
	read(12,*)
	read(12,*)
	read(12,*)
	
	do i=1,angletypes
	    read(12,*)anglecoeffs(i,:)
	end do
	
	read(12,*)
	read(12,*)
	read(12,*)
	
	end if
	
	
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

    if(coeffes.eq."on")then
	do i=1,atomtypes
	   read(12,*)paircoeffs(i,:)
!	   write(*,*)paircoeffs(i,:)
	end do
	
	read(12,*)
	read(12,*)
	read(12,*)
	
	do i=1,bondtypes
	    read(12,*)bondcoeffs(i,:)
	end do
	
	read(12,*)
	read(12,*)
	read(12,*)
	
	do i=1,angletypes
	    read(12,*)anglecoeffs(i,:)
	end do
	
	read(12,*)
	read(12,*)
	read(12,*)
	
	do i=1,dihedraltypes
	    read(12,*)dihedralcoeffs(i,:)
	end do
	
	read(12,*)
	read(12,*)
	read(12,*)
	
	end if
	

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

    if(coeffes.eq."on")then
	do i=1,atomtypes
	   read(12,*)paircoeffs(i,:)
	end do
	
	read(12,*)
	read(12,*)
	read(12,*)
	
	do i=1,bondtypes
	    read(12,*)bondcoeffs(i,:)
	end do
	
	read(12,*)
	read(12,*)
	read(12,*)
	
	do i=1,angletypes
	    read(12,*)anglecoeffs(i,:)
	end do
	
	read(12,*)
	read(12,*)
	read(12,*)
	
	do i=1,dihedraltypes
	    read(12,*)dihedralcoeffs(i,:)
	end do
	
	read(12,*)
	read(12,*)
	read(12,*)
	
	do i=1,impropertypes
	    read(12,*)impropercoeffs(i,:)
	end do
	
	read(12,*)
	read(12,*)
	read(12,*)
	
	end if
	
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
!	 read(13,*)CRYST1,box(:)
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
	 if(coeffes.eq."on")then
	 write(11,*)'Pair Coeffs'
	 write(11,*)
	 
	 do i=1,atomtypes
	    write(11,'(I5,2f12.3)')int(paircoeffs(i,1)),paircoeffs(i,2:)
	 end do
	 write(11,*)
	 end if
	 
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
	 
	 if(coeffes.eq."on")then
	 write(11,*)'Pair Coeffs'
	 write(11,*)
	 
	 do i=1,atomtypes
	    write(11,'(I5,2f12.3)')int(paircoeffs(i,1)),paircoeffs(i,2:)
	 end do
	 write(11,*)
	 
	 write(11,*)'Bond Coeffs'
	 write(11,*)
	 
	 do i=1,bondtypes
	    write(11,'(I5,2f12.3)')int(bondcoeffs(i,1)),bondcoeffs(i,2:)
	 end do
	 write(11,*)
	 
	 end if
	 
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
	 
	 if(coeffes.eq."on")then
	 write(11,*)'Pair Coeffs'
	 write(11,*)
	 
	 do i=1,atomtypes
	    write(11,'(I5,2f12.3)')int(paircoeffs(i,1)),paircoeffs(i,2:)
	 end do
	 write(11,*)
	 
	 write(11,*)'Bond Coeffs'
	 write(11,*)
	 
	 do i=1,bondtypes
	    write(11,'(I5,2f12.3)')int(bondcoeffs(i,1)),bondcoeffs(i,2:)
	 end do
	 write(11,*)
	 
	 write(11,*)'Angle Coeffs'
	 write(11,*)
	 
	 do i=1,angletypes
	    write(11,'(I5,2f12.3)')int(anglecoeffs(i,1)),anglecoeffs(i,2:)
	 end do
	 write(11,*)
	 
	 end if
	 
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
	 
	 if(coeffes.eq."on")then
	 write(11,*)'Pair Coeffs'
	 write(11,*)
	 
	 do i=1,atomtypes
	    write(11,'(I5,2f12.3)')int(paircoeffs(i,1)),paircoeffs(i,2:)
	 end do
	 write(11,*)
	 
	 write(11,*)'Bond Coeffs'
	 write(11,*)
	 
	 do i=1,bondtypes
	    write(11,'(I5,2f12.3)')int(bondcoeffs(i,1)),bondcoeffs(i,2:)
	 end do
	 write(11,*)
	 
	 write(11,*)'Angle Coeffs'
	 write(11,*)
	 
	 do i=1,angletypes
	    write(11,'(I5,2f12.3)')int(anglecoeffs(i,1)),anglecoeffs(i,2:)
	 end do
	 write(11,*)
	 
	 write(11,*)'Dihedral Coeffs'
	 write(11,*)
	 
	 do i=1,dihedraltypes
	    write(11,'(I5,f12.3,2I5)')int(dihedralcoeffs(i,1)),dihedralcoeffs(i,2),int(dihedralcoeffs(i,3:))
	 end do
	 write(11,*)
	 
	 end if
	 
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
	
	if(coeffes.eq."on")then
	 write(11,*)'Pair Coeffs'
	 write(11,*)
	 
	 do i=1,atomtypes
	    write(11,'(I5,2f12.3)')int(paircoeffs(i,1)),paircoeffs(i,2:)
	 end do
	 write(11,*)
	 
	 write(11,*)'Bond Coeffs'
	 write(11,*)
	 
	 do i=1,bondtypes
	    write(11,'(I5,2f12.3)')int(bondcoeffs(i,1)),bondcoeffs(i,2:)
	 end do
	 write(11,*)
	 
	 write(11,*)'Angle Coeffs'
	 write(11,*)
	 
	 do i=1,angletypes
	    write(11,'(I5,2f12.3)')aint(anglecoeffs(i,1)),anglecoeffs(i,2:)
	 end do
	 write(11,*)
	 
	 write(11,*)'Dihedral Coeffs'
	 write(11,*)
	 
	 do i=1,dihedraltypes
	    write(11,'(I5,f12.3,2I5)')int(dihedralcoeffs(i,1)),dihedralcoeffs(i,2),int(dihedralcoeffs(i,3:))
	 end do
	 write(11,*)
	 
	 write(11,*)'Improper Coeffs'
	 write(11,*)
	 
	 do i=1,impropertypes
	    write(11,'(I5,2f12.3)')int(impropercoeffs(i,1)),impropercoeffs(i,2:)
	 end do
	 write(11,*)
	 
	 end if
	 
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
	