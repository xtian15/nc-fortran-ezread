module netcdf_reader
  use netcdf

  implicit none

  private

  public :: nc_readvar

  interface nc_readvar
    module procedure nc_readvar1d_integer2
    module procedure nc_readvar1d_integer4
    module procedure nc_readvar1d_real4
    module procedure nc_readvar1d_real8

    module procedure nc_readvar2d_integer2
    module procedure nc_readvar2d_integer4
    module procedure nc_readvar2d_real4
    module procedure nc_readvar2d_real8

    module procedure nc_readvar3d_integer2
    module procedure nc_readvar3d_integer4
    module procedure nc_readvar3d_real4
    module procedure nc_readvar3d_real8

    module procedure nc_readvar4d_integer2
    module procedure nc_readvar4d_integer4
    module procedure nc_readvar4d_real4
    module procedure nc_readvar4d_real8
  end interface


  integer, parameter :: i2 = 2
  integer, parameter :: i4 = 4
  integer, parameter :: i8 = 8
  integer, parameter :: r4 = 4
  integer, parameter :: r8 = 8

  integer(i4), parameter :: rank_one   = 1
  integer(i4), parameter :: rank_two   = 2
  integer(i4), parameter :: rank_three = 3
  integer(i4), parameter :: rank_four  = 4


contains

subroutine nc_readvar1d_integer2( filename, varname, varvalues )

  implicit none

  character(*),intent(in   ) :: filename
  character(*),intent(in   ) :: varname
  integer(i2), dimension(:), pointer :: varvalues

  ! local
  integer(i4) :: ncid, varid, dimids(rank_one)
  integer(i4) :: varsize(rank_one)
  integer(i4) :: vartype
  integer(i4) :: ndims
  integer(i4) :: errstatus


  errstatus = nf90_open( trim(filename), nf90_nowrite, ncid)
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the file id:",trim(filename)
     stop
  endif


  errstatus = nf90_inq_varid( ncid, trim(varname), varid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var id: ", trim(varname)
     stop
  endif 

  errstatus = nf90_inquire_variable( ncid, varid, ndims = ndims, &
                                     xtype=vartype, dimids = dimids )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to retrieve info of the var: ", trim(varname)
     stop
  endif


  if ( vartype /= nf90_short ) then
     print*, "error: var type mismatch."
     print*, "       the type of var ( ", trim(varname), " ) is ", vartype
     stop
  endif

  if ( ndims /= rank_one ) then
     print*, "error: var ( ", trim(varname), " ) is not of rank_one"
     stop
  endif

  errstatus = nf90_inquire_dimension( ncid, dimids(1), len=varsize(1) )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  allocate(varvalues(varsize(1)))


  errstatus = nf90_get_var( ncid, varid, varvalues )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var value: ", trim(varname)
     stop
  endif

  errstatus = nf90_close( ncid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to the close the var: ", trim(filename)
     stop
  endif
    
end subroutine


subroutine nc_readvar1d_integer4( filename, varname, varvalues )

  implicit none

  character(*),intent(in   ) :: filename
  character(*),intent(in   ) :: varname
  integer(i4), dimension(:), pointer :: varvalues

  ! local
  integer(i4) :: ncid, varid, dimids(rank_one)
  integer(i4) :: varsize(rank_one)
  integer(i4) :: vartype
  integer(i4) :: ndims
  integer(i4) :: errstatus


  errstatus = nf90_open( trim(filename), nf90_nowrite, ncid)
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the file id:",trim(filename)
     stop
  endif


  errstatus = nf90_inq_varid( ncid, trim(varname), varid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var id: ", trim(varname)
     stop
  endif 


  errstatus = nf90_inquire_variable( ncid, varid, ndims = ndims, &
                                     xtype=vartype, dimids = dimids )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to retrieve info of the var: ", trim(varname)
     stop
  endif


  if ( vartype /= nf90_int ) then
     print*, "error: var type mismatch."
     print*, "       the type of var ( ", trim(varname), " ) is ", vartype
     stop
  endif

  if ( ndims /= rank_one ) then
     print*, "error: var ( ", trim(varname), " ) is not of rank_one"
     stop
  endif

  errstatus = nf90_inquire_dimension( ncid, dimids(1), len=varsize(1) )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  allocate(varvalues(varsize(1)))

  errstatus = nf90_get_var( ncid, varid, varvalues )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var value: ", trim(varname)
     stop
  endif

  errstatus = nf90_close( ncid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to the close the var: ", trim(filename)
     stop
  endif
    
end subroutine


subroutine nc_readvar1d_real4( filename, varname, varvalues )

  implicit none

  character(*),intent(in   ) :: filename
  character(*),intent(in   ) :: varname
  real(r4), dimension(:), pointer :: varvalues
  ! local 
  integer(i4) :: ncid, varid, dimids(rank_one)
  integer(i4) :: varsize(rank_one)
  integer(i4) :: vartype
  integer(i4) :: ndims
  integer(i4) :: errstatus


  errstatus = nf90_open( trim(filename), nf90_nowrite, ncid)
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the file id:",trim(filename)
     stop
  endif


  errstatus = nf90_inq_varid( ncid, trim(varname), varid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var id: ", trim(varname)
     stop
  endif 

  errstatus = nf90_inquire_variable( ncid, varid, ndims = ndims, &
                                     xtype=vartype, dimids = dimids )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to retrieve info of the var: ", trim(varname)
     stop
  endif

  if ( vartype /= nf90_float ) then
     print*, "error: var type mismatches."
     print*, "       the type of var ( ", trim(varname), " ) is ", vartype
     stop
  endif

  if ( ndims /= rank_one ) then
     print*, "error: var ( ", trim(varname), " ) is not of rank_one"
     stop
  endif

  errstatus = nf90_inquire_dimension( ncid, dimids(1), len=varsize(1) )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  allocate(varvalues(varsize(1)))

  errstatus = nf90_get_var( ncid, varid, varvalues )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var value: ", trim(varname)
     stop
  endif

  errstatus = nf90_close( ncid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to the close the var: ", trim(filename)
     stop
  endif
    
end subroutine


subroutine nc_readvar1d_real8( filename, varname, varvalues )

  implicit none

  character(*),intent(in   ) :: filename
  character(*),intent(in   ) :: varname
  real(r8), dimension(:), pointer :: varvalues

  ! local
  integer(i4) :: ncid, varid, dimids(rank_one)
  integer(i4) :: varsize(rank_one)
  integer(i4) :: vartype
  integer(i4) :: ndims
  integer(i4) :: errstatus

  errstatus = nf90_open( trim(filename), nf90_nowrite, ncid)
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the file id:",trim(filename)
     stop
  endif

  errstatus = nf90_inq_varid( ncid, trim(varname), varid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var id: ", trim(varname)
     stop
  endif 

  errstatus = nf90_inquire_variable( ncid, varid, ndims = ndims, &
                                     xtype=vartype, dimids = dimids )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to retrieve info of the var: ", trim(varname)
     stop
  endif

  if ( vartype /= nf90_double ) then
     print*, "error: var type mismatches."
     print*, "       the type of var ( ", trim(varname), " ) is ", vartype
     stop
  endif

  if ( ndims /= rank_one ) then
     print*, "error: var ( ", trim(varname), " ) is not of rank_one"
     stop
  endif

  errstatus = nf90_inquire_dimension( ncid, dimids(1), len=varsize(1) )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  allocate(varvalues(varsize(1)))

  errstatus = nf90_get_var( ncid, varid, varvalues )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var value: ", trim(varname)
     stop
  endif

  errstatus = nf90_close( ncid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to the close the var: ", trim(filename)
     stop
  endif
    
end subroutine


subroutine nc_readvar2d_integer2( filename, varname, varvalues )

  implicit none

  character(*),intent(in   ) :: filename
  character(*),intent(in   ) :: varname
  integer(i2), dimension(:,:), pointer :: varvalues
  ! local
  integer(i4) :: ncid, varid, dimids(rank_two)
  integer(i4) :: vartype
  integer(i4) :: varsize(rank_two)
  integer(i4) :: ndims
  integer(i4) :: errstatus

  errstatus = nf90_open( trim(filename), nf90_nowrite, ncid)
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the file id:",trim(filename)
     stop
  endif

  errstatus = nf90_inq_varid( ncid, trim(varname), varid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var id: ", trim(varname)
     stop
  endif 

  errstatus = nf90_inquire_variable( ncid, varid, ndims = ndims, &
                                     xtype=vartype, dimids = dimids )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to retrieve info of the var: ", trim(varname)
     stop
  endif

  if ( vartype /= nf90_short ) then
     print*, "error: var type mismatch."
     print*, "       the type of var ( ", trim(varname), " ) is ", vartype
     stop
  endif

  if ( ndims /= rank_two ) then
     print*, "error: var ( ", trim(varname), " ) is not of rank_two"
     stop
  endif

  errstatus = nf90_inquire_dimension( ncid, dimids(1), len=varsize(1) )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(2), len=varsize(2) )  
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  allocate(varvalues(varsize(1), varsize(2)))

  errstatus = nf90_get_var( ncid, varid, varvalues )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var value: ", trim(varname)
     stop
  endif

  errstatus = nf90_close( ncid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to the close the var: ", trim(filename)
     stop
  endif

end subroutine


subroutine nc_readvar2d_integer4( filename, varname, varvalues )

  implicit none

  character(*),intent(in   ) :: filename
  character(*),intent(in   ) :: varname
  integer(i4), dimension(:,:), pointer :: varvalues
  ! local
  integer(i4) :: ncid, varid, dimids(rank_two)
  integer(i4) :: vartype
  integer(i4) :: varsize(rank_two)
  integer(i4) :: ndims
  integer(i4) :: errstatus

  errstatus = nf90_open( trim(filename), nf90_nowrite, ncid)
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the file id:",trim(filename)
     stop
  endif

  errstatus = nf90_inq_varid( ncid, trim(varname), varid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var id: ", trim(varname)
     stop
  endif 

  errstatus = nf90_inquire_variable( ncid, varid, ndims = ndims, &
                                     xtype=vartype, dimids = dimids )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to retrieve info of the var: ", trim(varname)
     stop
  endif

  if ( vartype /= nf90_int ) then
     print*, "error: var type mismatch."
     print*, "       the type of var ( ", trim(varname), " ) is ", vartype
     stop
  endif

  if ( ndims /= rank_two ) then
     print*, "error: var ( ", trim(varname), " ) is not of rank_two"
     stop
  endif

  errstatus = nf90_inquire_dimension( ncid, dimids(1), len=varsize(1) )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(2), len=varsize(2) )  
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  allocate(varvalues(varsize(1), varsize(2)))

  errstatus = nf90_get_var( ncid, varid, varvalues )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var value: ", trim(varname)
     stop
  endif

  errstatus = nf90_close( ncid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to the close the var: ", trim(filename)
     stop
  endif

end subroutine


subroutine nc_readvar2d_real4( filename, varname, varvalues )

  implicit none

  character(*),intent(in   ) :: filename
  character(*),intent(in   ) :: varname
  real(r4), dimension(:,:), pointer :: varvalues

  ! local 
  integer(i4) :: ncid, varid, dimids(rank_two)
  integer(i4) :: vartype
  integer(i4) :: varsize(rank_two)
  integer(i4) :: ndims
  integer(i4) :: errstatus

  errstatus = nf90_open( trim(filename), nf90_nowrite, ncid)
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the file id:",trim(filename)
     stop
  endif

  errstatus = nf90_inq_varid( ncid, trim(varname), varid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var id: ", trim(varname)
     stop
  endif 

  errstatus = nf90_inquire_variable( ncid, varid, ndims = ndims, &
                                     xtype=vartype, dimids = dimids )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to retrieve info of the var: ", trim(varname)
     stop
  endif

  if ( vartype /= nf90_float ) then
     print*, "error: var type mismatch."
     print*, "       the type of var ( ", trim(varname), " ) is ", vartype
     stop
  endif

  if ( ndims /= rank_two ) then
     print*, "error: var ( ", trim(varname), " ) is not of rank_two"
     stop
  endif

  errstatus = nf90_inquire_dimension( ncid, dimids(1), len=varsize(1) )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(2), len=varsize(2) )  
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  allocate(varvalues(varsize(1), varsize(2)))


  errstatus = nf90_get_var( ncid, varid, varvalues )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var value: ", trim(varname)
     stop
  endif

  errstatus = nf90_close( ncid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to the close the var: ", trim(filename)
     stop
  endif

end subroutine


subroutine nc_readvar2d_real8( filename, varname, varvalues )

  implicit none

  character(*),intent(in   ) :: filename
  character(*),intent(in   ) :: varname
  real(r8), dimension(:,:), pointer :: varvalues
  ! local
  integer(i4) :: ncid, varid, dimids(rank_two)
  integer(i4) :: vartype
  integer(i4) :: varsize(rank_two)
  integer(i4) :: ndims
  integer(i4) :: errstatus

  errstatus = nf90_open( trim(filename), nf90_nowrite, ncid)
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the file id:",trim(filename)
     stop
  endif

  errstatus = nf90_inq_varid( ncid, trim(varname), varid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var id: ", trim(varname)
     stop
  endif 

  errstatus = nf90_inquire_variable( ncid, varid, ndims = ndims, &
                                     xtype=vartype, dimids = dimids )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to retrieve info of the var: ", trim(varname)
     stop
  endif

  if ( vartype /= nf90_double ) then
     print*, "error: var type mismatch."
     print*, "       the type of var ( ", trim(varname), " ) is ", vartype
     stop
  endif

  if ( ndims /= rank_two ) then
     print*, "error: var ( ", trim(varname), " ) is not of rank_two"
     stop
  endif

  errstatus = nf90_inquire_dimension( ncid, dimids(1), len=varsize(1) )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(2), len=varsize(2) )  
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  allocate(varvalues(varsize(1), varsize(2)))

  errstatus = nf90_get_var( ncid, varid, varvalues )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var value: ", trim(varname)
     stop
  endif

  errstatus = nf90_close( ncid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to the close the var: ", trim(filename)
     stop
  endif

end subroutine


subroutine nc_readvar3d_integer2( filename, varname, varvalues )

  implicit none

  character(*),intent(in   ) :: filename
  character(*),intent(in   ) :: varname
  integer(i2), dimension(:,:,:), pointer :: varvalues
  ! local
  integer(i4) :: ncid, varid, dimids(rank_three)
  integer(i4) :: vartype
  integer(i4) :: varsize(rank_three)
  integer(i4) :: ndims
  integer(i4) :: errstatus

  errstatus = nf90_open( trim(filename), nf90_nowrite, ncid)
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the file id:",trim(filename)
     stop
  endif

  errstatus = nf90_inq_varid( ncid, trim(varname), varid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var id: ", trim(varname)
     stop
  endif 

  errstatus = nf90_inquire_variable( ncid, varid, ndims = ndims, &
                                     xtype=vartype, dimids = dimids )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to retrieve info of the var: ", trim(varname)
     stop
  endif

  if ( vartype /= nf90_short ) then
     print*, "error: var type mismatch."
     print*, "       the type of var ( ", trim(varname), " ) is ", vartype
     stop
  endif

  if ( ndims /= rank_three ) then
     print*, "error: var ( ", trim(varname), " ) is not of rank_three"
     stop
  endif

  errstatus = nf90_inquire_dimension( ncid, dimids(1), len=varsize(1) )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(2), len=varsize(2) )  
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(3), len=varsize(3) )  
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  allocate(varvalues(varsize(1), varsize(2), varsize(3)))

  errstatus = nf90_get_var( ncid, varid, varvalues )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var value: ", trim(varname)
     stop
  endif

  errstatus = nf90_close( ncid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to the close the var: ", trim(filename)
     stop
  endif

end subroutine


subroutine nc_readvar3d_integer4( filename, varname, varvalues )

  implicit none

  character(*),intent(in   ) :: filename
  character(*),intent(in   ) :: varname
  integer(i4), dimension(:,:,:), pointer :: varvalues

  ! local
  integer(i4) :: ncid, varid, dimids(rank_three)
  integer(i4) :: vartype
  integer(i4) :: varsize(rank_three)
  integer(i4) :: ndims
  integer(i4) :: errstatus

  errstatus = nf90_open( trim(filename), nf90_nowrite, ncid)
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the file id:",trim(filename)
     stop
  endif

  errstatus = nf90_inq_varid( ncid, trim(varname), varid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var id: ", trim(varname)
     stop
  endif 

  errstatus = nf90_inquire_variable( ncid, varid, ndims = ndims, &
                                     xtype=vartype, dimids = dimids )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to retrieve info of the var: ", trim(varname)
     stop
  endif

  if ( vartype /= nf90_int ) then
     print*, "error: var type mismatch."
     print*, "       the type of var ( ", trim(varname), " ) is ", vartype
     stop
  endif

  if ( ndims /= rank_three ) then
     print*, "error: var ( ", trim(varname), " ) is not of rank_three"
     stop
  endif

  errstatus = nf90_inquire_dimension( ncid, dimids(1), len=varsize(1) )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(2), len=varsize(2) )  
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(3), len=varsize(3) )  
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  allocate(varvalues(varsize(1), varsize(2), varsize(3)))

  errstatus = nf90_get_var( ncid, varid, varvalues )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var value: ", trim(varname)
     stop
  endif

  errstatus = nf90_close( ncid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to the close the var: ", trim(filename)
     stop
  endif

end subroutine


subroutine nc_readvar3d_real4( filename, varname, varvalues )

  implicit none

  character(*),intent(in   ) :: filename
  character(*),intent(in   ) :: varname
  real(r4), dimension(:,:,:), pointer :: varvalues
  ! local
  integer(i4) :: ncid, varid, dimids(rank_three)
  integer(i4) :: vartype
  integer(i4) :: varsize(rank_three)
  integer(i4) :: ndims
  integer(i4) :: errstatus

  errstatus = nf90_open( trim(filename), nf90_nowrite, ncid)
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the file id:",trim(filename)
     stop
  endif

  errstatus = nf90_inq_varid( ncid, trim(varname), varid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var id: ", trim(varname)
     stop
  endif 

  errstatus = nf90_inquire_variable( ncid, varid, ndims = ndims, &
                                     xtype=vartype, dimids = dimids )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to retrieve info of the var: ", trim(varname)
     stop
  endif

  if ( vartype /= nf90_float ) then
     print*, "error: var type mismatch."
     print*, "       the type of var ( ", trim(varname), " ) is ", vartype
     stop
  endif

  if ( ndims /= rank_three ) then
     print*, "error: var ( ", trim(varname), " ) is not of rank_three"
     stop
  endif

  errstatus = nf90_inquire_dimension( ncid, dimids(1), len=varsize(1) )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(2), len=varsize(2) )  
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(3), len=varsize(3) )  
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  allocate(varvalues(varsize(1), varsize(2), varsize(3)))

  errstatus = nf90_get_var( ncid, varid, varvalues )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var value: ", trim(varname)
     stop
  endif

  errstatus = nf90_close( ncid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to the close the var: ", trim(filename)
     stop
  endif

end subroutine


subroutine nc_readvar3d_real8( filename, varname, varvalues )

  implicit none

  character(*),intent(in   ) :: filename
  character(*),intent(in   ) :: varname
  real(r8), dimension(:,:,:), pointer :: varvalues
  ! local
  integer(i4) :: ncid, varid, dimids(rank_three)
  integer(i4) :: vartype
  integer(i4) :: varsize(rank_three)
  integer(i4) :: ndims
  integer(i4) :: errstatus

  errstatus = nf90_open( trim(filename), nf90_nowrite, ncid)
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the file id:",trim(filename)
     stop
  endif

  errstatus = nf90_inq_varid( ncid, trim(varname), varid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var id: ", trim(varname)
     stop
  endif 

  errstatus = nf90_inquire_variable( ncid, varid, ndims = ndims, &
                                     xtype=vartype, dimids = dimids )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to retrieve info of the var: ", trim(varname)
     stop
  endif

  if ( vartype /= nf90_double ) then
     print*, "error: var type mismatch."
     print*, "       the type of var ( ", trim(varname), " ) is ", vartype
     stop
  endif

  if ( ndims /= rank_three ) then
     print*, "error: var ( ", trim(varname), " ) is not of rank_three"
     stop
  endif

  errstatus = nf90_inquire_dimension( ncid, dimids(1), len=varsize(1) )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(2), len=varsize(2) )  
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(3), len=varsize(3) )  
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  allocate(varvalues(varsize(1), varsize(2), varsize(3)))

  errstatus = nf90_get_var( ncid, varid, varvalues )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var value: ", trim(varname)
     stop
  endif

  errstatus = nf90_close( ncid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to the close the var: ", trim(filename)
     stop
  endif

end subroutine


subroutine nc_readvar4d_integer2( filename, varname, varvalues )

  implicit none

  character(*),intent(in   ) :: filename
  character(*),intent(in   ) :: varname
  integer(i2), dimension(:,:,:,:), pointer :: varvalues

  ! local
  integer(i4) :: ncid, varid, dimids(rank_four)
  integer(i4) :: vartype
  integer(i4) :: varsize(rank_four)
  integer(i4) :: ndims
  integer(i4) :: errstatus

  errstatus = nf90_open( trim(filename), nf90_nowrite, ncid)
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the file id:",trim(filename)
     stop
  endif

  errstatus = nf90_inq_varid( ncid, trim(varname), varid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var id: ", trim(varname)
     stop
  endif 

  errstatus = nf90_inquire_variable( ncid, varid, ndims = ndims, &
                                     xtype=vartype, dimids = dimids )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to retrieve info of the var: ", trim(varname)
     stop
  endif

  if ( vartype /= nf90_short ) then
     print*, "error: var type mismatch."
     print*, "       the type of var ( ", trim(varname), " ) is ", vartype
     stop
  endif

  if ( ndims /= rank_four ) then
     print*, "error: var ( ", trim(varname), " ) is not of rank_four"
     stop
  endif

  errstatus = nf90_inquire_dimension( ncid, dimids(1), len=varsize(1) )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(2), len=varsize(2) )  
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(3), len=varsize(3) )  
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(4), len=varsize(4) )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  allocate(varvalues(varsize(1), varsize(2), varsize(3), varsize(4)))

  errstatus = nf90_get_var( ncid, varid, varvalues )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var value: ", trim(varname)
     stop
  endif

  errstatus = nf90_close( ncid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to the close the var: ", trim(filename)
     stop
  endif

end subroutine


subroutine nc_readvar4d_integer4( filename, varname, varvalues )

  implicit none

  character(*),intent(in   ) :: filename
  character(*),intent(in   ) :: varname
  integer(i4), dimension(:,:,:,:), pointer :: varvalues
  ! local
  integer(i4) :: ncid, varid, dimids(rank_four)
  integer(i4) :: vartype
  integer(i4) :: varsize(rank_four)
  integer(i4) :: ndims
  integer(i4) :: errstatus

  errstatus = nf90_open( trim(filename), nf90_nowrite, ncid)
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the file id:",trim(filename)
     stop
  endif

  errstatus = nf90_inq_varid( ncid, trim(varname), varid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var id: ", trim(varname)
     stop
  endif 

  errstatus = nf90_inquire_variable( ncid, varid, ndims = ndims, &
                                     xtype=vartype, dimids = dimids )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to retrieve info of the var: ", trim(varname)
     stop
  endif

  if ( vartype /= nf90_int ) then
     print*, "error: var type mismatch."
     print*, "       the type of var ( ", trim(varname), " ) is ", vartype
     stop
  endif

  if ( ndims /= rank_four ) then
     print*, "error: var ( ", trim(varname), " ) is not of rank_four"
     stop
  endif

  errstatus = nf90_inquire_dimension( ncid, dimids(1), len=varsize(1) )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(2), len=varsize(2) )  
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(3), len=varsize(3) )  
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(4), len=varsize(4) )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  allocate(varvalues(varsize(1), varsize(2), varsize(3), varsize(4)))

  errstatus = nf90_get_var( ncid, varid, varvalues )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var value: ", trim(varname)
     stop
  endif

  errstatus = nf90_close( ncid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to the close the var: ", trim(filename)
     stop
  endif

end subroutine


subroutine nc_readvar4d_real4( filename, varname, varvalues )

  implicit none

  character(*),intent(in   ) :: filename
  character(*),intent(in   ) :: varname
  real(r4), dimension(:,:,:,:), pointer :: varvalues

  ! local
  integer(i4) :: ncid, varid, dimids(rank_four)
  integer(i4) :: vartype
  integer(i4) :: varsize(rank_four)
  integer(i4) :: ndims
  integer(i4) :: errstatus

  errstatus = nf90_open( trim(filename), nf90_nowrite, ncid)
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the file id:",trim(filename)
     stop
  endif

  errstatus = nf90_inq_varid( ncid, trim(varname), varid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var id: ", trim(varname)
     stop
  endif 

  errstatus = nf90_inquire_variable( ncid, varid, ndims = ndims, &
                                     xtype=vartype, dimids = dimids )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to retrieve info of the var: ", trim(varname)
     stop
  endif

  if ( vartype /= nf90_float ) then
     print*, "error: var type mismatch."
     print*, "       the type of var ( ", trim(varname), " ) is ", vartype
     stop
  endif

  if ( ndims /= rank_four ) then
     print*, "error: var ( ", trim(varname), " ) is not of rank_four"
     stop
  endif

  errstatus = nf90_inquire_dimension( ncid, dimids(1), len=varsize(1) )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(2), len=varsize(2) )  
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(3), len=varsize(3) )  
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(4), len=varsize(4) )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  allocate(varvalues(varsize(1), varsize(2), varsize(3), varsize(4)))

  errstatus = nf90_get_var( ncid, varid, varvalues )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var value: ", trim(varname)
     stop
  endif

  errstatus = nf90_close( ncid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to the close the var: ", trim(filename)
     stop
  endif

end subroutine



subroutine nc_readvar4d_real8( filename, varname, varvalues )

  implicit none

  character(*),intent(in   ) :: filename
  character(*),intent(in   ) :: varname
  real(r8), dimension(:,:,:,:), pointer :: varvalues

  ! local
  integer(i4) :: ncid, varid, dimids(rank_four)
  integer(i4) :: vartype
  integer(i4) :: varsize(rank_four)
  integer(i4) :: ndims
  integer(i4) :: errstatus

  errstatus = nf90_open( trim(filename), nf90_nowrite, ncid)
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the file id:",trim(filename)
     stop
  endif

  errstatus = nf90_inq_varid( ncid, trim(varname), varid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var id: ", trim(varname)
     stop
  endif 

  errstatus = nf90_inquire_variable( ncid, varid, ndims = ndims, &
                                     xtype=vartype, dimids = dimids )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to retrieve info of the var: ", trim(varname)
     stop
  endif

  if ( vartype /= nf90_double ) then
     print*, "error: var type mismatch."
     print*, "       the type of var ( ", trim(varname), " ) is ", vartype
     stop
  endif

  if ( ndims /= rank_four ) then
     print*, "error: var ( ", trim(varname), " ) is not of rank_four"
     stop
  endif

  errstatus = nf90_inquire_dimension( ncid, dimids(1), len=varsize(1) )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(2), len=varsize(2) )  
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(3), len=varsize(3) )  
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  errstatus = nf90_inquire_dimension( ncid, dimids(4), len=varsize(4) )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the variable dimension(s)"
     stop
  endif
  allocate(varvalues(varsize(1), varsize(2), varsize(3), varsize(4)))

  errstatus = nf90_get_var( ncid, varid, varvalues )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to get the var value: ", trim(varname)
     stop
  endif

  errstatus = nf90_close( ncid )
  if ( errstatus /= nf90_noerr ) then
     print*, "error: fail to the close the var: ", trim(filename)
     stop
  endif

end subroutine

end module

