program nc_reader

  use netcdf_reader
  
  implicit none
  
  character(256) :: fileName='ECMWF_ERA-40_subset.nc'
  
  real, dimension(:), pointer :: lat
  real, dimension(:,:), pointer :: var2d
  integer*2, dimension(:,:,:), pointer :: tcw
  integer, dimension(:), pointer :: time
  
  call nc_readvar(fileName, 'latitude', lat)
  call nc_readvar(fileName, 'time', time)
  call nc_readvar(fileName, 'tcw', tcw)

  print *, shape(lat), shape(time), shape(tcw)
  
end program nc_reader
