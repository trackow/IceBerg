module config_mod

    implicit none

    public

    ! KIND parameter for reals used in exchange with the coupler. This is made
    ! compatible with OASIS double precision
    integer,parameter :: RNP = SELECTED_REAL_KIND(12,307)

    character(len=*),parameter :: NamelistFileName  = "namelist.amip"

    ! x and y (i and j) dimensions of the AMIP grid
    integer,parameter :: GridNX = 360
    integer,parameter :: GridNY = 180

    ! Logical IO unit number to read the namelist
    integer,parameter :: NAMLUN = 13

    ! Logical IO unit number to write the logfile
    integer,parameter :: LOGLUN = 14

    ! Array with names of the netCDF files
    integer,parameter :: fname_length=200
    character(len=fname_length) :: FileListSST(15)
    character(len=fname_length) :: FileListSIC(15)

    ! Switch for linera/nearest neighbour time interpolation
    logical :: LInterpolate

    ! Switch for debugging output, can be set in namelist
    logical :: LDebug

contains

subroutine ERROR(msg)
    character(len=*),intent(in) :: msg

    write (*,'("*EE* ",A)') msg
    stop 'Aborting AMIP forcing.'
end subroutine ERROR

end module config_mod
