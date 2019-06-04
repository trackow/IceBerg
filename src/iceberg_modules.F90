module iceberg_mod

    use config_mod
    use NetCDF

    implicit none

    private

    public setup_iceberg
    public update_sst_and_sic
    public finalise_iceberg

    real(RNP),pointer     :: iceberg_sst(:,:)
    real(RNP),pointer     :: iceberg_sic(:,:)
    real(RNP),pointer     :: iceberg_ssh(:,:)
    real(RNP),pointer     :: iceberg_u_oce(:,:)
    real(RNP),pointer     :: iceberg_v_oce(:,:)
    real(RNP),pointer     :: iceberg_u_atm(:,:)
    real(RNP),pointer     :: iceberg_v_atm(:,:)
    real(RNP),pointer     :: iceberg_u_ice(:,:)
    real(RNP),pointer     :: iceberg_v_ice(:,:)
    real(RNP),pointer     :: iceberg_melt(:,:)

    real(kind=8) :: deltaT

    integer :: 	ifile, ncid_sst,varid_sst, ncid_sic,varid_sic, ncid_ssh,varid_ssh, &
		ncid_u_oce,varid_u_oce, ncid_v_oce,varid_v_oce, &
		ncid_u_atm,varid_u_atm, ncid_v_atm,varid_v_atm, &
		ncid_u_ice,varid_u_ice, ncid_v_ice,varid_v_ice, &
		ncid_melt,varid_melt 
    integer :: t_index,old_t_index

    ! keep precision of variables in the netcdf files here
    real(kind=4) :: sst(GridNX,GridNY,2)
    real(kind=4) :: sic(GridNX,GridNY,2)
    real(kind=8), allocatable :: t_sst(:)

    real(kind=8) :: t,t1,t2

    character (len=fname_length) :: fname_sst,fname_sic,fname_ssh,fname_u_oce,fname_v_oce,fname_u_atm,fname_v_atm,fname_u_ice,fname_v_ice,fname_melt

    logical :: lfirst,file_found

contains

    subroutine setup_iceberg(yy,mm,dd,dtime)

        use CPLNG

        integer, intent(in) :: yy,mm,dd,dtime

        iceberg_sst      => CPLNG_FLD(CPLNG_IDX('iceberg_sst'))%D
        iceberg_sic      => CPLNG_FLD(CPLNG_IDX('iceberg_sic'))%D
        iceberg_ssh      => CPLNG_FLD(CPLNG_IDX('iceberg_ssh'))%D
        iceberg_u_oce    => CPLNG_FLD(CPLNG_IDX('iceberg_u_oce'))%D
        iceberg_v_oce    => CPLNG_FLD(CPLNG_IDX('iceberg_v_oce'))%D
        iceberg_u_atm    => CPLNG_FLD(CPLNG_IDX('iceberg_u_atm'))%D
        iceberg_v_atm    => CPLNG_FLD(CPLNG_IDX('iceberg_v_atm'))%D
        iceberg_u_ice    => CPLNG_FLD(CPLNG_IDX('iceberg_u_ice'))%D
        iceberg_v_ice    => CPLNG_FLD(CPLNG_IDX('iceberg_v_ice'))%D

        iceberg_melt  => CPLNG_FLD(CPLNG_IDX('iceberg_melt'))%D

        deltaT=dtime/86400.

        call days_since_refdate(yy,mm,dd,t)

        ! initialise with dummy values
        t1=-9999.
        t2=-9999.

        ! point to first file
        ifile=1
        lfirst=.true.

    end subroutine setup_iceberg


    subroutine finalise_iceberg

        call close_file(ifile)

    end subroutine finalise_iceberg


    subroutine days_since_refdate(yy,mm,dd,nd)

        integer, intent(in)       :: yy,mm,dd
        real(kind=8), intent(out) :: nd
        integer                   :: yy0,dpm(12)
        logical                   :: leapyear

        leapyear=(mod(yy,4)==0 .and. (mod(yy,100)/=0 .or. mod(yy,400)==0))

        ! sets correct number of days for Jan 1 of year yy
        ! refdate is Jan 1, 1870 (in AMIP forcing dataset for CMIP6)
        yy0=1870
        nd=(yy-yy0)*365+int((yy-(yy0/4)*4)/4)- &
            & int((yy-(yy0/100)*100)/100)+int((yy-(yy0/400)*400)/400)

        if (leapyear) nd=nd-1

        dpm=(/31,28,31,30,31,30,31,31,30,31,30,31/)
        if (leapyear) dpm(2)=29

        if (mm>1) then
           nd=nd+sum(dpm(1:(mm-1)))+dd-1
        else
           nd=nd+dd-1
        endif

    end subroutine days_since_refdate


    subroutine update_position_melt

        integer :: nn

        if (lfirst) then
            call open_file(ifile)
            if ( t < t_sst(1) ) then
                t_index=1
            else
                do t_index=1,size(t_sst)-1
                    if (t_sst(t_index)<=t .and. t<t_sst(t_index+1)) exit
                enddo
            endif
            t2=t_sst(t_index)
            t1=t2
            old_t_index=t_index
            call read_file
            lfirst=.false.
        else
            t=t+deltaT
        endif

        do while ( t > t2 .and. file_found )
            call read_next_timestep_from_file
        enddo

        if (LInterpolate) then
            ! linear time interpolation
            if ( t1 < t2 ) then
                if (LDebug) then
                    write(LOGLUN,'(A,3F8.1)') 'time interpolation t1,t,t2',t1,t,t2
                endif
                AMIP_sst=((t-t1)*sst(:,:,2)+(t2-t)*sst(:,:,1))/(t2-t1)
                AMIP_sic=max(0.d0,min(1.d0,((t-t1)*sic(:,:,2)+(t2-t)*sic(:,:,1))/(t2-t1)))
            else
                if (LDebug) then
                    write(LOGLUN,'(A,3F8.1)') 'no time interpolation t,t2',t,t2
                endif
                AMIP_sst=sst(:,:,2)
                AMIP_sic=max(0.d0,min(1.d0,sic(:,:,2)))
            endif
        else
            ! nearest neighbour
            if ( t1 < t2 ) then
                if ( (floor(t)-floor(t1)) < (floor(t2)-floor(t)) ) then
                    nn=1
                else
                    nn=2
                endif
            else
                nn=2
            endif
            if (LDebug) then
                write(LOGLUN,'(A,I4,3F8.1)') 'nearest neigbour nn,t1,t,t2',nn,t1,t,t2
            endif
            AMIP_sst=sst(:,:,nn)
            AMIP_sic=max(0.d0,min(1.d0,sic(:,:,nn)))
        endif

    end subroutine update_sst_and_sic


    subroutine read_next_timestep_from_file

        t1=t2
        sst(:,:,1)=sst(:,:,2)
        sic(:,:,1)=sic(:,:,2)
        
        if ( t > maxval(t_sst) ) then
            call close_file(ifile)
            ifile=ifile+1
            call open_file(ifile)

            if ( .not.file_found ) then
                ifile=ifile-1
                return
            endif

            old_t_index=0
        endif
        
        t_index=old_t_index+1
        
        t2=t_sst(t_index)
        call read_file

        old_t_index=t_index

    end subroutine read_next_timestep_from_file

  
    subroutine open_file(i)

        integer, intent(in) :: i

        integer :: dimid_time,time_len,varid_time

        fname_sst=FileListSST(i)

        inquire(file=fname_sst,exist=file_found)
        if ( .not.file_found ) return

        write(LOGLUN,*) 'open SST file ',trim(fname_sst)
        call nf90_chkerr(nf90_open(fname_sst,NF90_NOWRITE,ncid_sst))

        fname_sic=FileListSIC(i)
        write(LOGLUN,*) 'open SIC file ',trim(fname_sic)
        call nf90_chkerr(nf90_open(fname_sic,NF90_NOWRITE,ncid_sic))

        ! length of time dimension
        call nf90_chkerr(nf90_inq_dimid(ncid_sst,'time',dimid_time))
        call nf90_chkerr(nf90_inquire_dimension(ncid_sst,dimid_time,len=time_len))
        if (allocated(t_sst)) deallocate(t_sst)
        allocate(t_sst(time_len))

        ! get time variable
        ! assume time variables are identical in sst and sic file
        ! therefore use only t_sst
        call nf90_chkerr(nf90_inq_varid(ncid_sst,'time',varid_time))
        call nf90_chkerr(nf90_get_var(ncid_sst,varid_time,t_sst))

        ! get varid
        call nf90_chkerr(nf90_inq_varid(ncid_sst,'tos',varid_sst))
        call nf90_chkerr(nf90_inq_varid(ncid_sic,'sic',varid_sic))

    end subroutine open_file


    subroutine read_file

        write(LOGLUN,*) '...read timestep',t_index,t_sst(t_index)
        call nf90_chkerr(nf90_get_var(ncid_sst,varid_sst,sst(:,:,2), &
            & start=(/1,1,t_index/),count=(/GridNX,GridNY,1/)))
        call nf90_chkerr(nf90_get_var(ncid_sic,varid_sic,sic(:,:,2), &
            & start=(/1,1,t_index/),count=(/GridNX,GridNY,1/)))

    end subroutine read_file


    subroutine close_file(i)

        integer, intent(in) :: i

        if ( file_found ) then
            write(LOGLUN,*) 'close SST file ',trim(fname_sst)
            call nf90_chkerr(nf90_close(ncid_sst))

            write(LOGLUN,*) 'close SIC file ',trim(fname_sic)
            call nf90_chkerr(nf90_close(ncid_sic))
        endif

    end subroutine close_file


    subroutine nf90_chkerr(iret)
        integer,intent(in) :: iret
        if (iret /= NF90_NOERR) call ERROR(nf90_strerror(iret))
    end subroutine nf90_chkerr


end module iceberg_mod
