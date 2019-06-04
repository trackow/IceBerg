program amip_forcing

    use config_mod
    use amip_forcing_mod
    use cplng

    implicit none

    integer ::      RunLengthSec, TimeStepSec
    integer ::      StartYear, StartMonth, StartDay, FixYear
    namelist /NAMAMIP/ RunLengthSec, TimeStepSec, StartYear, StartMonth, StartDay, FixYear, FileListSST, FileListSIC, LDebug, LInterpolate

    integer :: time
    logical :: LUNnotFree

    ! Default settings, can be overwritten from namelist
    LInterpolate=.true.
    LDebug=.false.
    FixYear=0

    ! open logfile
    open(LOGLUN,file='amip.log',status='NEW')

    call cplng_init
    call cplng_config


    ! Read RunLengthSec,TimeStepSec from namelist
    inquire(NAMLUN,opened=LUNnotFree)
    if (LUNnotFree) call ERROR('Namelist LUN not free')
    open(NAMLUN,file=NamelistFileName,status='OLD')
    read(NAMLUN,nml=NAMAMIP)
    close(NAMLUN)

    ! FixYear>0 sets a perpetual forcing for year FixYear
    ! This fix works properly as long as a chunk of the simulation
    ! is not longer than 1 year
    if ( FixYear <= 0 ) then
        call setup_amip_forcing(StartYear,StartMonth,StartDay,TimeStepSec)
    else
        call error('FixYear not implemented yet - stopping')
    endif

    ! Time loop
    ! Note that according to OASIS convention, the first time step is at time=0
    ! and the last one is at runlength-timestep (see OASIS documentation)
    do time=0,RunLengthSec-TimeStepSec,TimeStepSec

       ! Currently nothing (no signal) is received from the atmosphere.
       ! This could potentially lead to problems in the case of very long
       ! runs with high resolution, in that case a buffer (Oasis?) will hold
       ! hold a large amount of data from the AMIP_READER that haven't been
       ! fetched by the atmopshere model yet.
       !
       ! call cplng_exchange(time,cplng_stage_rcv_atm)

        call update_sst_and_sic

        call cplng_exchange(time,cplng_stage_snd_atm)

    enddo

    call finalise_amip_forcing

    call cplng_finalize

    close(LOGLUN)

end program amip_forcing
