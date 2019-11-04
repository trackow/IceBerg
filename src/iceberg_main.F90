program iceberg

    use config_mod
    use iceberg_mod
    use cplng

    implicit none

    integer ::      RunLengthSec, TimeStepSec
    integer ::      StartYear, StartMonth, StartDay, FixYear
    namelist /NAMICEBERG/ RunLengthSec, TimeStepSec, StartYear, StartMonth, StartDay, FixYear, FileListSST, FileListSIC, LDebug, LInterpolate

    integer :: time
    logical :: LUNnotFree

    ! Default settings, can be overwritten from namelist
    LInterpolate=.true.
    LDebug=.false.
    FixYear=0

    ! open logfile
    open(LOGLUN,file='iceberg.log',status='NEW')

    call cplng_init
    call cplng_config


    ! Read RunLengthSec,TimeStepSec from namelist
    inquire(NAMLUN,opened=LUNnotFree)
    if (LUNnotFree) call ERROR('Namelist LUN not free')
    open(NAMLUN,file=NamelistFileName,status='OLD')
    read(NAMLUN,nml=NAMICEBERG)
    close(NAMLUN)

    call setup_iceberg(StartYear,StartMonth,StartDay,TimeStepSec)

    ! Time loop
    ! Note that according to OASIS convention, the first time step is at time=0
    ! and the last one is at runlength-timestep (see OASIS documentation)
    do time=0,RunLengthSec-TimeStepSec,TimeStepSec

        call cplng_exchange(time,cplng_stage_rec)

        call update_position_melt

       ! call cplng_exchange(time,cplng_stage_snd)

    enddo

    call finalise_iceberg

    call cplng_finalize

    close(LOGLUN)

end program iceberg
