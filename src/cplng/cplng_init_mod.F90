MODULE CPLNG_INIT_MOD

    IMPLICIT NONE

    PRIVATE

    PUBLIC CPLNG_INIT

CONTAINS

SUBROUTINE CPLNG_INIT

    USE MOD_OASIS
    USE CPLNG_DATA_MOD

    INTEGER          :: NCOMPID
    INTEGER          :: IERROR
    CHARACTER(LEN=3) :: CERRSTR

    ! -------------------------------------------------------------------------
    ! * (2) Initialise OASIS coupling
    ! -------------------------------------------------------------------------
    CALL OASIS_INIT_COMP(CPLNG_COMP_ID,CPLNG_COMP_NAME,IERROR)
    IF (IERROR/=OASIS_OK) THEN
        WRITE (CERRSTR,'(I3)') IERROR
        CALL OASIS_ABORT(CPLNG_COMP_ID,CPLNG_COMP_NAME,"CPLNG_INIT: Error in OASIS_INIT_COMM: "//CERRSTR)
    ENDIF

END SUBROUTINE CPLNG_INIT

END MODULE CPLNG_INIT_MOD
