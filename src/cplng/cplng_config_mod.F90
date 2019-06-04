MODULE CPLNG_CONFIG_MOD

    IMPLICIT NONE

    PRIVATE

    PUBLIC CPLNG_CONFIG

CONTAINS

SUBROUTINE CPLNG_CONFIG

    USE CONFIG_MOD
    USE MOD_OASIS
    USE CPLNG_DATA_MOD

    TYPE NODE_TYPE
        TYPE(CPLNG_FLD_TYPE),POINTER :: THIS
        TYPE(NODE_TYPE),     POINTER :: NEXT
    END TYPE NODE_TYPE
    TYPE(NODE_TYPE),POINTER :: TMP_FLD_LIST=>NULL()
    TYPE(NODE_TYPE),POINTER :: PTR

    ! Variables needed in OASIS calls
    INTEGER :: IERROR
    INTEGER :: IG_PARAL(3), IL_PART_ID
    INTEGER :: IVAR_NODIMS(2)
    INTEGER :: IVAR_ACTUAL_SHAPE(2)
    INTEGER :: II
    INTEGER :: ILVL,ICAT
    CHARACTER(LEN=128) :: CFLDNAME
    CHARACTER(LEN=3)   :: CERRSTR

    ! -------------------------------------------------------------------------
    ! * (1) CONFIGURE COUPLING FIELDS
    ! -------------------------------------------------------------------------

    ! Fields recieved by IceBerg coming from the ocean or atmosphere. Note that 
    ! for the definition of stages it is not important if e.g. wind speed 
    ! components are coming from the atmosphere or the from ocean model that is 
    ! driven by atmosphere file input.
    CALL ADD_FLD('iceberg_sst',OASIS_OUT,CPLNG_STAGE_REC)
    CALL ADD_FLD('iceberg_ssi',OASIS_OUT,CPLNG_STAGE_REC)
    CALL ADD_FLD('iceberg_ssh',OASIS_OUT,CPLNG_STAGE_REC)
    CALL ADD_FLD('iceberg_u_oce',OASIS_OUT,CPLNG_STAGE_REC)
    CALL ADD_FLD('iceberg_v_oce',OASIS_OUT,CPLNG_STAGE_REC)
    CALL ADD_FLD('iceberg_u_atm',OASIS_OUT,CPLNG_STAGE_REC)
    CALL ADD_FLD('iceberg_v_atm',OASIS_OUT,CPLNG_STAGE_REC)
    CALL ADD_FLD('iceberg_u_ice',OASIS_OUT,CPLNG_STAGE_REC)
    CALL ADD_FLD('iceberg_v_ice',OASIS_OUT,CPLNG_STAGE_REC)

    CALL ADD_FLD('iceberg_melt',OASIS_OUT,CPLNG_STAGE_SǸD)

    ! -------------------------------------------------------------------------
    ! * (2) Provide array of coupling fields (CPLNG_FLD)
    ! -------------------------------------------------------------------------
    IF (.NOT.ASSOCIATED(TMP_FLD_LIST)) THEN
        CALL OASIS_ABORT(CPLNG_COMP_ID,CPLNG_COMP_NAME,"CPLNG_CONFIG: No coupling fields configured.")
    ENDIF

    ! Count members in TMP_FLD_LIST
    II = 1
    PTR => TMP_FLD_LIST
    DO WHILE(ASSOCIATED(PTR%NEXT))
        II = II+1
        PTR => PTR%NEXT
    ENDDO

    ALLOCATE(CPLNG_FLD(II))

    ! Copy fields to array, destroy temporary list
    DO II=1,SIZE(CPLNG_FLD)
        CPLNG_FLD(II) = TMP_FLD_LIST%THIS
        PTR          => TMP_FLD_LIST
        TMP_FLD_LIST => TMP_FLD_LIST%NEXT
        DEALLOCATE(PTR)
    ENDDO

    ! -------------------------------------------------------------------------
    ! * (3) SET UP OASIS PARTITION
    ! -------------------------------------------------------------------------

    IG_PARAL(1:3) = (/ 0,0,GridNX*GridNY /)

    ! Define partition for OASIS
    CALL OASIS_DEF_PARTITION(IL_PART_ID,IG_PARAL,IERROR)
    IF (IERROR/=OASIS_OK) THEN
        WRITE (CERRSTR,'(I3)') IERROR
        CALL OASIS_ABORT(CPLNG_COMP_ID,CPLNG_COMP_NAME,"CPLNG_CONFIG: Error on OASIS_DEF_PARTITION (gridpoint): "//CERRSTR)
    ENDIF

    ! -------------------------------------------------------------------------
    ! * (4) DEFINE COUPLING FIELDS FOR OASIS
    ! -------------------------------------------------------------------------
    IVAR_NODIMS = (/ 2, 1 /)
    IVAR_ACTUAL_SHAPE = (/ GridNX, GridNY /)

    DO II=1,SIZE(CPLNG_FLD)

        ! Allocate data member and initialise to a strange value
        ALLOCATE(CPLNG_FLD(II)%D(GridNX,GridNY))
        CPLNG_FLD(II)%D = -HUGE(0.0_RNP)

        CALL OASIS_DEF_VAR(CPLNG_FLD(II)%ID,         &
        &                  TRIM(CPLNG_FLD(II)%NAME), &
        &                  IL_PART_ID,               &
        &                  IVAR_NODIMS,              &
        &                  CPLNG_FLD(II)%INOUT,      &
        &                  IVAR_ACTUAL_SHAPE,        &
        &                  OASIS_REAL,               &
        &                  IERROR                    )
        IF (IERROR/=OASIS_OK) THEN
            WRITE (CERRSTR,'(I3)') IERROR
            CALL OASIS_ABORT(CPLNG_COMP_ID,CPLNG_COMP_NAME,"CPLNG_CONFIG: Error in OASIS_DEF_VAR: " &
            &                                  // CERRSTR // " (" // TRIM(CPLNG_FLD(II)%NAME) // ")")
        ENDIF
    ENDDO

    ! -------------------------------------------------------------------------
    ! * (5) FINALISE OASIS DEFINITION PHASE
    ! -------------------------------------------------------------------------
    CALL OASIS_ENDDEF(IERROR)
    IF (IERROR/=OASIS_OK) THEN
        WRITE (CERRSTR,'(I3)') IERROR
        CALL OASIS_ABORT(CPLNG_COMP_ID,CPLNG_COMP_NAME,"CPLNG_CONFIG: Error in OASIS_ENDDEF: "//CERRSTR)
    ENDIF

contains

    SUBROUTINE ADD_FLD(NAME,INOUT,STAGE)

        ! Arguments
        CHARACTER(LEN=*),INTENT(IN) :: NAME
        INTEGER,         INTENT(IN) :: INOUT
        INTEGER,         INTENT(IN) :: STAGE

        ! Locals
        TYPE(NODE_TYPE),POINTER :: NEW,PTR

        ALLOCATE(NEW)
        NEW%NEXT => NULL()

        ALLOCATE(NEW%THIS)
        NEW%THIS%NAME  = NAME
        NEW%THIS%INOUT = INOUT
        NEW%THIS%STAGE = STAGE

        IF (ASSOCIATED(TMP_FLD_LIST)) THEN
            PTR => TMP_FLD_LIST
            DO WHILE(ASSOCIATED(PTR%NEXT))
                PTR => PTR%NEXT
            ENDDO
            PTR%NEXT => NEW
        ELSE
            TMP_FLD_LIST => NEW
        ENDIF

    END SUBROUTINE ADD_FLD

END SUBROUTINE CPLNG_CONFIG

END MODULE CPLNG_CONFIG_MOD
