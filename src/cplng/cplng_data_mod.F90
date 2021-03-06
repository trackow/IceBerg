MODULE CPLNG_DATA_MOD

    USE CONFIG_MOD

    IMPLICIT NONE

    PRIVATE

    PUBLIC RNP

    PUBLIC CPLNG_FLD_TYPE
    PUBLIC CPLNG_FLD
    PUBLIC CPLNG_IDX

    PUBLIC CPLNG_COMP_ID
    PUBLIC CPLNG_COMP_NAME

    PUBLIC CPLNG_STAGE_IGNORE
    PUBLIC CPLNG_STAGE_SND
    PUBLIC CPLNG_STAGE_REC

    TYPE CPLNG_FLD_TYPE
        INTEGER                    :: ID
        CHARACTER(LEN=128)         :: NAME
        INTEGER                    :: INOUT
        INTEGER                    :: STAGE
        REAL(KIND=RNP),ALLOCATABLE :: D(:,:)
    END TYPE CPLNG_FLD_TYPE

    TYPE(CPLNG_FLD_TYPE),ALLOCATABLE,TARGET :: CPLNG_FLD(:)

    INTEGER,PARAMETER :: CPLNG_STAGE_IGNORE  = 0
    INTEGER,PARAMETER :: CPLNG_STAGE_SND = 1
    INTEGER,PARAMETER :: CPLNG_STAGE_REC = 2

    INTEGER                    :: CPLNG_COMP_ID
    CHARACTER(LEN=8),PARAMETER :: CPLNG_COMP_NAME = 'ICEBERG'

CONTAINS

FUNCTION CPLNG_IDX(FLD_NAME)

    USE MOD_OASIS, ONLY: OASIS_ABORT

    ! Argument
    CHARACTER(LEN=*), INTENT(IN) :: FLD_NAME
    ! Result
    INTEGER :: CPLNG_IDX
    ! Locals
    INTEGER :: II

    IF (.NOT.ALLOCATED(CPLNG_FLD)) THEN
        CALL OASIS_ABORT(CPLNG_COMP_ID,CPLNG_COMP_NAME,"CPLNG_IDX: CPLNG_FLD not allocated upon call")
    ENDIF

    ! Simple loop searching for the right field name
    DO II=1,SIZE(CPLNG_FLD)
        IF (TRIM(FLD_NAME) == TRIM(CPLNG_FLD(II)%NAME)) EXIT
    ENDDO

    IF (II>SIZE(CPLNG_FLD)) THEN
        CALL OASIS_ABORT(CPLNG_COMP_ID,CPLNG_COMP_NAME,"CPLNG_IDX: Field name not found: "//TRIM(FLD_NAME))
    ENDIF

    CPLNG_IDX = II
END FUNCTION CPLNG_IDX

END MODULE CPLNG_DATA_MOD
