MODULE CPLNG_EXCHANGE_MOD

    IMPLICIT NONE

    PRIVATE

    PUBLIC CPLNG_EXCHANGE

CONTAINS

SUBROUTINE CPLNG_EXCHANGE(KTIME_IN_SECONDS,KSTAGE)

    USE MOD_OASIS
    USE CPLNG_DATA_MOD

    ! Argument
    INTEGER, INTENT(IN) :: KTIME_IN_SECONDS
    INTEGER, INTENT(IN) :: KSTAGE

    ! Locals
    INTEGER          :: II
    INTEGER          :: IERROR
    CHARACTER(LEN=3) :: CERRSTR

    ! Early return if KSTAGE is set to ignore
    IF (KSTAGE==CPLNG_STAGE_IGNORE) RETURN

    DO II=1,SIZE(CPLNG_FLD)

        ! Do nothing if this field is not exchanged in the current stage
        IF (CPLNG_FLD(II)%STAGE/=KSTAGE) CYCLE

        ! Decide whether the coupling field is to be sent (put) or
        ! received (get)
        SELECT CASE (CPLNG_FLD(II)%INOUT)

        CASE (OASIS_Out) ! Call OASIS_PUT for couple fields that are sent

            CALL OASIS_PUT(CPLNG_FLD(II)%ID, &
            &              KTIME_IN_SECONDS, &
            &              CPLNG_FLD(II)%D,  &
            &              IERROR)

            SELECT CASE (IERROR)

            CASE (OASIS_Sent,      &
            &     OASIS_LocTrans,  &
            &     OASIS_ToRest,    &
            &     OASIS_Output,    &
            &     OASIS_SentOut,   &
            &     OASIS_ToRestOut, &
            &     OASIS_Waitgroup, &
            &     OASIS_Ok         )

                CONTINUE

            CASE DEFAULT

                WRITE (CERRSTR,'(I3)') IERROR
                CALL OASIS_ABORT(CPLNG_COMP_ID,CPLNG_COMP_NAME,"CPLNG_EXCHANGE: Error in OASIS_PUT: "//CERRSTR)

            END SELECT

        CASE (OASIS_In) ! Call OASIS_GET for couple fields that are received

            CALL OASIS_GET(CPLNG_FLD(II)%ID, &
            &              KTIME_IN_SECONDS, &
            &              CPLNG_FLD(II)%D,  &
            &              IERROR)

            SELECT CASE (IERROR)

            CASE (OASIS_Recvd,       &
            &     OASIS_FromRest,    &
            &     OASIS_Input,       &
            &     OASIS_RecvOut,     &
            &     OASIS_FromRestOut, &
            &     OASIS_Ok           )

                CONTINUE

            CASE DEFAULT

                WRITE (CERRSTR,'(I3)') IERROR
                CALL OASIS_ABORT(CPLNG_COMP_ID,CPLNG_COMP_NAME,"CPLNG_EXCHANGE: Error in OASIS_GET: "//CERRSTR)

            END SELECT

        CASE DEFAULT ! Everthing else is an error

            WRITE (CERRSTR,'(I3,A,I5,A)') II,' (INOUT=',CPLNG_FLD(II)%INOUT,')'
            CALL OASIS_ABORT(CPLNG_COMP_ID,CPLNG_COMP_NAME,"CPLNG_EXCHANGE: Error in definition of field no. "//CERRSTR)

        END SELECT

    ENDDO

END SUBROUTINE CPLNG_EXCHANGE

END MODULE CPLNG_EXCHANGE_MOD
