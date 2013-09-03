#define ESMF_CHECK if (ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#define ESMF_UCHECK if (ESMF_LogFoundError(rcToCheck=urc,msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
! include configuration
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

module swanfmMod

  ! ESMF module, defines all ESMF data types and procedures
  use ESMF

  ! User Component registration routines
  use   swan_esmf, only : swan_register
  use   unstruc_esmf, only : unstruc_register
  use   swanfmCouplerMod, only :  swanfmCoupler_register

  implicit none

  private

  ! Subcomponents
  type(ESMF_GridComp), save :: swancomp, dflowfmcomp
  type(ESMF_CplComp), save :: swanfmcpl

  ! States
  type(ESMF_State), save :: swanimp, swanexp, dflowfmimp, dflowfmexp

  ! Public entry point
  public swanfm_register

  !------------------------------------------------------------------------------

contains

  subroutine swanfm_register(comp, rc)

    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc



    rc = ESMF_SUCCESS ! initialize


    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, &
         userRoutine=swanfm_init, rc=rc)
ESMF_CHECK
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, &
         userRoutine=swanfm_run, rc=rc)
ESMF_CHECK
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, &
         userRoutine=swanfm_final, rc=rc)
ESMF_CHECK

  end subroutine swanfm_register

  subroutine swanfm_init(gcomp, importState, exportState, clock, rc)

    type(ESMF_GridComp)  :: gcomp ! Do we make this? I think so....
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    integer :: urc ! return codes from models
    ! Is this where the grids should reside?
    type(ESMF_Grid) :: gridSwan, gridFm, gridCouple
    character(ESMF_MAXSTR) :: filename

    rc = ESMF_SUCCESS ! initialize


    swancomp = ESMF_GridCompCreate(name="swan model", rc=rc)
ESMF_CHECK
    dflowfmcomp = ESMF_GridCompCreate(name="dflowfm model", rc=rc)
ESMF_CHECK
    swanfmcpl = ESMF_CplCompCreate(name="swan-fm coupler", rc=rc)
ESMF_CHECK

    swanimp = ESMF_StateCreate(name="swan import", stateintent=ESMF_STATEINTENT_IMPORT, &
         rc=rc)
ESMF_CHECK
    swanexp = ESMF_StateCreate(name="swan export", stateintent=ESMF_STATEINTENT_EXPORT, &
         rc=rc)
ESMF_CHECK
    dflowfmimp = ESMF_StateCreate(name="dflowfm import", stateintent=ESMF_STATEINTENT_IMPORT, &
         rc=rc)
ESMF_CHECK
    dflowfmexp = ESMF_StateCreate(name="dflowfm export", stateintent=ESMF_STATEINTENT_EXPORT, &
         rc=rc)
ESMF_CHECK

! Pass on attributes
    call ESMF_AttributeGet(gcomp, name="mdu", value=filename,rc=rc)
ESMF_CHECK
    call ESMF_AttributeSet(dflowfmcomp, name="mdu", value=filename, rc=rc)
ESMF_CHECK
    call ESMF_AttributeGet(gcomp, name="mdw", value=filename,rc=rc)
ESMF_CHECK
    call ESMF_AttributeSet(swancomp, name="mdw", value=filename, rc=rc)
ESMF_CHECK




    call ESMF_GridCompSetServices(swancomp, swan_register, userRc=urc, &
         rc=rc)
ESMF_CHECK
    call ESMF_GridCompSetServices(dflowfmcomp, unstruc_register, userRc=urc, &
         rc=rc)
ESMF_CHECK
    call ESMF_CplCompSetServices(swanfmcpl, swanfmcoupler_register, userRc=urc, &
         rc=rc)
ESMF_CHECK


! make empty grids
    gridFm = ESMF_GridEmptyCreate(rc=rc)
ESMF_CHECK
    gridSwan = ESMF_GridEmptyCreate(rc=rc)
ESMF_CHECK
! Set to the components
    call ESMF_GridCompSet(dflowfmcomp, grid=gridFm, rc=rc)
ESMF_CHECK
    call ESMF_GridCompSet(swancomp, grid=gridSwan, rc=rc)
ESMF_CHECK
! Initialize
    call ESMF_GridCompInitialize(swancomp, clock=clock, importstate=swanimp, exportstate=swanexp, rc=rc, userRc=urc)
ESMF_CHECK
ESMF_UCHECK
    call ESMF_GridCompInitialize(dflowfmcomp, importState=dflowfmimp, exportstate=dflowfmexp, rc=rc, userRc=urc)
ESMF_CHECK
ESMF_UCHECK


call ESMF_GridCompGet(swancomp, importstate=swanimp, exportstate=swanexp, rc=rc)
ESMF_CHECK
call ESMF_GridCompGet(dflowfmcomp, importstate=dflowfmimp, exportstate=dflowfmexp, rc=rc)
ESMF_CHECK
! ! Reconcile
!     call ESMF_StateReconcile(dflowfmimp, rc=rc)
! ESMF_CHECK
!     call ESMF_StateReconcile(dflowfmexp, rc=rc)
! ESMF_CHECK
!     call ESMF_StateReconcile(swanimp, rc=rc)
! ESMF_CHECK
!     call ESMF_StateReconcile(swanexp, rc=rc)
! ESMF_CHECK

! Create coupling (two way)
    call ESMF_CplCompInitialize(swanfmcpl, importstate=dflowfmexp, exportstate=swanimp, clock=clock, rc=rc, userRc=urc)
ESMF_CHECK
ESMF_UCHECK
    call ESMF_CplCompInitialize(swanfmcpl, importstate=swanexp, exportstate=dflowfmimp, clock=clock, rc=rc, userRc=urc)
ESMF_CHECK
ESMF_UCHECK
  end subroutine swanfm_init

  !------------------------------------------------------------------------------
  !BOPI
  ! !IROUTINE: swanfm_run - run routine

  ! !INTERFACE:
  subroutine swanfm_run(comp, importState, exportState, clock, rc)
    !
    ! !ARGUMENTS:
    type(ESMF_GridComp)  :: comp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    !
    ! !DESCRIPTION:
    !     User-supplied run routine.
    !
    !     The arguments are:
    !     \begin{description}
    !     \item[comp]
    !          Component.
    !     \item[importState]
    !          Importstate.
    !     \item[exportState]
    !          Exportstate.
    !     \item[clock]
    !          External clock.
    !     \item[{[rc]}]
    !          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
    !          otherwise {\tt ESMF\_FAILURE}.
    !     \end{description}
    !
    !EOPI

    ! Local variables
    type(ESMF_Clock) :: localclock
    integer          :: urc

    type(ESMF_Grid) :: xdgrid, xbgrid
    type(ESMF_XGrid) :: xgrid
    type(ESMF_XGridSpec), allocatable :: sparseMatA2X(:)

    !BOE
    !
    ! !DESCRIPTION:
    ! \subsubsection{Example of Time Stepping Loop}
    !
    ! Advancing in time with ESMF clock, the coupled swanfm component calls
    ! the run methods of the gridded components and coupler component sequentially:
    !BOC
    ! Make our own local copy of the clock
    localclock = ESMF_ClockCreate(clock, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !     print *, "Run Loop Start time"
    !     call ESMF_ClockPrint(localclock, options="currtime string", rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    do while (.not. ESMF_ClockIsStopTime(localclock, rc=rc))

       ! Water before sand, each timestep....

       ! Should I update the timestep here???
       ! I think the gridcomprun should take care of this... The clock has the timestep.

       !        ! print the local time
       !        call ESMF_ClockPrint(localclock, options="currtime string", rc=rc)

       ! Run dflowfm Component
       write(*,*) 'Calling dflowfm run'
       call ESMF_GridCompRun(dflowfmcomp, importState=dflowfmimp, exportState=dflowfmexp, &
            clock=localclock, rc=rc, userRc=urc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
       if(urc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=urc)

       ! Run swan Component
       write(*,*) 'Calling Dune run'
       call ESMF_GridCompRun(swancomp, importState=swanimp, exportState=swanexp, &
            clock=localclock, rc=rc, userRc=urc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
       if(urc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=urc)

       ! Couple export state of fm to import of swan
       write(*,*) 'Calling XB->Dune'
       call ESMF_CplCompRun(swanfmcpl, importState=dflowfmexp, exportState=swanimp, &
            clock=localclock, rc=rc, userRc=urc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
       if(urc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=urc)

       ! Couple export state of swan to import of fm
       write(*,*) 'Calling Dune->XB'
       call ESMF_CplCompRun(swanfmcpl, importState=swanexp, exportState=dflowfmimp, &
            clock=localclock, rc=rc, userRc=urc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
       if(urc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=urc)

       ! Advance the time
       call ESMF_ClockAdvance(localclock, rc=rc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

       ! This demo runs a lot of time steps and only outputs files
       !        ! every N iterations.  This print statement, if commented in,
       ! generates a lot of output.
       !        !call ESMF_ClockPrint(localclock, "currtime string", rc)

    enddo

    !     print *, "Run Loop End time"
    !     call ESMF_ClockPrint(localclock, options="currtime string", rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    !EOC
    !EOE

    !BOE
    !
    ! !DESCRIPTION:
    ! \subsubsection{Example of Clock Destruction}
    !
    ! At the end of run method, destroy the clock used to iterate through time:
    !BOC
    call ESMF_ClockDestroy(localclock, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    !EOC
    !EOE

  end subroutine swanfm_run


  !------------------------------------------------------------------------------
  !BOPI
  ! !IROUTINE:  coupledflow_final - user supplied finalize routine

  ! !INTERFACE:
  subroutine swanfm_final(comp, importState, exportState, clock, rc)
    !
    ! !ARGUMENTS:
    type(ESMF_GridComp)  :: comp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    !
    ! !DESCRIPTION:
    !     User-supplied finalize routine.
    !
    !     The arguments are:
    !     \begin{description}
    !     \item[comp]
    !          Component.
    !     \item[importState]
    !          Importstate.
    !     \item[exportState]
    !          Exportstate.
    !     \item[clock]
    !          External clock.
    !     \item[{[rc]}]
    !          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
    !          otherwise {\tt ESMF\_FAILURE}.
    !     \end{description}
    !
    !EOPI

    integer :: urc

    ! First finalize all subcomponents

    ! Finalize swan Component
    call ESMF_GridCompFinalize(swancomp, importState=swanimp, exportState=swanexp, &
         clock=clock, rc=rc, userRc=urc)
    if (rc .ne. ESMF_SUCCESS .or. urc .ne. ESMF_SUCCESS) then
       !          print *, "Injector Component Finalize routine returned error"
       return
    endif

    ! Finalize FlowSolver Component
    call ESMF_GridCompFinalize(dflowfmcomp, importState=dflowfmimp, exportState=dflowfmimp, &
         clock=clock, rc=rc, userRc=urc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if(urc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=urc)

    ! Finalize Coupler
    call ESMF_CplCompFinalize(swanfmcpl, importState=swanexp, exportState=dflowfmimp, &
         clock=clock, rc=rc, userRc=urc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if(urc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=urc)

    !      print *, "CoupledFlowMod finished calling all subcomponent Finalize routines"

    ! Then clean them up

    !      print *, "ready to destroy all states"
    call ESMF_StateDestroy(swanimp, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_StateDestroy(swanexp, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_StateDestroy(dflowfmimp, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_StateDestroy(dflowfmexp, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !      print *, "ready to destroy all components"
    call ESMF_GridCompDestroy(swancomp, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridCompDestroy(dflowfmcomp, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_CplCompDestroy(swanfmcpl, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !      print *, "end of CoupledFlowMod Finalization routine"
    rc = ESMF_SUCCESS

  end subroutine swanfm_final


end module swanfmMod
