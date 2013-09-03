#define ESMF_CHECK if (ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#define ESMF_UCHECK if (ESMF_LogFoundError(rcToCheck=urc,msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
! $Id: CouplerMod.F90,v 1.13 2011/06/30 05:58:24 theurich Exp $
!
!-------------------------------------------------------------------------
!BOP
!
! \subsection{Source for 2-way Coupler Component CouplerMod.F90}
!
! !DESCRIPTION:
!  The Coupler Component provides two-way coupling between the Injector
!  and FlowSolver Models.  During initialization this Component is
!  responsible for setting that data "is needed" from the export state
!  of each model.  In its run routine it calls route to transfer the
!  needed data directly from one Component's export state to the other
!  Component's import state.
!
!
!EOP

module  SwanFMCouplerMod

  ! ESMF Framework module - defines ESMF data types and procedures
  use ESMF

  ! Import all known esmf modules
  use unstruc_esmf
  use swan_esmf

  implicit none

  private
  type(ESMF_RouteHandle),save :: fromfm_rh, fromswan_rh

  ! Public entry point
  public  SwanFMCoupler_register

contains


  !------------------------------------------------------------------------------
  !BOPI
  ! !IROUTINE: swanfmcoupler_register - public SetServices entry point

  ! !INTERFACE:
  subroutine  SwanFMCoupler_register(comp, rc)
    !
    ! !ARGUMENTS:
    type(ESMF_CplComp)   :: comp
    integer, intent(out) :: rc
    !
    ! !DESCRIPTION:
    !     User-supplied setservices routine.
    !
    !     The arguments are:
    !     \begin{description}
    !     \item[comp]
    !          Component.
    !     \item[rc]
    !          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
    !          otherwise {\tt ESMF\_FAILURE}.
    !     \end{description}
    !
    !EOPI

    ! because none of the arguments to this subroutine will ever be optional,
    ! go ahead and set rc to an initial return code before using it below.
    ! (this makes some eager error-checking compilers happy.)
    rc = ESMF_FAILURE

    ! Register the callback routines.

    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, userRoutine=swanfmcoupler_init, rc=rc)
    ESMF_CHECK
    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_RUN, userRoutine=swanfmcoupler_run, rc=rc)
    ESMF_CHECK
    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, userRoutine=swanfmcoupler_final, rc=rc)
    ESMF_CHECK

    print *, "swan-fm coupler module: Registered Initialize, Run, and Finalize routines"

  end subroutine SwanFMCoupler_register


  !------------------------------------------------------------------------------
  !BOPI
  ! !IROUTINE: swanfmcoupler_init - swanfmcoupler init routine

  ! !INTERFACE:
  subroutine swanfmcoupler_init(comp, importState, exportState, clock, rc)

    !
    ! !ARGUMENTS:
    type(ESMF_CplComp)   :: comp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    character(len=100)         :: name, tofieldname, fromfieldname, &
         importstatename, exportstatename

    type(ESMF_Field)     :: tofield, fromfield
    integer(ESMF_KIND_I4), pointer :: indices(:,:)
    real(ESMF_KIND_R8), pointer :: weights(:)

    !
    ! !DESCRIPTION:
    !     User-supplied init routine.
    !
    !     The arguments are:
    !     \begin{description}
    !     \item[comp]
    !          Component.
    !     \item[importState]
    !          Nested state object containing import data.
    !     \item[exportState]
    !          Nested state object containing export data.
    !     \item[clock]
    !          External clock.
    !     \item[rc]
    !          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
    !          otherwise {\tt ESMF\_FAILURE}.
    !     \end{description}
    !
    !EOPI
    call ESMF_StateGet(importstate, name=name, rc=rc)

    ! The import of this component is the export of fm/Dune component and vice versa
    if (trim(name) .eq. 'dflowfm export') then
       fromfieldname = "s1"
       tofieldname = "WLEVL"
    elseif (trim(name) .eq. 'swan export') then
       fromfieldname = "WLEVL"
       tofieldname = "s1"
    end if

    call ESMF_StatePrint(importstate,  rc=rc)
    ! Get Fields...
    call ESMF_StateGet(importState, fromfieldname , field=fromfield, rc=rc)
    ESMF_CHECK

    call ESMF_StateGet(exportState, tofieldname, field=tofield, rc=rc)
    ESMF_CHECK
    call ESMF_FieldWrite(tofield, trim(tofieldname) // 'init' // '.nc', rc=rc)
    call ESMF_FieldWrite(fromfield, trim(fromfieldname) // 'init' // '.nc', rc=rc)


    if (trim(name) .eq. 'dflowfm export') then
       write(*,*) 'Regridstore ', trim(fromfieldname), '->', trim(tofieldname)
       call ESMF_FieldRegridStore(fromfield, tofield, &
            unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
            routehandle=fromfm_rh, rc=rc)
       ESMF_CHECK
       ! Indices and weights now contain the mapping between dzbdt to dh_dt

    else if(trim(name) .eq. 'swan export') then
       write(*,*) 'Regridstore ', trim(fromfieldname), '->', trim(tofieldname)
       call ESMF_FieldRegridStore(fromfield, tofield, &
            unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
            routehandle=fromswan_rh, weights=weights, indices=indices,  rc=rc)
       ESMF_CHECK
    end if
    !   ! Local variables
    rc = ESMF_SUCCESS
    print *, "swanfmcoupler Init returning"

  end subroutine swanfmcoupler_init


  !------------------------------------------------------------------------------
  !BOPI
  ! !IROUTINE: swanfmcoupler_run - swanfmcoupler run routine

  ! !INTERFACE:
  subroutine swanfmcoupler_run(comp, importState, exportState, clock, rc)

    !
    ! !ARGUMENTS:
    type(ESMF_CplComp)   :: comp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_Grid)      :: grid
    type(ESMF_Array)     :: coordarray
    integer(ESMF_KIND_I4),pointer :: srcMaskValues(:), dstMaskValues(:)
    integer(ESMF_KIND_I4), pointer :: indices(:,:)
    real(ESMF_KIND_R8), pointer :: weights(:)
    real(ESMF_KIND_R8), allocatable :: zscl(:) ! zs at the coastline
    character(len=100)         :: name, tofieldname, fromfieldname, &
         tototalfieldname, fromtotalfieldname, importstatename, exportstatename
    real  :: tstop, tstart
    integer(ESMF_KIND_I8) :: advanceCount, i, j
    integer(ESMF_KIND_I4) :: timeslice
    type(ESMF_Field)     :: tofield, fromfield, tototalfield, fromtotalfield, &
         zsexportfield, wetzexportfield, zsmeanimportfield
    real(ESMF_KIND_r8), pointer  :: farraydblptr(:,:)
    integer(ESMF_KIND_i4), pointer  :: farrayintptr(:,:)
    integer(ESMF_KIND_I4), allocatable  :: lastwetz(:)
    integer :: nx, ny, wetzcell
    real(ESMF_KIND_R8) :: zscumul, zsmean
    ! This should be a vector.... (n locations)
    real(ESMF_KIND_R8), pointer :: zsmeanfdbl_f_ptr(:)


    !
    ! !DESCRIPTION:
    !     User-supplied run routine.
    !
    !     The arguments are:
    !     \begin{description}
    !     \item[comp]
    !          Component.
    !     \item[importState]
    !          Nested state object containing import data.
    !     \item[exportState]
    !          Nested state object containing export data.
    !     \item[clock]
    !          External clock.
    !     \item[rc]
    !          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
    !          otherwise {\tt ESMF\_FAILURE}.
    !     \end{description}
    !
    !EOPI


    ! Define the field names.
    call ESMF_StateGet(importstate, name=name, rc=rc)
    ! The import of this component is the export of fm/swan component and vice versa
    if (trim(name) .eq. 'dflowfm export') then
       fromfieldname = "from"
       tofieldname = "to"
    elseif (trim(name) .eq. 'swan export') then
       fromfieldname = "from"
       tofieldname = "to"
    end if

    ! Get Fields...
    call ESMF_StateGet(importState, fromfieldname , field=fromfield, rc=rc)
    ESMF_CHECK
    call ESMF_StateGet(exportState, tofieldname, field=tofield, rc=rc)
    ESMF_CHECK
    call ESMF_StateGet(importState, fromtotalfieldname , field=fromtotalfield, rc=rc)
    ESMF_CHECK
    call ESMF_StateGet(exportState, tototalfieldname, field=tototalfield, rc=rc)
    ESMF_CHECK
    ! Get Fields...
    ! Compute the field bundle route handler (mapping)
    ! The routehandler could be reused but we're not because it might change on different wind directions

    call ESMF_ClockGet(clock, advanceCount=advanceCount, rc=rc)
    ESMF_CHECK
    ! This is a cast from a 8byte integer to a 4 byte integer....
    timeslice = transfer(advanceCount,timeslice) + 1
    call ESMF_FieldWrite(fromfield, trim(fromfieldname) // '.nc', timeslice=timeslice, rc=rc)
    call ESMF_FieldWrite(fromtotalfield, trim(fromtotalfieldname) // '.nc', timeslice=timeslice, rc=rc)

    ! Process wetz and zsexport into -> importfield???
    if (trim(name) .eq. 'dflowfm export') then
       call ESMF_StateGet(importState, 'zsexport', field=zsexportfield, rc=rc)
       ESMF_CHECK
       call ESMF_FieldWrite(zsexportfield, 'zsexport.nc', timeslice=timeslice, rc=rc)

       call ESMF_FieldGet(zsexportfield, localDe=0, farrayPtr=farraydblptr, &
            rc=rc) !totalLBound=ftlb, totalUBound=ftub, totalCount=ftc,
       ESMF_CHECK
       call ESMF_StateGet(importState, 'wetzexport', field=wetzexportfield, rc=rc)
       ESMF_CHECK
       call ESMF_FieldWrite(wetzexportfield, 'wetzexport.nc', timeslice=timeslice, rc=rc)
       ESMF_CHECK
       call ESMF_FieldGet(wetzexportfield, localDe=0, farrayPtr=farrayintptr, &
            rc=rc) !totalLBound=ftlb, totalUBound=ftub, totalCount=ftc,
       ESMF_CHECK

       allocate(lastwetz(nx+1))
       allocate(zscl(nx+1))
       lastwetz = 0
       zscl = 0
       ! Searching for waterlevel at seaside waterline
       do i=1,(nx+1)
          !  find the first dry cell
          do j=2,(ny+1)
             wetzcell = farrayintptr(i,j)
             if(wetzcell .eq. 0) exit
          enddo
          lastwetz(i) = j
          ! save the waterlevel in the last wet cell
          zscl(i) = farraydblptr(i,j-1)
       enddo

       zscumul = 0
       do i=1,(nx+1)
          zscumul = zscumul + zscl(i)
       enddo
       ! calculate mean waterlevel at waterline
       zsmean = zscumul / (nx+1)

       call ESMF_StatePrint(exportState, rc=rc)
       ESMF_CHECK


       ! Store in import field of dune (in the exportstate of this coupler)
       call ESMF_StateGet(exportState, "zsmeanimport" , field=zsmeanimportfield, rc=rc)
       ESMF_CHECK

       call ESMF_FieldGet(zsmeanimportfield, farrayPtr=zsmeanfdbl_f_ptr, rc=rc)
       ESMF_CHECK
       zsmeanfdbl_f_ptr = zsmean

       call ESMF_FieldWrite(zsmeanimportfield, 'zsmeanimport.nc', timeslice=timeslice, rc=rc)
       ESMF_CHECK
    end if



    if (trim(name) .eq. 'dflowfm export') then
       ! I hope this uses all the regridding options, like the weights and stuff....
       write(*,*) 'Regridding', trim(fromfieldname), '->', trim(tofieldname)
       call ESMF_FieldRegrid(fromfield, tofield, &
            fromfm_rh, rc=rc)
       ESMF_CHECK
    elseif (trim(name) .eq. 'swan export') then
       ! I hope this uses all the regridding options, like the weights and stuff....
       write(*,*) 'Regridding', trim(fromfieldname), '->', trim(tofieldname)
       call ESMF_FieldRegrid(fromfield, tofield, &
            fromswan_rh, rc=rc)
       ESMF_CHECK
    end if

    call ESMF_FieldWrite(tototalfield, trim(tototalfieldname) // '.nc', timeslice=timeslice, rc=rc)
    ESMF_CHECK
    call ESMF_FieldWrite(tofield, trim(tofieldname) // '.nc', timeslice=timeslice, rc=rc)
    ESMF_CHECK


    ! rc has the last error code already
    rc = ESMF_SUCCESS
  end subroutine swanfmcoupler_run


  !------------------------------------------------------------------------------
  !BOPI
  ! !IROUTINE:  swanfmcoupler_final - finalization routine

  ! !INTERFACE:
  subroutine swanfmcoupler_final(comp, importState, exportState, clock, rc)

    !
    ! !ARGUMENTS:
    type(ESMF_CplComp)   :: comp
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
    !          Nested state object containing import data.
    !     \item[exportState]
    !          Nested state object containing export data.
    !     \item[clock]
    !          External clock.
    !     \item[rc]
    !          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
    !          otherwise {\tt ESMF\_FAILURE}.
    !     \end{description}
    !
    !EOPI

    print *, "swanfmcoupler Final starting"

    ! none of the arguments to this subroutine will ever be optional, so
    ! go ahead and set rc to an initial return code before using it below.
    ! (this makes some eager error-checking compilers happy.)
    rc = ESMF_FAILURE

    ! Release some things here?
    rc = ESMF_SUCCESS

    print *, "swanfmcoupler Final returning"

  end subroutine swanfmcoupler_final


end module swanfmcouplerMod
