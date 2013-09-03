#define ESMF_CHECK if (ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#define ESMF_UCHECK if (ESMF_LogFoundError(rcToCheck=urc,msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

! Main program source file for demo swanfmapp.F90
!
! ESMF Application Wrapper for swan-fm.  This file contains the
!  main program, and creates a top level ESMF Gridded Component to contain
!  all other Components.
!
!
program ESMF_swanfm

  ! ESMF Uses CamelCase notation, so let's stick to that for all coupler code....
  ! ESMF module, defines all ESMF data types and methods
  use ESMF

  ! swanfm Component registration routines
  use swanfmMod, only : swanfm_register

  implicit none

  ! Configuration

  character(ESMF_MAXSTR) :: fname ! config file name
  ! input filenames
  character(ESMF_MAXSTR) :: mdu_filename
  character(ESMF_MAXSTR) :: mdw_filename

  type(ESMF_Config)   :: cf      ! the Config itself


  ! Component, and State
  type(ESMF_GridComp) :: swanfmComp  ! the coupled Component
  type(ESMF_State)    :: swanfmState ! the coupled State

  ! Clock, TimeInterval, and Times
  type(ESMF_Clock)        :: clock
  type(ESMF_TimeInterval) :: timeStep
  type(ESMF_Time)         :: startTime
  type(ESMF_Time)         :: stopTime

  ! Clock variables
  integer :: s_month, s_day, s_hour, s_min
  integer :: e_month, e_day, e_hour, e_min
  integer :: step_min

  ! Just some counters
  integer :: i, j

  ! We're not using an exhange grid, instead we're regridding twice
  ! For nesting the coupled component later we should also specify
  ! the grid of the combined model
  ! Just an empty grid for now
  type(ESMF_Grid) :: grid


  ! Return codes for error checks
  integer :: rc, urc


  ! Example of Initializing the Framework
  !
  !     The first call to ESMF must be the initialize method.   As part of
  !     initialization the default Calendar can be specified and options
  !     for logging can be set.
  !     Here we are setting the default Calendar to be Gregorian, and
  !     request default logging into seperate files for each PET:

  ! We need some kind of time loop for the combination of both models.
  ! This is it....

  ! Initialize ESMF, set the default calendar and log type.
  call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, rc=rc)
ESMF_CHECK



  ! Read configuration
  cf = ESMF_ConfigCreate(rc=rc)             ! Create the empty Config

  fname = "swanfm.ini"                  ! Name the Resource File
  call ESMF_ConfigLoadFile(cf, fname, rc=rc) ! Load the Resource File
ESMF_CHECK


  !
  !     The following piece of code provides an example of Clock creation used in
  !     the Demo.  Note that the Gregorian calendar was set as the default in
  !     the ESMF\_Initialize() call above.  As shown in this example, we first
  !     initialize a time interval (timestep) to 2 seconds:
  call ESMF_ConfigGetAttribute(cf, step_min, label='time_step', &
       default=1, rc=rc)
ESMF_CHECK
  call ESMF_TimeIntervalSet(timeStep, m=step_min, rc=rc)
ESMF_CHECK
  !And then we set the start time and stop time to input values for the month,
  !day, and hour (assuming the year to be 1970):
  s_month = 1
  s_day = 1
  s_hour = 0
  s_min = 0
  call ESMF_TimeSet(startTime, yy=1970, mm=s_month, dd=s_day, &
       h=s_hour, m=s_min, s=0, rc=rc)
ESMF_CHECK
  e_month = 1
  e_day = 1
  e_hour = 0
  ! Run for 2 minutes (for testing)
  call ESMF_ConfigGetAttribute(cf, e_min, label='time_stop', &
       default=1, rc=rc)
ESMF_CHECK
  call ESMF_TimeSet(stopTime, yy=1970, mm=e_month, dd=e_day, &
       h=e_hour, m=e_min, s=0, rc=rc)
ESMF_CHECK
!With the time interval, start time, and stop time set above, the Clock can
!now be created:
  clock = ESMF_ClockCreate(timeStep=timeStep, startTime=startTime, &
       stopTime=stopTime, rc=rc)
ESMF_CHECK

!Subsequent calls to ESMF_ClockAdvance with this clock will increment the
!current time from the start time by the timestep.


! Create the top level Gridded Component.
  swanfmComp = ESMF_GridCompCreate(name="swan-fm coupler", rc=rc)
ESMF_CHECK
  call ESMF_GridCompSetServices(swanfmComp, swanfm_register, &
       userRc=urc, rc=rc)
ESMF_CHECK
ESMF_UCHECK


! read input files
call ESMF_ConfigGetAttribute(cf, mdu_filename, label='mdu', &
     default='input', rc=rc)
ESMF_CHECK

call ESMF_ConfigGetAttribute(cf, mdw_filename, label='mdw', &
     default='input', rc=rc)
ESMF_CHECK

write(*,*) 'Setting on mdu', mdu_filename
! Store them in the component
call ESMF_AttributeSet(swanfmComp, name='mdu', value=mdu_filename, rc=rc)
ESMF_CHECK
call ESMF_AttributeSet(swanfmComp, name='mdw', value=mdw_filename, rc=rc)
ESMF_CHECK

  ! Create the Grid and attach it to the Component:
  !
  ! For now make an empty grid....
  ! We could put an exchange or distributed grid here...
  grid = ESMF_GridEmptyCreate(rc=rc)
ESMF_CHECK
  ! Set the Combined Grid in the Swanfm Component
  call ESMF_GridCompSet(swanfmComp, grid=grid, rc=rc)
ESMF_CHECK

swanfmState = ESMF_StateCreate(Name="swanfm state", rc=rc)
ESMF_CHECK


! This is where the run happens
! Initialize, Run, and Finalize
!
! Init, Run, and Finalize sections of the swanfm component
call ESMF_GridCompInitialize(swanfmComp, &
     importState=swanfmState, exportState=swanfmState, &
     clock=clock, userRc=urc, rc=rc)
ESMF_CHECK





ESMF_UCHECK
  call ESMF_GridCompRun(swanfmComp, &
       importState=swanfmState, exportState=swanfmState, &
       clock=clock, userRc=urc, rc=rc)
  !  print *, "fm-swan Component Run finished, rc =", rc, urc
ESMF_CHECK
ESMF_UCHECK
  call ESMF_GridCompFinalize(swanfmComp, &
       importState=swanfmState, exportState=swanfmState, &
       clock=clock, userRc=urc, rc=rc)
  !  print *, "fm-swan Component Finalize finished, rc =", rc, urc
ESMF_CHECK
ESMF_UCHECK



  ! Object Destruction
  !
  !     Near the end of the application, call object destroy methods to
  !     clean up the objects previously created:
  call ESMF_GridCompDestroy(swanfmComp, rc=rc)
ESMF_CHECK
  call ESMF_GridDestroy(grid, rc=rc)
ESMF_CHECK
  call ESMF_ClockDestroy(clock, rc=rc)
ESMF_CHECK




  print *, "SUCCESS!  The swanfm ", &
       "ran to completion!"
  print *, "See the output files in the source directory for ", &
       "the generated data."
  print *, "**********************************************************"

  !
  ! Example of ESMF Finalize
  !
  !Call ESMF_Finalize at the end of an ESMF application:
  call ESMF_Finalize()

end program ESMF_swanfm
