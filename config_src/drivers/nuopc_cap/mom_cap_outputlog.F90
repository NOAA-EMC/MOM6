! This file is part of MOM6, the Modular Ocean Model version 6.
! See the LICENSE file for licensing information.
! SPDX-License-Identifier: Apache-2.0
!!
!> @file mom_cap_outputlog.F90
!> @brief The outputlog feature for UFS
!!
!! This module contains a set of subroutines that check if MOM restart and history files
!! have been written and closed. This file is specific to UWM operational requirements
!! and configurations (eg specific output frequencies in hours) and may break if used outside
!! the scope of intended use.
!!
!! This module is a stub when CESMCOUPLED is defined
module MOM_cap_outputlog

#ifdef CESMCOUPLED
  use ESMF                  , only : ESMF_GridComp, ESMF_Clock, ESMF_SUCCESS
  use MOM_grid              , only : ocean_grid_type
implicit none; private

public :: outputlog_init, outputlog_run, outputlog_restart
contains
subroutine outputlog_init(gcomp, mclock, ocean_grid, rc)
  type(ESMF_GridComp)  :: gcomp                            !< an ESMF_GridComp object
  type(ESMF_Clock)     :: mclock                           !< the ESMF_clock for the model
  type(ocean_grid_type), pointer, intent(in) :: ocean_grid !< the ocean grid
  integer, intent(out) :: rc                               !< return code
  rc = ESMF_SUCCESS
end subroutine outputlog_init
subroutine outputlog_run(mclock, atStopTime, rc)
  type(ESMF_Clock)              :: mclock     !< the ESMF_clock for the model
  logical, intent(in), optional :: atStopTime !< if true, checks for final output file
  integer, intent(out)          :: rc         !< return code
  rc = ESMF_SUCCESS
end subroutine outputlog_run
subroutine outputlog_restart(mclock, num_rest_files, rc)
  type(ESMF_Clock)     :: mclock         !< the ESMF_clock for the model
  integer, intent(in)  :: num_rest_files !< the number of restart files
  integer, intent(out) :: rc             !< return code
  rc = ESMF_SUCCESS
end subroutine outputlog_restart
#else
use MOM_coms_infra        , only : root_pe
use MOM_error_handler     , only : is_root_pe, MOM_error, FATAL
use MOM_get_input         , only : get_MOM_input, directories
use MOM_grid              , only : ocean_grid_type
use mpp_domains_mod       , only : mpp_get_io_domain_layout
use NUOPC                 , only : NUOPC_CompAttributeGet
use ESMF                  , only : ESMF_GridComp, ESMF_GridCompGet, ESMF_VM, ESMF_VMGet
use ESMF                  , only : ESMF_Time, ESMF_Clock, ESMF_ClockGet, ESMF_Alarm, ESMF_AlarmSet
use ESMF                  , only : ESMF_ClockGetAlarm, ESMF_AlarmRingerOff, ESMF_AlarmIsRinging
use ESMF                  , only : ESMF_ClockGetNextTime, ESMF_TimeGet, ESMF_TimeInterval
use ESMF                  , only : ESMF_AlarmGet, ESMF_TimeIntervalSet, ESMF_TimeIntervalPrint
use ESMF                  , only : ESMF_SUCCESS, ESMF_LogWrite, ESMF_LOGMSG_INFO, ESMF_FAILURE
use ESMF                  , only : ESMF_LogSetError, ESMF_LogFoundError, ESMF_LOGERR_PASSTHRU
use ESMF                  , only : operator(*), operator(+), operator(-), operator(>), operator(==)
use MOM_cap_methods       , only : ChkErr
use MOM_cap_time          , only : AlarmInit
use shr_is_restart_fh_mod , only : log_restart_fh
use mom_outputlog_methods , only : get_file_state, file_is_complete, get_unlimited_len
use mom_outputlog_methods , only : get_timestr, get_importexport
use mom_outputlog_methods , only : readnml, debug_info
use mom_outputlog_methods , only : outputlog_config_type, outputlog_state_type, outputlog_modeltime_type
use mom_outputlog_methods , only : set_toffset, get_file_state_atring, track_restn, setup_freq_config
use mpi_f08               , only : MPI_Comm, MPI_INTEGER, MPI_SUCCESS
use netcdf

implicit none; private

public :: outputlog_init, outputlog_run, outputlog_restart
public :: track_freqn

integer, parameter :: n_freq  = 4                               !< the number of allowable tracked frequencies
integer, parameter, dimension(n_freq) :: freq = (/1, 3, 6, 24/) !< the allowed tracked frequency hours

type(outputlog_config_type) :: cf(n_freq)    !< a structure containing the configuration, per frequency
type(outputlog_state_type)  :: state(n_freq) !< a structure containing the time dependent file state, per frequency
type(outputlog_modeltime_type) :: modeltime  !< a structure containing the current model time state

type(ESMF_VM)           :: vm                !< the component VM
type(ESMF_Time)         :: lastrestart       !< an ESMF_Time of the last written restart
type(MPI_Comm)          :: mpicomm           !< the model communicator

integer                 :: toffset           !< the timeoffset in hours
integer                 :: nfiles            !< the number of history files when io_layout is used
logical                 :: debug_onroot      !< a logical to enable debug message printing
character(len=256)      :: restartdir        !< the restart directory for MOM6 restarts
character(len=256)      :: outputdir         !< the output directory for MOM6 history output
character(len=256)      :: errmsg            !< an error message
character(len=*), parameter :: u_FILE_u =  __FILE__   !< an ESMF message tracker

contains
!> Initialize the configuration of the outputlog feature
!!
!! `outputlog_init` is called from [MOM_cap_mod::ModelSetRunClock][1].
!!
!! @param[in]      gcomp    an ESMF_GridComp object
!! @param[in]     mclock    an ESMF_Clock object
!! @param[in]  ocean_grid   ocean grid
!! @param[out] rc           return code
!!
!! [1]: \mom6_nuopc_url/mom_cap.F90#L2095
subroutine outputlog_init(gcomp, mclock, ocean_grid, rc)

  type(ESMF_GridComp)  :: gcomp
  type(ESMF_Clock)     :: mclock
  type(ocean_grid_type), pointer, intent(in) :: ocean_grid
  integer, intent(out) :: rc

  ! local variables
  type(ESMF_TimeInterval) :: alarmoffset
  type(directories)       :: dirs
  logical                 :: debug
  integer                 :: n, int_mpic, io_layout(2)
  integer                 :: toffset, hour
  character(len=256)      :: msgString
  character(len=256)      :: subname='MOM_cap:(outputlog_init)'
  !----------------------------------------------------------------------------

  rc = ESMF_SUCCESS
  call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return
  call ESMF_VMGet(vm=vm,mpiCommunicator=int_mpic, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return
  mpicomm%mpi_val = int_mpic

  call get_MOM_input(dirs=dirs)
  restartdir = trim(dirs%restart_output_dir)
  outputdir = trim(dirs%output_directory)

  io_layout = mpp_get_io_domain_layout(ocean_grid%Domain%mpp_domain)
  nfiles = io_layout(1) * io_layout(2)

  call ESMF_ClockGet(mclock, currTime=modeltime%currTime, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return
  call ESMF_TimeIntervalSet(modeltime%tincrement, m=1, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  ! initialize
  lastrestart = modeltime%currTime
  ! opt_n must be set before readnml
  cf(:)%opt_n = freq(:)

  call readnml('input.nml', cf, debug, errmsg, rc=rc)
  rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return
  if (is_root_pe() .and. len_trim(errmsg) > 0) print '(A)',trim(subname)//trim(errmsg)

  debug_onroot = debug .and. is_root_pe()

  call ESMF_TimeGet(modeltime%currTime, h=hour, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  do n = 1,n_freq
    call setup_freq_config(freq(n), nfiles, modeltime, cf(n), state(n), rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! the time offset in hours required to ensure the alarm rings at multiples of freq(n)
    ! regardless of start day/hour
    toffset = set_toffset(hour, freq(n))
    alarmoffset = toffset*60*modeltime%tincrement

    call AlarmInit(mclock,                            &
         alarm     = cf(n)%alarm,                     &
         option    = 'nhours',                        &
         opt_n     = cf(n)%opt_n,                     &
         opt_ymd   = -999,                            &
         RefTime   = modeltime%currTime+alarmoffset,  &
         alarmname = cf(n)%alarm_name, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_AlarmSet(cf(n)%alarm, clock=mclock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    write(msgString,'(A)')trim(subname)//' Output alarm '//trim(cf(n)%alarm_name)//' Created & Set'
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)

    if (is_root_pe()) then
      print '(A,i8)',trim(subname)//' toffset = ',toffset
      call ESMF_TimeIntervalPrint(cf(n)%filename_fhoffset, options="string", rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    endif
  enddo

  if (is_root_pe()) then
    do n = 1,n_freq
      if (cf(n)%requested) print '(A,i6,A)',trim(subname)//' output requested: freq (hours)= ' &
           ,cf(n)%opt_n,', time_reduction= '//cf(n)%timereduce
    enddo
  endif

end subroutine outputlog_init
!> Wrapper for logging output at single frequency
!!
!! `outputlog_run` is called during once during each [MOM_cap_mod::ModelAdvance][1]
!!
!! @param      mclock       an ESMF_Clock object
!! @param      atStopTime   when present, checks for final output file
!! @param      rc           return code
!!
!! [1]: \mom6_nuopc_url/mom_cap.F90#L1746
subroutine outputlog_run(mclock, atStopTime, rc)
  type(ESMF_Clock)              :: mclock
  logical, intent(in), optional :: atStopTime
  integer, intent(out)          :: rc

  ! local variables
  logical             :: lstop
  integer             :: n
  character(len=16)   :: logfile
  character(len=40)   :: importexport
  character(len=16)   :: timestr
  character(len=256)  :: subname='MOM_cap:(outputlog_run)'
  !----------------------------------------------------------------------------

  rc = ESMF_SUCCESS

  lstop = .false.
  if (present(atStopTime)) then
    lstop = atStopTime
  endif

  call ESMF_ClockGet(mclock, startTime=modeltime%startTime, currTime=modeltime%currTime, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return
  call ESMF_ClockGetNextTime(mclock, modeltime%nextTime, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  do n = 1,n_freq
    if (cf(n)%requested) then

      if (lstop) then
        write(logfile,'(A,I2.2,A)') 'mom6.lstop.',cf(n)%opt_n,'h'
      else
        write(logfile,'(A,I2.2,A)') 'mom6.',cf(n)%opt_n,'h'
      endif

      call ESMF_ClockGetAlarm(mclock, alarmname=trim(cf(n)%alarm_name), alarm=cf(n)%alarm, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call ESMF_AlarmGet(cf(n)%alarm, prevRingTime=state(n)%prevring, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      state(n)%ringing = ESMF_AlarmIsRinging(cf(n)%alarm, rc=rc)
      if (state(n)%ringing) call ESMF_AlarmRingerOff(cf(n)%alarm, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call track_freqn(modeltime, cf(n), state(n), mpicomm, is_root_pe(), root_pe(), outputdir, &
           lastrestart, debug_onroot, lstop, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      ! if complete, write a logfile
      if (is_root_pe()) then
        if (state(n)%filecomplete) then
          call log_restart_fh(state(n)%time_logfile, modeltime%startTime, complog=trim(logfile), &
               prefixtime=.true., lastrestart=state(n)%time_lastrestart, lastoutput=state(n)%filename, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
        endif
      endif

    endif
  enddo
end subroutine outputlog_run
!> Track and log file state at single output-frequency alarm
!!
!! @param[in]     mtime             the model time state
!! @param[inout]  cf_n              configuration for this frequency
!! @param[inout]  state_n           tracked state for this frequency
!! @param[in]     comm              MPI communicator
!! @param[in]     isroot            logical flag for root PE
!! @param[in]     rootpe            root rank in communicator
!! @param[in]     outputdir         output directory path
!! @param[in]     lastrestart       last restart write time
!! @param[in]     debug_onroot      logical flag to enable debug printing
!! @param[in]     lstop             logical flag for checking files at finalize
!! @param[out]    rc                return code
subroutine track_freqn(mtime, cf_n, state_n, comm, isroot, rootpe, outputdir, lastrestart, &
     debug_onroot, lstop, rc)

  type(outputlog_modeltime_type), intent(in)    :: mtime
  type(outputlog_config_type),    intent(inout) :: cf_n
  type(outputlog_state_type),     intent(inout) :: state_n
  type(MPI_Comm),                 intent(in)    :: comm
  logical,                        intent(in)    :: isroot
  integer,                        intent(in)    :: rootpe
  character(len=*),               intent(in)    :: outputdir
  type(ESMF_Time),                intent(in)    :: lastrestart
  logical,                        intent(in)    :: debug_onroot
  logical,                        intent(in)    :: lstop
  integer,                        intent(out)   :: rc

  ! local variables
  integer            :: nlen, fsize
  character(len=40)  :: importexport
  character(len=16)  :: timestr

  character(len=256) :: subname='MOM_cap:(track_freqn)'
  !----------------------------------------------------------------------------

  rc = ESMF_SUCCESS

  importexport = get_importexport(mtime%currTime, mtime%nextTime, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  if (.not. lstop) then
    ! when the alarm is ringing, set file check on next advance and construct the filename
    if (state_n%ringing) then
      state_n%chkfile_nextAdvance = .true.

      timestr = get_timestr(mtime%nextTime-cf_n%filename_fhoffset, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      state_n%filename = trim(outputdir)//trim(cf_n%fnameprefix)//trim(timestr)//'.nc'//trim(cf_n%fnamesuffix)

      call get_file_state_atring(state_n, comm, isroot, rootpe, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      if (debug_onroot) then
        print '(A,2(A,L),A,i16)',trim(subname)//' fname '//state_n%filename//'  '//importexport, &
             ' checkflag ',state_n%chkfile_nextAdvance,' use_filesize ',state_n%use_filesize,    &
             '  ',state_n%createsize
      endif
    endif ! state_n%ringing

    if (state_n%chkfile_nextAdvance) then
      state_n%filecomplete = file_is_complete(comm, isroot, rootpe, state_n%filename, state_n%use_filesize, &
           state_n%createsize, rc)
      rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      if (state_n%filecomplete) then
        call get_file_state(comm, isroot, rootpe, state_n%filename, fsize=state_n%completesize, rc=rc)
        rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return

        state_n%chkfile_nextAdvance = .false.
        state_n%time_lastrestart = lastrestart
        state_n%time_logfile = mtime%currTime
      endif
    endif
    if (debug_onroot) call debug_info(trim(subname)//'  ',state_n%filename, state_n%chkfile_nextAdvance, &
         state_n%createsize, importexport)
  else
    ! at lstop, use prevRing in place of currTime to allow for stopping between averaging intervals
    ! prevRing == currTime if stopping on intervals
    if (trim(cf_n%timereduce) == 'none') then
     timestr = get_timestr(state_n%prevring, rc=rc)
     if (ChkErr(rc,__LINE__,u_FILE_u)) return
    else
     timestr = get_timestr(state_n%prevring-30*cf_n%opt_n*mtime%tincrement, rc=rc)
     if (ChkErr(rc,__LINE__,u_FILE_u)) return
    endif
    state_n%filename = trim(outputdir)//trim(cf_n%fnameprefix)//trim(timestr)//'.nc'//trim(cf_n%fnamesuffix)

    call get_file_state(comm, isroot, rootpe, state_n%filename, nlen=nlen, fsize=fsize, rc=rc)
    rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    ! file lands complete; verify size against tracked completesize
    ! assumes at least one regular completion has occurred for this frequency before lstop is ever checked
    state_n%filecomplete = (nlen > 0 .and. fsize == state_n%completesize)

    if (state_n%filecomplete) then
      state_n%chkfile_nextAdvance = .false.
      state_n%time_lastrestart = lastrestart
      state_n%time_logfile = state_n%prevring
    endif
    if (debug_onroot) call debug_info(trim(subname)//' lstop ',state_n%filename, &
         state_n%chkfile_nextAdvance, state_n%createsize, importexport)
  endif ! lstop

end subroutine track_freqn
!> Check all restart files to determine if output has been completed
!!
!! `outputlog_restart` is called whenever restart-writing is triggered in
!! [MOM_cap_mod::ModelAdvance][1]
!!
!! @param[in]    mclock           an ESMF_Clock object
!! @param[in]    num_rest_files   the number of restart files
!! @param[out]   rc               return code
!!
!! [1]: \mom6_nuopc_url/mom_cap.F90#L1746)
subroutine outputlog_restart(mclock, num_rest_files, rc)
  type(ESMF_Clock)     :: mclock
  integer, intent(in)  :: num_rest_files
  integer, intent(out) :: rc

  ! local variables
  type(ESMF_Time)                 :: startTime, currTime, nextTime
  integer                         :: n
  character(len=40)               :: importexport
  logical,            allocatable :: allDone(:)
  character(len=256), allocatable :: fnames(:)
  character(len=256)              :: subname='MOM_cap:(outputlog_restart)'
  !----------------------------------------------------------------------------

  rc = ESMF_SUCCESS

  call ESMF_ClockGet(mclock, startTime=startTime, currTime=currTime, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return
  call ESMF_ClockGetNextTime(mclock, nextTime, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return
  importexport = get_importexport(currTime, nextTime, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  call track_restn(nextTime, num_rest_files, mpicomm, is_root_pe(), root_pe(), restartdir, allDone, fnames, rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  if (debug_onroot) then
    do n = 1,num_rest_files
      if (allDone(n)) then
        print '(A)',trim(subname)//' restart '//trim(fnames(n))//'  '//trim(importexport)//' complete'
      else
        print '(A)',trim(subname)//' restart '//trim(fnames(n))//'  '//trim(importexport)//' still 0'
      endif
    enddo
  endif

  if (all(allDone) .eqv. .true.) then
    lastrestart = nextTime
    if (is_root_pe()) then
      call log_restart_fh(nextTime, startTime, 'mom6.res', prefixtime=.true., rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    endif
  endif
end subroutine outputlog_restart
#endif
end module MOM_cap_outputlog
