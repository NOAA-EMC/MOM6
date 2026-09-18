! This file is part of MOM6, the Modular Ocean Model version 6.
! See the LICENSE file for licensing information.
! SPDX-License-Identifier: Apache-2.0
!!
!> @file mom_outputlog_methods.F90
!> @brief Contains methods for the outputlog feature
!!
!> This module contains a set of methods that are required by the UFS outputlog feature
module mom_outputlog_methods

use ESMF,            only : ESMF_Alarm, ESMF_TimeInterval, ESMF_Clock
use ESMF,            only : ESMF_SUCCESS, ESMF_Failure, ESMF_Time, ESMF_TimeGet
use ESMF,            only : operator(*)
use MOM_cap_methods, only : ChkErr
use mpi_f08,         only : MPI_Comm, MPI_INTEGER, MPI_SUCCESS
use netcdf

implicit none; private

!> Structure containing the configuration for output logging at a given frequency
type :: outputlog_config_type
  character(len=14)       :: alarm_name        !< alarm name
  integer                 :: opt_n             !< output frequency (hours)
  logical                 :: requested         !< if true, output logging at this freq is desired
  character(len=7)        :: timereduce        !< snapshot or average time treatment of output, default='average'
  character(len=13)       :: fnameprefix       !< user provided filename prefix, default='ocn'
  character(len=5)        :: fnamesuffix       !< filename suffix if io_layout is in use, default = ''
  type(ESMF_Alarm)        :: alarm             !< ESMF_Alarm associated with this freq
  type(ESMF_TimeInterval) :: filename_fhoffset !< ESMF_TimeInterval offset between tracked file completion and name
end type outputlog_config_type

!> Structure containing the time-dependent state of a tracked file
type :: outputlog_state_type
  logical                       :: chkfile_nextAdvance   !< logical flag to check file completion at next ModelAdvance
  logical                       :: use_filesize          !< logical flag to use the file size to determine completion
  logical                       :: ringing               !< logical flag for alarm ring
  logical                       :: filecomplete          !< logical flag for file completion
  character(len=:), allocatable :: filename              !< tracked file name
  integer                       :: createsize            !< tracked file size at creation
  integer                       :: completesize          !< tracked file size at completion
  type(ESMF_Time)               :: time_lastrestart      !< time of last restart write when a tracked file completes
  type(ESMF_Time)               :: time_logfile          !< time used to create the logfile for tracked file
  type(ESMF_Time)               :: prevRing              !< the prevRing time associated with an Alarm
end type outputlog_state_type

!> Structure containing the model time state during tracking
type :: outputlog_modeltime_type
  type(ESMF_Time)         :: startTime    !< the model startime
  type(ESMF_Time)         :: currTime     !< the model current time at this ModelAdvance
  type(ESMF_Time)         :: nextTime     !< the model time at the end of this ModelAdvance
  type(ESMF_TimeInterval) :: tincrement   !< a convenient time increment of 1 minute
end type outputlog_modeltime_type

character(len=*), parameter :: u_FILE_u =  __FILE__   !< an ESMF message tracker

public :: outputlog_config_type, outputlog_state_type, outputlog_modeltime_type
public :: get_file_state, get_file_state_atring, file_is_complete, get_unlimited_len, track_restn, set_restfname
public :: setup_freq_config, get_timestr, get_importexport
public :: readnml, debug_info, nf90_err
public :: setrequest, settype, setprefix, set_toffset

contains
!> Set up one frequency's non-alarm configuration and state
!!
!! @param[in]     freq_n        this frequency, in hours
!! @param[in]     nfiles        the number of history files when io_layout is used
!! @param[in]     mtime         the model time state
!! @param[inout]  cf_n          this frequency's config
!! @param[out]    state_n       this frequency's initial state
!! @param[out]    rc            return code
subroutine setup_freq_config(freq_n, nfiles, mtime, cf_n, state_n, rc)

  integer,                        intent(in)    :: freq_n
  integer,                        intent(in)    :: nfiles
  type(outputlog_modeltime_type), intent(in)    :: mtime
  type(outputlog_config_type),    intent(inout) :: cf_n
  type(outputlog_state_type),     intent(out)   :: state_n
  integer,                        intent(out)   :: rc

  ! local variables
  character(len=3)   :: chour
  character(len=256) :: subname='MOM_cap:(setup_freq_config)'
  !----------------------------------------------------------------------------

  rc = ESMF_SUCCESS

  write(chour,'(I2.2,A)')freq_n,'h'
  cf_n%alarm_name        = 'output_alarm'//trim(chour)
  if (nfiles == 1) then
    cf_n%fnamesuffix     = ''
  else
    cf_n%fnamesuffix     = '.0000'
  endif
  if (trim(cf_n%timereduce) == 'none') then
    cf_n%filename_fhoffset = 60*freq_n*mtime%tincrement
  else
    cf_n%filename_fhoffset = 90*freq_n*mtime%tincrement
  endif

  state_n%filename            = ' '
  state_n%chkfile_nextAdvance = .false.
  state_n%use_filesize        = .false.
  state_n%filecomplete        = .false.
  state_n%createsize          = 0
  state_n%completesize        = 0
  state_n%time_lastrestart    = mtime%currTime
  state_n%time_logfile        = mtime%currTime

end subroutine setup_freq_config
!> Read nml options to configure output logging
!!
!! @param[in]     fname    input namelist file
!! @param[inout]  cf       outputlog configuration
!! @param[out]    debug    logical flag to enable debug output
!! @param[out]    errmsg   error message
!! @param[out]    rc       return code
subroutine readnml(fname, cf, debug, errmsg, rc)

  character(len=*),            intent(in)    :: fname
  type(outputlog_config_type), intent(inout) :: cf(:)
  logical,                     intent(out)   :: debug
  character(len=*),            intent(out)   :: errmsg
  integer,                     intent(out)   :: rc

  integer :: nfreq, iounit, ierr
  logical :: existflag, outputlog_debug

  integer,           allocatable :: outputlog_fh(:)
  character(len=7),  allocatable :: outputlog_treduce(:)
  character(len=24), allocatable :: outputlog_fnameprefix(:)

  namelist / MOM_outputlog_nml/ outputlog_fh, outputlog_fnameprefix, outputlog_treduce, outputlog_debug

  rc = 0
  errmsg = ''
  nfreq = size(cf)
  allocate(outputlog_fh(1:nfreq))
  allocate(outputlog_treduce(1:nfreq))
  allocate(outputlog_fnameprefix(1:nfreq))
  outputlog_fh(:) = 0
  outputlog_treduce(:) = ''
  outputlog_fnameprefix(:) = ''
  outputlog_debug = .false.

  inquire(file=trim(fname), exist=existflag)
  if (.not. existflag) then
    write (errmsg, '(a)') 'FATAL ERROR: input file '//trim(fname)//' does not exist'
    ierr = 1
    return
  else
    open (action='read', file=trim(fname), iostat=ierr, newunit=iounit)
    read (nml=MOM_outputlog_nml, iostat=ierr, unit=iounit)
    close (iounit)
    if (ierr /= 0) then
      cf(:)%requested = .false.
      write (errmsg, '(a)') ' Namelist ERROR: MOM output logging disabled '
      return
    endif
  endif

  debug = outputlog_debug

  cf%requested = setrequest(cf%opt_n, outputlog_fh, errmsg, ierr)
  if (ierr /= 0) return

  cf%timereduce = settype(cf%opt_n, cf%requested, outputlog_fh, outputlog_treduce, errmsg, ierr)
  if (ierr /= 0) return

  cf%fnameprefix = setprefix(cf%opt_n, cf%requested, outputlog_fh, outputlog_fnameprefix, errmsg, ierr)
  if (ierr /= 0) return

end subroutine readnml
!> Get the state of the netcdf output file at ring time
!!
!! @param[inout]  state_n    this frequency's state -- mutated here
!! @param[in]     comm       MPI communicator
!! @param[in]     isroot     .true. on the root PE
!! @param[in]     rootpe     the root PE's rank
!! @param[out]    rc         return code
subroutine get_file_state_atring(state_n, comm, isroot, rootpe, rc)

  type(outputlog_state_type), intent(inout) :: state_n
  type(MPI_Comm),             intent(in)    :: comm
  logical,                    intent(in)    :: isroot
  integer,                    intent(in)    :: rootpe
  integer,                    intent(out)   :: rc

  integer :: nlen, fsize

  rc = ESMF_SUCCESS

  call get_file_state(comm, isroot, rootpe, state_n%filename, nlen=nlen, fsize=fsize, rc=rc)
  rc = merge(ESMF_SUCCESS, ESMF_Failure, rc == 0)
  if (rc /= ESMF_SUCCESS) return

  state_n%filecomplete = .false.
  state_n%createsize = fsize
  if (nlen == 0) then
     state_n%use_filesize = .false.
  else
     state_n%use_filesize = .true.
  endif

end subroutine get_file_state_atring
!> Check all restart parts' completion state
!!
!! @param[in]   nextTime        the time basis for this restart's filenames
!! @param[in]   num_rest_files  the number of restart parts
!! @param[in]   comm            MPI communicator
!! @param[in]   isroot          .true. on the root PE
!! @param[in]   rootpe          the root PE's rank
!! @param[in]   restartdir      the restart output directory
!! @param[out]  allDone         per-part completion state
!! @param[out]  fnames          per-part filename
!! @param[out]  rc              return code
subroutine track_restn(nextTime, num_rest_files, comm, isroot, rootpe, restartdir, allDone, fnames, rc)

  type(ESMF_Time),                  intent(in)  :: nextTime
  integer,                          intent(in)  :: num_rest_files
  type(MPI_Comm),                   intent(in)  :: comm
  logical,                          intent(in)  :: isroot
  integer,                          intent(in)  :: rootpe
  character(len=*),                 intent(in)  :: restartdir
  logical,             allocatable, intent(out) :: allDone(:)
  character(len=256),  allocatable, intent(out) :: fnames(:)
  integer,                          intent(out) :: rc

  integer :: n

  rc = ESMF_SUCCESS

  allocate(allDone(num_rest_files))
  allocate(fnames(num_rest_files))
  allDone = .false.
  fnames = ''

  do n = 1,num_rest_files
    fnames(n) = set_restfname(nextTime, n, restartdir, rc)
    if (rc /= ESMF_SUCCESS) return
  enddo

  do n = 1,num_rest_files
    allDone(n) = file_is_complete(comm, isroot, rootpe, fnames(n), .false., 0, rc)
    rc = merge(ESMF_SUCCESS, ESMF_Failure, rc == 0)
  enddo

end subroutine track_restn
!> Build the filename for restart part `filenumber`
!!
!! @param[in]   nextTime    the time basis for this restart's filename
!! @param[in]   filenumber  the filenumber (1 for no suffix, ie single file)
!! @param[in]   dir         the restart output directory
!! @param[out]  rc          return code
function set_restfname(nextTime, filenumber, dir, rc) result(fname)

  type(ESMF_Time),  intent(in)  :: nextTime
  integer,          intent(in)  :: filenumber
  character(len=*), intent(in)  :: dir
  integer,          intent(out) :: rc

  character(len=256) :: fname
  character(len=3)   :: suffix
  character(len=15)  :: timestr
  integer :: year, month, day, hour, minute, seconds

  rc = ESMF_SUCCESS

  call ESMF_TimeGet(nextTime, yy=year, mm=month, dd=day, h=hour, m=minute, s=seconds, rc=rc)
  if (rc /= ESMF_SUCCESS) return
  write(timestr,'(I4.4,2(I2.2),A,3(I2.2))') year, month, day,".", hour, minute, seconds

  if (filenumber == 1) then
    suffix = ''
  else if (filenumber-1 < 10) then
    write(suffix,'("_",I1)') filenumber-1
  else
    write(suffix,'("_",I2)') filenumber-1
  endif

  if (len_trim(suffix) == 0) then
    fname = trim(dir)//trim(timestr)//'.MOM.res.nc'
  else
    fname = trim(dir)//trim(timestr)//'.MOM.res'//trim(suffix)//'.nc'
  endif

end function set_restfname
!> Retrieve the unlimited dimension length and file size, broadcasting to all PEs
!!
!! @param[in]   comm      the MPI communicator
!! @param[in]   isroot    logical flag for root PE
!! @param[in]   rootpe    root rank in communicator
!! @param[in]   fname     the file name
!! @param[out]  nlen      optional, the length of the unlimited dimension
!! @param[out]  fsize     optional, the file size in bytes
!! @param[out]  rc        return code
subroutine get_file_state(comm, isroot, rootpe, fname, nlen, fsize, rc)

  type(MPI_Comm),    intent(in)  :: comm
  logical,           intent(in)  :: isroot
  integer,           intent(in)  :: rootpe
  character(len=*),  intent(in)  :: fname
  integer, optional, intent(out) :: nlen
  integer, optional, intent(out) :: fsize
  integer,           intent(out) :: rc

  logical :: existflag
  integer :: ierr, stats(2)

  rc = 0
  stats = nf90_fill_int

  if (isroot) then
    inquire(file=fname, exist=existflag)
    if (existflag) then
      if (present(nlen)) stats(1) = get_unlimited_len(trim(fname))
      if (present(fsize)) inquire(file=fname, size=stats(2))
    endif
  endif

  call MPI_Bcast(stats, 2, MPI_INTEGER, rootpe, comm, ierr)
  if (ierr /= MPI_SUCCESS) then
    rc = ierr
    return
  endif

  if (present(nlen)) nlen  = stats(1)
  if (present(fsize)) fsize = stats(2)

end subroutine get_file_state
!> Determine if the netcdf output file is complete
!!
!! @param[in]   comm          the MPI communicator
!! @param[in]   isroot        logical flag for root PE
!! @param[in]   rootpe        root rank in communicator
!! @param[in]   fname         the file name
!! @param[in]   chk4size      logical flag for check method in use
!! @param[in]   createsize    the filesize at creation
!! @param[out]  rc            return code
!! @return                    logical flag, true if the file is complete
logical function file_is_complete(comm, isroot, rootpe, fname, chk4size, createsize, rc) result(filecomplete)

  type(MPI_Comm),   intent(in)  :: comm
  logical,          intent(in)  :: isroot
  integer,          intent(in)  :: rootpe
  character(len=*), intent(in)  :: fname
  logical,          intent(in)  :: chk4size
  integer,          intent(in)  :: createsize
  integer,          intent(out) :: rc

  logical :: existflag
  integer :: l_nlen, l_fsize, ierr
  !----------------------------------------------------------------------------

  rc = 0
  filecomplete = .false.
  l_nlen = nf90_fill_int
  l_fsize = nf90_fill_int

  if (chk4size) then
    call get_file_state(comm, isroot, rootpe, fname, nlen=l_nlen, fsize=l_fsize, rc=ierr)
    if (ierr == 0) then
      filecomplete = (l_nlen > 0 .and. l_fsize > createsize)
    endif
  else
    call get_file_state(comm, isroot, rootpe, fname, nlen=l_nlen, rc=ierr)
    if (ierr == 0) then
      filecomplete = (l_nlen > 0)
    endif
  endif
  rc = ierr

end function file_is_complete
!> Validate requested output frequencies from namelist entries
!!
!! @param[in]   validfreqs     supported output frequencies (hours)
!! @param[in]   requested_fh   requested frequencies read from namelist
!! @param[out]  errmsg         error message
!! @param[out]  ierr           return code
!! @return                     logical flags indicating requested valid frequencies
function setrequest(validfreqs, requested_fh, errmsg, ierr) result(is_requested)
  integer,          intent(in)  :: validfreqs(:)
  integer,          intent(in)  :: requested_fh(:)
  character(len=*), intent(out) :: errmsg
  integer,          intent(out) :: ierr

  integer :: n, nfreq, reqval
  logical :: is_requested(size(validfreqs))

  nfreq = size(validfreqs)
  ierr = 0
  errmsg = ''
  is_requested = .false.

  if (all(requested_fh == 0)) return

  do n = 1,nfreq
    reqval = requested_fh(n)
    if (reqval /= 0) then
      if (.not. any(validfreqs == reqval)) then
        ierr = 1
        write(errmsg, '(A, I0)') "MOM_outputlog: Unsupported output frequency requested: ", reqval
        return
      endif
    endif
  enddo

  do n = 1, size(requested_fh)
    reqval = requested_fh(n)
    if (reqval /= 0) then
      if (count(requested_fh == reqval) > 1) then
        ierr = 1
        write(errmsg, '(A, I0)') "MOM_outputlog: Duplicate output frequency requested: ", reqval
        return
      endif
    endif
  enddo

  do n = 1, nfreq
    if (any(requested_fh == validfreqs(n))) then
      is_requested(n) = .true.
    endif
  enddo
end function setrequest
!> Helper function to locate index of namelist provided frequency in array of valid frequencies
!!
!! param[in]      nml_fh        namelist freq list
!! param[in]      target_freq   desired frequency value
!! @return        m             index association
function find_nml_slot(nml_fh, target_freq) result(m)
  integer, intent(in) :: nml_fh(:)
  integer, intent(in) :: target_freq

  integer :: m

  do m = 1, size(nml_fh)
    if (nml_fh(m) == target_freq) return
  enddo
  m = 0
end function find_nml_slot
!> Determine output reduction type for each requested frequency
!!
!! @param[in]   validfreqs   supported output frequencies (hours)
!! @param[in]   requested    logical flags for active output frequencies
!! @param[in]   nml_fh       requested frequencies read from namelist
!! @param[in]   nml_type     requested output reduction types from namelist
!! @param[out]  errmsg       error message
!! @param[out]  ierr         return code
!! @return                   output reduction type by supported frequency slot
function settype(validfreqs, requested, nml_fh, nml_type, errmsg, ierr) result(filetypes)

  integer,          intent(in)  :: validfreqs(:)
  logical,          intent(in)  :: requested(:)
  integer,          intent(in)  :: nml_fh(:)
  character(len=*), intent(in)  :: nml_type(:)
  character(len=*), intent(out) :: errmsg
  integer,          intent(out) :: ierr

  integer :: n, m, nfreq
  character(len=7) :: reqval
  character(len=7) :: filetypes(size(validfreqs))

  nfreq = size(validfreqs)
  ierr = 0
  errmsg = ''
  filetypes = ''

  if (.not. any(requested)) return

  do m = 1, nfreq
    reqval = trim(adjustl(nml_type(m)))
    if (reqval /= '') then
      if (reqval /= 'average' .and. reqval /= 'none') then
        ierr = 1
        errmsg = "MOM_outputlog: Invalid nml_type '"// trim(reqval)// "'. Must be exactly 'average' or 'none'"
        return
      endif
    endif
  enddo

  do n = 1, nfreq
    if (nml_fh(n) == 0 .and. len_trim(nml_type(n)) > 0) then
      ierr = 1
      errmsg = "MOM_outputlog: File type keyword provided for an inactive frequency slot."
      return
    endif
  enddo

  do n = 1, nfreq
    if (.not. requested(n)) cycle

    m = find_nml_slot(nml_fh, validfreqs(n))
    if (m == 0) then
      ierr = 1
      write(errmsg, '(A, I0)') "MOM_outputlog: internal error -- no matching nml_fh slot for validfreqs ", &
           validfreqs(n)
      return
    endif

    reqval = trim(adjustl(nml_type(m)))
    if (reqval == 'average' .or. reqval == 'none') then
      filetypes(n) = reqval
    else
      filetypes(n) = 'average'
    endif
  enddo
end function settype
!> Determine filename prefixes for each requested frequency
!!
!! @param[in]   validfreqs       supported output frequencies (hours)
!! @param[in]   requested        logical flags for active output frequencies
!! @param[in]   nml_fh           requested frequencies read from namelist
!! @param[in]   nml_fnameprefix  requested filename prefixes from namelist
!! @param[out]  errmsg           error message
!! @param[out]  ierr             return code
!! @return                       filename prefixes by supported frequency slot
function setprefix(validfreqs, requested, nml_fh, nml_fnameprefix, errmsg, ierr) result(fileprefixes)

  integer,          intent(in)  :: validfreqs(:)
  logical,          intent(in)  :: requested(:)
  integer,          intent(in)  :: nml_fh(:)
  character(len=*), intent(in)  :: nml_fnameprefix(:)
  character(len=*), intent(out) :: errmsg
  integer,          intent(out) :: ierr

  integer :: n, m, nfreq, n_active
  character(len=13) :: reqval       ! 12 user chars max + appended '_'
  character(len=13) :: fileprefixes(size(validfreqs))

  nfreq = size(validfreqs)
  ierr = 0
  errmsg = ''
  fileprefixes = ''

  n_active = count(requested)
  if (n_active == 0) return

  do n = 1, nfreq
    if (nml_fh(n) == 0 .and. len_trim(nml_fnameprefix(n)) > 0) then
      ierr = 1
      write(errmsg, '(A, I2)') 'MOM_outputlog: filename prefix provided for inactive slot ', n
      return
    endif
  enddo
  do n = 1, nfreq
    if (nml_fh(n) /= 0 .and. len_trim(nml_fnameprefix(n)) > 12) then
      ierr = 1
      write(errmsg, '(A, I2)') 'MOM_outputlog: filename prefix too long for active slot ', n
      return
    endif
  enddo
  do n = 1, nfreq
    if (nml_fh(n) /= 0 .and. len_trim(nml_fnameprefix(n)) > 0) then
      if (verify(trim(nml_fnameprefix(n)), &
           'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_') > 0) then
        ierr = 1
        write(errmsg, '(A, I2, A)') 'MOM_outputlog: filename prefix for active slot ', n, &
             ' contains invalid characters (letters, digits, underscore only)'
        return
      endif
    endif
  enddo

  ! default file prefix == 'ocn' for any single freq run
  if (n_active == 1) then
    n = findloc(requested, .true., dim=1)   ! exactly one true value

    m = find_nml_slot(nml_fh, validfreqs(n))
    if (m == 0) then
      ierr = 1
      write(errmsg, '(A, I0)') "MOM_outputlog: internal error -- no matching nml_fh slot for validfreqs ", &
           validfreqs(n)
      return
    endif

    reqval = trim(adjustl(nml_fnameprefix(m)))
    if (reqval == '') then
      fileprefixes(n) = 'ocn_'
    else
      fileprefixes(n) = trim(reqval)//'_'
    endif
    return
  endif

  ! multi-freq output; must provide fileprefixes
  if (n_active > 1) then
    do n = 1, nfreq
      if (.not. requested(n)) cycle

      m = find_nml_slot(nml_fh, validfreqs(n))
      if (m == 0) then
        ierr = 1
        write(errmsg, '(A, I0)') "MOM_outputlog: internal error -- no matching nml_fh slot for validfreqs ", &
             validfreqs(n)
        return
      endif

      reqval = trim(adjustl(nml_fnameprefix(m)))
      if (reqval == '') then
        ierr = 1
        write(errmsg, '(A, I0, A)') "MOM_outputlog: Multiple frequencies requested," // &
             " but nml_fnameprefix is missing for frequency ", validfreqs(n), "h."
        return
      endif
      fileprefixes(n) = trim(reqval)//'_'
    enddo

    ! multi-freq output: must provide unique fileprefixes
    do n = 1, nfreq
      if (.not. requested(n)) cycle

      do m = n + 1, nfreq
        if (requested(m) .and. fileprefixes(n) == fileprefixes(m)) then
          ierr = 1
          errmsg = "MOM_outputlog: Ambiguous nml_fnameprefix '" // trim(fileprefixes(n)) // &
               "'. Multiple active output streams cannot share the same filename root."
          return
        endif
      enddo
    enddo
  endif ! n_active > 1

end function setprefix
!> Compute the alarm-alignment toffset
!!
!! The time offset in hours (toffset) required to ensure that an output-frequency alarm
!! rings on that frequency's own nominal grid, regardless of the actual (eg restart or
!! IAU-shifted) start hour.
!!
!! @param[in]  hour   the model's actual start hour (0-23)
!! @param[in]  freq   the output frequency, in hours
!! @return            the alignment offset, in hours
function set_toffset(hour, freq) result(toffset)
  integer, intent(in) :: hour
  integer, intent(in) :: freq

  integer             :: toffset

  if (freq == 1 .or. freq == 24) then
    toffset = 0
  else if (mod(hour, freq) /= 0) then
    toffset = freq - mod(hour, freq)
  else
    toffset = 0
  endif
end function set_toffset
!> Return the length of the unlimited dimension
!!
!! @param[in]  fname   the file name
!! @return             unlimited dimension length
integer function get_unlimited_len(fname) result(unlen)
  character(len=*), intent(in) :: fname

  integer :: ncid, dimid
  !----------------------------------------------------------------------------

  unlen = nf90_fill_int
  call nf90_err(nf90_open(trim(fname), nf90_nowrite, ncid), 'nf90_open: '//trim(fname))
  call nf90_err(nf90_inquire(ncid, unlimiteddimid=dimid), 'inquire unlimiteddimid')
  call nf90_err(nf90_inquire_dimension(ncid, dimid, len=unlen), 'inquire unlimited dimension')
  call nf90_err(nf90_close(ncid), 'close: '//trim(fname))
end function get_unlimited_len
!> Convenience function to return a 16-character time string
!!
!! @param[in]  MyTime   an ESMF_Time object
!! @param[out] rc       return code
!! @return              16-character formatted time string (YYYY_MM_DD_HH_MM)
function get_timestr(MyTime, rc) result(timestr)
  type(ESMF_Time), intent(in)  :: MyTime
  integer,         intent(out) :: rc

  character(len=16) :: timestr
  integer :: year, month, day, hour, minute
  !----------------------------------------------------------------------------

  rc = ESMF_SUCCESS

  call ESMF_TimeGet(MyTime, yy=year, mm=month, dd=day, h=hour, m=minute, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return
  write(timestr,'(I4.4,4(A,I2.2))')year,'_',month,'_',day,'_',hour,'_',minute
end function get_timestr
!> Convenience function to return import/export timestring
!!
!! @param[in]  currTime   an ESMF_Time object
!! @param[in]  nextTime   an ESMF_Time object
!! @param[out] rc         return code
!! @return                40-character string
function get_importexport(currTime, nextTime, rc) result(importexport)

  type(ESMF_Time), intent(in)  :: currTime, nextTime
  integer,         intent(out) :: rc

  character(len=19) :: import_timestr, export_timestr
  character(len=40) :: importexport
  !----------------------------------------------------------------------------

  rc = ESMF_SUCCESS

  call ESMF_TimeGet(currTime, timestring=import_timestr, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return
  call ESMF_TimeGet(nextTime, timestring=export_timestr, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return
  importexport = trim(import_timestr)//'  '//trim(export_timestr)
end function get_importexport
!> Write debug info to stdout, only called on root pe
!!
!! @param[in]    tag            an information tag
!! @param[in]    fname          the filename to check
!! @param[in]    filesize       the filesize at creation time
!! @param[in]    chkflag        logical flag for checking next Advance
!! @param[in]    timestring     a timestring
subroutine debug_info(tag,fname,chkflag,filesize,timestring)
  character(len=*), intent(in) :: tag
  character(len=*), intent(in) :: fname
  integer,          intent(in) :: filesize
  logical,          intent(in) :: chkflag
  character(len=*), intent(in) :: timestring

  logical :: existflag
  integer :: fsize
  integer :: unlen
  character(len=256) :: msgString
  !----------------------------------------------------------------------------

  inquire(file=fname, exist=existflag)
  if (existflag) then
    inquire(file=fname, size=fsize)
    unlen = get_unlimited_len(trim(fname))

    write(msgString,'(A)')tag//'  '//fname//' exists '//timestring
    if (chkflag) then
      print '(A,L,2i16,i5)',trim(msgString)//' not complete, chkflag ',chkflag,filesize,fsize,unlen
    else
      print '(A,L,2i16,i5)',trim(msgString)//'     complete, chkflag ',chkflag,filesize,fsize,unlen
    endif
  else
    write(msgString,'(A)')tag//'  '//'no output file exists '//timestring
    print '(A)',trim(msgString)
  endif
end subroutine debug_info
!> Handle netcdf errors
!!
!! @param[in]  ierr        the error code
!! @param[in]  string      the error message
subroutine nf90_err(ierr, string)
  integer,          intent(in) :: ierr
  character(len=*), intent(in) :: string
  !----------------------------------------------------------------------------

  if (ierr /= nf90_noerr) then
    write(0, '(A)') 'FATAL ERROR: ' // trim(string)// ' : ' // trim(nf90_strerror(ierr))
    ! This fails on WCOSS2 with Intel 19 compiler. See https://community.intel.com/
    ! Search term "STOP and ERROR STOP with variable stop codes"
    ! When WCOSS2 moves to Intel 2020+, uncomment the next line and remove stop 99
    !stop ierr
    stop 99
  endif
end subroutine nf90_err
end module mom_outputlog_methods
