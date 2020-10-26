module shellfish
  ! moduleは増やさない．submoduleを導入する．パラメータはnamelistから読み込むのがよいか？
  ! scope for growth や mortality の関数等を開発する．
  ! 関数等は複数用意して，切り替えられるようにする．インターフェースを用意して，引数で切り替え．
  ! 従属変数は標準的な（短い）名前を踏襲する．一方，係数等はわかりやすい（長めの）名前を付ける．
  ! 国際的には殻長ではなく重量を従属変数としているよう．どちらがよいか？
  ! CFL=1以外では数値拡散が顕著にみられる．数値拡散を実際の分散のアナロジーと見るか？
  ! 数値拡散をコントロールするか，TVDを適用するか？
  ! CFL=1の条件をできるだけ満たすのがよいか？
  ! test programの有効無効はプリプロセッサで制御する
  !
  implicit none
  integer, parameter :: rp = selected_real_kind(p=12)  ! 12: double precision, 6: single precision
  ! ファイル読み込みに変更の余地
  integer, parameter :: imax = 1
  integer, parameter :: jmax = 1
  integer, parameter :: nmax = 10
  integer, parameter :: timestep_end = 10
  integer, parameter :: output_timestep_interval = 1

  ! Definition of parameters
  ! namelist で読み込む形式に将来変更
  real(rp) :: dt = 1.0_rp ! [d]
  real(rp) :: ds = 0.1_rp ! [g]
  real(rp) :: eta_max = 0.1_rp  ! max scope of growth [g/d]
  !real(rp) :: mu0 = 0.0_rp  ! mortality

  ! Inserting a file
  include 'shellfish_params.h'


  private
  public :: shellfish_main

contains

  subroutine shellfish_main()
    ! Temporary main routine for shellfish calculation.
    ! In practical application, cal_shellfish_run() should be called in the time integration loop.
    integer :: timestep

    do timestep = 1, timestep_end
      call cal_shellfish_run(timestep)
    end do
  
  end subroutine shellfish_main

  subroutine cal_shellfish_run(timestep)
    ! main subroutine called in main routine
    integer, intent(in) :: timestep
    real(rp), dimension(imax, jmax, 0:nmax) :: Ns  ! Ns(s, t) or Ns(s, t+dt) : population (m^-2)
    logical :: L_ini = .true.
    logical :: L_out = .false.

    ! Call initial setting only once at the beginning
    if (L_ini) then
      call shellfish_ini(Ns)
      L_ini = .false.
    end if

    call shellfish_run(Ns)
        
    ! Output results
    if (mod(timestep, output_timestep_interval) == 0) then
      L_out = .true.
    end if

    if (L_out) then
      call shellfish_out(Ns, timestep)
      L_out = .false.
    end if

  end subroutine cal_shellfish_run


  subroutine shellfish_ini(Ns)
    ! Set initial condition for Ns using private function ini_ns()
    real(rp), intent(out), dimension(imax, jmax, 0:nmax) :: Ns     ! Ns(s, t) : pupulation (m^-2)

    Ns = ini_Ns()

  end subroutine shellfish_ini


  subroutine shellfish_run(Ns)
    real(rp), intent(inout), dimension(imax, jmax, 0:nmax) :: Ns  ! Ns(s, t) or Ns(s, t+dt)
    real(rp), dimension(imax, jmax, 0:nmax) :: eta  ! scope for growth (g/day)
    real(rp), dimension(imax, jmax, nmax) :: mu   ! mortality (1/day)
    real(rp) :: CFL  ! CFL number
    
    ! gets eta
    eta = update_eta()

    ! gets mu
    mu = update_mu()

    ! check CFL condition
    CFL = maxval(eta(:,:,1:)) * dt / ds
    write(0,*) 'CFL= ', CFL  ! Unit number = 0 means standard error output available in ifort and gfortran
    if (CFL > 1.0_rp) then
      write(0,*) 'dt= ', dt, 'ERROR: CFL= ', CFL, ' > 1'
      stop
    end if

    call update_Ns(Ns, eta, mu)

  end subroutine shellfish_run


  subroutine shellfish_out(Ns, timestep)
    ! Output results
    real(rp), intent(in), dimension(imax, jmax, 0:nmax) :: Ns  ! Ns(s, t) or Ns(s, t+dt)
    integer, intent(in) :: timestep
    integer :: i, j, n

    do n = 1, nmax
      do j = 1, jmax
        do i = 1, imax
          write(6, '(4i5, f12.3)') timestep, i, j, n, Ns(i, j, n)  ! Unit number=6 is stadard output.
        end do
      end do
    end do

  end subroutine shellfish_out


  function ini_Ns()
    ! Set initial value for Ns
    real(rp) :: ini_Ns(imax, jmax, 0:nmax)

    ini_Ns = 0.0_rp  ! initialize with 0
    ini_Ns(:, :, 1) = 1.0_rp

  end function ini_Ns


  function update_eta() result(eta)
    ! Define scope for growth
    real(rp), dimension(imax, jmax, 0:nmax) :: eta  ! scope for growth (g/day)

    eta = eta_max  ! should be modified

    ! Boundary conditions
    eta(:, :, 0) = 0.0_rp
    eta(:, :, nmax) = 0.0_rp

  end function update_eta


  function update_mu() result(mu)
    ! Define mortality mu
    real(rp), dimension(imax, jmax, nmax) :: mu  ! mortality (1/day)

    mu = mu0

  end function update_mu


  subroutine update_Ns(Ns, eta, mu)
    ! Population dynamics computation
    real(rp), dimension(imax,jmax,0:nmax), intent(inout) :: Ns  ! Ns(s, t+dt)  : population (m^-2)
    real(rp), dimension(imax,jmax,0:nmax), intent(in) :: eta  ! eta(s,t)     : scope for growth (g/day)
    real(rp), dimension(imax,jmax,nmax), intent(in) :: mu     ! mu(s, t)     : mortality (1/day)

    ! Upwind scheme with explicit integration for source term
    Ns(:,:,1:nmax) = Ns(:, :, 1:nmax) + dt/ds  &
      &              * (eta(:, :, 0:nmax-1) * Ns(:, :, 0:nmax-1) - eta(: ,: ,1:nmax) * Ns(:, :, 1:nmax) )  &
      &              - mu(:, :, :) * Ns(:, :, 1:nmax) * dt

    ! Upwind scheme with implicit integration for source term (to be implemented)
    
    ! Upwind scheme with Crank-Nicolson integration for source term (to be implemented)

    ! Improvement on upwind scheme should be considered.

    ! Unit number=0 is standard error output for checking.
    write(0,*) 'eta00= ', eta(:, :, 0:nmax-1)
    write(0,*) 'eta10= ', eta(:, :, 1:nmax)
    write(0,*) 'Ns00= ', Ns(:, :, 0:nmax-1)
    write(0,*) 'Ns10= ', Ns(:, :, 1:nmax)

  end subroutine update_Ns


end module shellfish


program shellfish_test
  ! Temporary main program for testing
  ! cal_shellfish_run(timestep) should be called in the time integration loop in a practical application.
  use shellfish, only : shellfish_main
  implicit none

  call shellfish_main()

end program shellfish_test
