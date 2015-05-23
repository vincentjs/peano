subroutine test_cov()
  !! Tests the covariance subroutine, cov.f90.
  use statistics

  real, allocatable :: a(:)
  real, allocatable :: b(:)
  real, allocatable :: c(:)

  allocate(a(6))
  allocate(b(6))
  allocate(c(6))
  a = (/0, 1, 2, 3, 4, 5/)
  b = (/0, 10, 20, 30, 40, 50/)
  c = cov(a,b)
  
end subroutine test_cov
