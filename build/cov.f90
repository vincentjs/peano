module statistics

contains

pure real function mean(x) result(xbar)
  !! Returns the arithmatic mean of a vector.
  !! $$ \mu = \frac{1,n} \Sum_{i=1}^n x_i

  implicit none

  real, allocatable,  intent(in) :: x(:)
  
  real :: Sx
  integer :: n, i

  xbar = 0.0
  Sx = 0.0
  
  if (allocated(x)) then
    n = size(x)
    do i = 1, n
      Sx = Sx + x(i)
    end do

    xbar = Sx / n
  end if
end function mean
  
pure real function cov(x, y) result (sigma)
  !! Returns the covariance of two vectors \(x\) and \(y\),
  !! $$ cov(x,y) =\frac{1,n-1} \Sum_{i=1}^n \left(x_i - \mu_x \right) \left(B_i-\mu_y\right) $$

  implicit none
  
  real, allocatable, intent(in) :: x(:), y(:)
  
  real :: Sx, Sy, xb, yb
  integer :: n, i

  !!TODO: If x and y aren't same size, return error
  !!TODO: Handle x and y not same bound start/end
  sigma = 0.0
  
  if (allocated(x).and. allocated(y)) then
    n = size(x)

    xb = mean(x)
    yb = mean(y)

    do i = 1, n
      Sx = x(i) - xb
      Sy = y(i) - yb
      sigma = sigma + (Sx*Sy / n)
    end do
  end if
  
end function cov

end module statistics
