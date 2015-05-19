module statistics

  implicit none
  
  public :: mean, cov
  
contains

pure real function factorial(x) result(xfac)
  !! Returns the factorial of the scalar value \(x\),
  !! $$ x! = n(n-1)(n-2)\cccdot(3)(2)(1) $$

  real, intent(in) :: x

  xfac = 1
  if (x >= 0) then
     do i = x, 1, -1
        xfac = xfac * i
     end do
  end if

end function factorial
  
pure real function mean(x) result(xbar)
  !! Returns the arithmatic mean of a vector.
  !! $$ \mu = \frac{1,n} \Sum_{i=1}^n x_i $$

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

pure real function median(x) result (xm)
  !! Returns the median of a vector. If the vector has an odd number
  !! of elements, the "mean of middle" two is returned.

  implicit none

  real, allocatable, intent(in) :: x(:)

  integer :: n, nd2
  real :: xmm1, xmp1

  n = size(x)
  nd2 = n/2

  if (mod(n,2) .eq. 0) then
     xm = x(nd2)
  else
     xmm1 = x(nd2-1)
     xmp1 = x(nd2+1)
     xm = (xmm1 + xmp1) / 2.0
  end if
  
end function median

pure real function variance(x) result(ss)
  !! Returns the sample variance of the vector \(x\),
  !! $$ s^2 = \frac{1}{n-1} \Sum_{i=1}^n \left(x_i - \mu_x \right)^2 $$

  implicit none

  real, allocatable, intent(in) :: x(:)

  integer :: n
  real :: xb

  n = size(x)

  xb = mean(x)
  
  if (allocated(x)) then
     l = lbound(x,1)
     u = ubound(x,1)
     do i = l, u
        ss = ss + (x(i) - xb)**2
     end do

     ss = 1 / (n - 1) * ss
  end if
  
end function variance

pure real function standardDeviation(x) result(s)
  !! Returns the sample standard deviation of a vector \(x\),
  !! $$ s = \sqrt{\frac{1}{n-1} \Sum_{i=1}^n \left(x_i - \mu_x \right)^2} $$

  implicit none

  real, allocatable, intent(in) :: x(:)

  s = sqrt(variance(x))

end function standardDeviation

pure real function covariance(x, y) result(sigma)
  !! Returns the covariance of two vectors \(x\) and \(y\),
  !! $$ cov(x,y) =\frac{1}{n-1} \Sum_{i=1}^n \left(x_i - \mu_x \right) \left(B_i-\mu_y\right) $$

  implicit none
  
  real, allocatable, intent(in) :: x(:), y(:)
  
  real :: Sx, Sy, xb, yb
  integer :: n, i, l, u

  !!TODO: If x and y aren't same size, return error
  !!TODO: Handle x and y not same bound start/end
  sigma = 0.0
  
  if (allocated(x).and. allocated(y)) then
    n = size(x)

    xb = mean(x)
    yb = mean(y)

    l = lbound(x,1)
    u = ubound(x,1)
    do i = l, u
      Sx = x(i) - xb
      Sy = y(i) - yb
      sigma = sigma + (Sx*Sy / n)
    end do
  end if
  
end function covariance

end module statistics
