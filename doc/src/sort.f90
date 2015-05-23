module sort
  !! Contains functions for sorting vector data sets

  implicit none
  
contains

  pure subroutine insertion_sort(S)
    !! An unconditionally stable, adaptive sorting algorithm with low overhead and /(O\left(n^2\)/) worst-case time. This algorithm is ideal when the data set is nearly sorted or when the data set's range is small.

    real, intent(in out) :: S(:)
    real :: tmp
    integer :: l, u, i, j

    l = lbound(S,1); u = ubound(S,1)

    do i = l+1, u
       j = i - 1
       tmp = S(i)
       do while (j >= l .and. S(j) > tmp)
          S(j+1) = S(j)
          j = j - 1
       end do
       S(j+1) = tmp
    end do
  end subroutine insertion_sort

  pure subroutine selection_sort(S)
    !! Conditionally stable, /(\Theta\left(n^2\right)/) comparisons but only /(\Theta\left(n\right)) swaps. Generally considered to have poor performance, but may be the algorithm of choice if the cost of swapping is expensive.

    real, allocatable, intent(in out) :: S(:)
    real :: tmp
    integer :: l, u, i, j, k

    l = lbound(S,1); u = ubound(S,1)

    do i = l, u
       k = i
       tmp = S(i)
       do j = l+1, u
          if (S(j) < S(k)) k = j
       end do
       S(i) = S(k)
       S(k) = tmp
    end do
           
  end subroutine selection_sort
  
end module sort

    
