module search
  use statistics

contains

  pure recursive integer function binary_search(A, key) result(imid)
    !! Returns the index of the key if it is found in the array A. If the key is not found, -1 is returned.
    integer, intent(in) :: A(:)
    integer, intent(in) :: key

    integer :: imin, imax

    imin = lbound(A,1)
    imax = ubound(A,1)
    imid = -1

    if (imax < imin) then
       ! No match found
       return
    else
       imid = ceiling(median(A))

       if (A(imid) > key) then
          ! Search left
          imid = binary_search(A(imin:imid-1), key)
       else if (A(imid) < key) then
          ! Search right
          imid = binary_search(A(imid+1:imax), key)
       else
          ! Match found
          return
       end if
    end if
        
  end function binary_search
  
  
end module search
