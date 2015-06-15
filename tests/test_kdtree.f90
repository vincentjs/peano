subroutine test_kdtree()
  !! Tests the kd_tree module found in kdtree.f90.

  use kd_tree

  type(kdtree), pointer :: tree
  real, dimension(2,6) :: S

  !! Test matrix: [(2,3),(5,4),(9,6),(4,7),(8,1),(7,2)]
  S = reshape([2, 5, 9, 4, 8, 7, 3, 4, 6, 7, 1, 2], shape(S))

  tree => build_kd_tree(S)
  
end subroutine test_kdtree
