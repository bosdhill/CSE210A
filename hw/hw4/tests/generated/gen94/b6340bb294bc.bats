load ../../harness

@test "b6340bb294bc" {
  check 'if (false ∧  UK -   x    < 98698863101864523186886400967575590879 -  x) then 
skip     else skip' '⇒ skip, {}'
}
