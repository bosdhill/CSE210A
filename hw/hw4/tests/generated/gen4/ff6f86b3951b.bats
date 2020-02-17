load ../../harness

@test "ff6f86b3951b" {
  check 'if (3     +   x    =     y    +  1) then      skip      else  Ax := 0   *M    ' '⇒ Ax := (0*M), {}
⇒ skip, {Ax → 0}'
}
