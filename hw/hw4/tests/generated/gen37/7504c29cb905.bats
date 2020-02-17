load ../../harness

@test "7504c29cb905" {
  check 'if (-3  +    EZ  <    0*  z    ∨   z*     -1   <x   -   -4)    then z   := z    + 0      else  
l9     := z-     3     ' '⇒ z := (z+0), {}
⇒ skip, {z → 0}'
}
