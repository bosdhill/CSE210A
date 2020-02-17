load ../../harness

@test "597318084d38" {
  check 'if (false∧     z     +     x   =    -3  -   -2)     then 
skip   else  skip' '⇒ skip, {}'
}
