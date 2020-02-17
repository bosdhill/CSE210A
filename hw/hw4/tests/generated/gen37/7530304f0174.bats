load ../../harness

@test "7530304f0174" {
  check 'while (¬true)    do 
    z    :=    RK -     3     ' '⇒ skip, {}'
}
