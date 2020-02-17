load ../../harness

@test "2811ffffb825" {
  check 'while (¬true)      do 
 
 x :=    y    +     0   ' '⇒ skip, {}'
}
