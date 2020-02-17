load ../../harness

@test "f79b785ccf20" {
  check 'y   :=    -4 +  -2 ;skip     ;
 
  y    :=  -3  -     4 ' '⇒ skip; skip; y := (-3-4), {y → -6}
⇒ skip; y := (-3-4), {y → -6}
⇒ y := (-3-4), {y → -6}
⇒ skip, {y → -7}'
}
