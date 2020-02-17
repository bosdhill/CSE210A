load ../../harness

@test "02e1e43b45db" {
  check 'if (-2 + y <   z   +     -1     ∧     y  =3     --2) then    skip    else  
 skip' '⇒ skip, {}'
}
