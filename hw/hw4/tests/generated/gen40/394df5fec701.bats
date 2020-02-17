load ../../harness

@test "394df5fec701" {
  check 'z :=4 +z;



if (z * -2 =   3    *    -1)      then    skip else  
 
skip  ' '⇒ skip; if ((z*-2)=(3*-1)) then { skip } else { skip }, {z → 4}
⇒ if ((z*-2)=(3*-1)) then { skip } else { skip }, {z → 4}
⇒ skip, {z → 4}'
}
