-- Engineer: Ethan Kho
--
PACKAGE step6_package IS
      TYPE operations IS (op_A,op_B,op_notA,op_notB,op_AxorB,op_AorB,op_AandB,
                          op_AnandB,op_AxnorB,op_0,op_1,op_incA,op_incB,op_decA,
                          op_decB,op_negA,op_negB,op_AplusB,op_AplusBwC,
                          op_AminB,op_AminBwC,op_BminA);

      PROCEDURE binadd (l,r : IN BIT_VECTOR;
                                 cin : IN BIT;
                                 sum : OUT BIT_VECTOR ;
                                 cout : OUT BIT);       

   
      PROCEDURE binsub (x,y : IN BIT_VECTOR;
                                     bin : IN BIT;
                                     diff : OUT BIT_VECTOR ;
                                     bout : OUT BIT);
END step6_package;


PACKAGE BODY step6_package IS
    
    -- declaration of binadd , neg procedures
    PROCEDURE binadd (l,r : IN BIT_VECTOR;
                                     cin : IN BIT;
                                     sum : OUT BIT_VECTOR ;
                                     cout : OUT BIT) IS
    VARIABLE carry : BIT; --internal variable carry
    
    BEGIN
         carry := cin;
    FOR i IN l'reverse_range LOOP
          -- compute sum for position I
          sum(i) := l(i) XOR r(i) XOR carry; 
          -- compute carry out for position I into carry variable
          carry := (l(i) AND r(i)) OR (l(i) AND carry) OR (r(i) and carry);
    END LOOP;
          -- assign cout from final value of carry
          cout := carry;
    END PROCEDURE binadd;    
    


    -- declaration of binsub 
    PROCEDURE binsub (x,y : IN BIT_VECTOR;
                                     bin : IN BIT;
                                     diff : OUT BIT_VECTOR ;
                                     bout : OUT BIT) IS
    VARIABLE borr : BIT; --internal variable carry
    
    BEGIN
         borr := bin;
    FOR i IN x'reverse_range LOOP
          -- compute diff for position I
          diff(i) := (NOT x(i) AND NOT y(i) AND borr) OR
                     (NOT x(i) AND y(i) AND NOT borr) OR
                     (x(i) AND  y(i) AND borr) OR
                     (x(i) AND NOT y(i) AND NOT borr) ; 
          -- compute borrow out for position I into borr variable
          borr := (NOT x(i) AND borr) OR (y(i) AND borr) OR (NOT x(i) and y(i));
    END LOOP;
          -- assign cout from final value of carry
          bout := borr;
    END PROCEDURE binsub;
END step6_package;


USE work.step6_package.all;
         
ENTITY   alt_8bit_alu  IS
	    PORT (	alu_op  :  IN  operations;
			A, B         :  IN  BIT_VECTOR ( 7 downto  0);
			Cin	     :  IN  BIT;
			Zout	     :  OUT  BIT_VECTOR ( 7 downto  0);
			Cout         :  OUT  BIT  );
END  alt_8bit_alu;


ARCHITECTURE  behavioral  OF  alt_8bit_alu IS
    
    BEGIN  
    
      
    alu_8bit : PROCESS (A, B, alu_op, Cin)       
    
   
    VARIABLE        CoutTemp: BIT;
    VARIABLE        Ztemp   : BIT_VECTOR (7 DOWNTO 0); 
        BEGIN    
            
            
            
            CASE  alu_op  IS
                
                  WHEN op_A =>  Zout<= A;  Cout <= '0'; --op_A
                  WHEN op_B =>  Zout<= B;  Cout <= '0'; --op_B 
                  WHEN op_notA =>  Zout<= NOT A;  Cout <= '0'; --op_notA;
                  WHEN op_notB =>  Zout<= NOT B;  Cout <= '0'; --op_notB;
                  WHEN op_AxorB =>  Zout<= A XOR B;  Cout <= '0'; --op_AxorB;
                  WHEN op_AorB =>  Zout<= A OR B;  Cout <= '0'; --op_AorB;    
                  WHEN op_AandB =>  Zout<= A AND B;  Cout <= '0'; --op_AandB;
                  WHEN op_AnandB =>  Zout<= A NAND B;  Cout <= '0'; --op_AnandB;
                  WHEN op_AxnorB =>  Zout<= A XNOR B;  Cout <= '0'; --op_AxnorB;
                  WHEN op_0 =>  Zout<= "00000000";  Cout <= '0'; --op_0;
                  WHEN op_1 =>  Zout<= "11111111";  Cout <= '0'; --op_1; 
                  WHEN op_incA => binadd(A, "00000000", Cin, Ztemp, CoutTemp);
                                          Zout <= Ztemp; Cout <= CoutTemp; --op_incA;
                  WHEN op_incB => binadd(B, "00000000", Cin, Ztemp, CoutTemp);
                                          Zout <= Ztemp; Cout <= CoutTemp; --op_incB;
                  WHEN op_decA => binsub(A, "10000000", '0', Ztemp, CoutTemp);
                                          Zout <= Ztemp; Cout <= CoutTemp; --op_decA;                                                                          
                  WHEN op_decB => binsub(B, "10000000", '0', Ztemp, CoutTemp);
                                          Zout <= Ztemp; Cout <= CoutTemp; --op_decB;
                  WHEN op_negA => binadd(NOT A, "00000000", '1', Ztemp, CoutTemp);
                                          Zout <= Ztemp; Cout <= CoutTemp; --op_negA;
                  WHEN op_negB => binadd(NOT B, "00000000", '1', Ztemp, CoutTemp);
                                          Zout <= Ztemp; Cout <= CoutTemp; --op_negB;
                  WHEN op_AplusB | op_AplusBwC => binadd (A, B, Cin, Ztemp, CoutTemp); 
                                          Zout <= Ztemp; Cout <= CoutTemp; --op_AplusB, op_AplusBwC, 
                  WHEN op_AminB | op_AminBwC => binsub(A, B, Cin, Ztemp, CoutTemp);
                                          Zout <= Ztemp; Cout <= CoutTemp; --op_AminB, op_AminBwC, op_BminA;  
                  WHEN op_BminA => binsub(B, A, Cin, Ztemp, CoutTemp);
                                          Zout <= Ztemp; Cout <= CoutTemp; -- op_BminA  
	               WHEN OTHERS         =>  NULL;
                  END CASE;
                  
            
          
                      
            END PROCESS alu_8bit;     
        
        
        
        
      
                  
        END  behavioral; 



-------------------------------------------------------------------------------
--  The Test bench for testing of the 8 bit ALU 
-------------------------------------------------------------------------------
ENTITY p6 IS
END p6;

-------------------------------------------------------------------------------
--  The Test Bench Architecture
-------------------------------------------------------------------------------
USE work.step6_package.all;

ARCHITECTURE test OF p6 IS

--  TYPE operations IS (op_A,op_B,op_notA,op_notB,op_AxorB,op_AorB,op_AandB,
--                    op_AnandB,op_AxnorB,op_0,op_1,op_incA,op_incB,op_decA,
--                    op_decB,op_negA,op_negB,op_AplusB,op_AplusBwC,
--                    op_AminB,op_AminBwC,op_BminA);
  TYPE oper_type  IS ARRAY (0 to 21) of operations;
  CONSTANT      oper_tbl     : oper_type := (op_A,op_B,op_notA,op_notB,
                   op_AxorB,op_AorB,op_AandB,op_AnandB,op_AxnorB,op_0,
                   op_1,op_incA,op_incB,op_decA,op_decB,op_negA,op_negB,
                   op_AplusB,op_AplusBwC,op_AminB,op_AminBwC,op_BminA);
  SIGNAL        oper : operations;

  TYPE result_table IS ARRAY(op_A to op_BminA,1 to 7) of bit_vector(7 downto 0);
  TYPE result_array is ARRAY(1 to 7) of bit_vector(7 downto 0);
  CONSTANT res_tbl : result_table :=
(("00000000","00000000","11111111","11111111","01010101","11110000","10110010"),-- op_A
("00000000","11111111","00000000","11111111","10101010","00001111","00101011"),-- op_B
("11111111","11111111","00000000","00000000","10101010","00001111","01001101"),-- not_A
("11111111","00000000","11111111","00000000","01010101","11110000","11010100"),-- not_B
("00000000","11111111","11111111","00000000","11111111","11111111","10011001"),-- AxorB
("00000000","11111111","11111111","11111111","11111111","11111111","10111011"),-- A or B
("00000000","00000000","00000000","11111111","00000000","00000000","00100010"),-- AandB
("11111111","11111111","11111111","00000000","11111111","11111111","11011101"),-- AnandB
("11111111","00000000","00000000","11111111","00000000","00000000","01100110"),-- AxnorB
("00000000","00000000","00000000","00000000","00000000","00000000","00000000"),-- op_0
("11111111","11111111","11111111","11111111","11111111","11111111","11111111"),-- op_1
("00000001","00000001","00000000","00000000","01010110","11110001","10110011"),-- inc A
("00000001","00000000","00000001","00000000","10101011","00010000","00101100"),-- inc B
("11111111","11111111","11111110","11111110","01010100","11101111","10110001"),-- dec A
("11111111","11111110","11111111","11111110","10101001","00001110","00101010"),-- dec B
("00000000","00000000","00000001","00000001","10101011","00010000","01001110"),-- neg_A
("00000000","00000001","00000000","00000001","01010110","11110001","11010101"),-- neg_B
("00000000","11111111","11111111","11111110","11111111","11111111","11011101"),-- AplusB
("00000000","11111111","11111111","11111110","11111111","11111111","11011101"),--A+B+0
("00000000","00000001","11111111","00000000","10101011","11100001","10000111"),-- A-B
("00000000","00000001","11111111","00000000","10101011","11100001","10000111"),-- A-B-0
("00000000","11111111","00000001","00000000","01010101","00011111","01111001"));-- B-A
  CONSTANT AplBpl1_tbl : result_array :=
("00000001","00000000","00000000","11111111","00000000","00000000","11011110");-- A+B+1
  CONSTANT AmBm1_tbl : result_array :=
("11111111","00000000","11111110","11111111","10101010","11100000","10000110");-- A-B-1
  TYPE carry_result_table IS ARRAY(op_A to op_BminA,1 to 7) of bit;
  TYPE carry_result_array IS ARRAY (1 to 7) of bit;
  CONSTANT carry_res_tbl : carry_result_table :=
                    (('0','0','0','0','0','0','0'), -- op_A
                     ('0','0','0','0','0','0','0'), -- op_B
                     ('0','0','0','0','0','0','0'), -- notA
                     ('0','0','0','0','0','0','0'), -- notB
                     ('0','0','0','0','0','0','0'), -- AxorB
                     ('0','0','0','0','0','0','0'), -- AorB
                     ('0','0','0','0','0','0','0'), -- AandB
                     ('0','0','0','0','0','0','0'), -- AnandB
                     ('0','0','0','0','0','0','0'), -- AnxorB
                     ('0','0','0','0','0','0','0'), -- op_0
                     ('0','0','0','0','0','0','0'), -- op_1
                     ('0','0','1','1','0','0','0'), -- incA
                     ('0','1','0','1','0','0','0'), -- incB
                     ('1','1','0','0','0','0','0'), -- decA
                     ('1','0','1','0','0','0','0'), -- decB
                     ('1','1','0','0','0','0','0'), -- negA
                     ('1','0','1','0','0','0','0'), -- negB
                     ('0','0','0','1','0','0','0'), -- A+B
                     ('0','0','0','1','0','0','0'), -- A+B+0
                     ('0','1','0','0','1','0','0'), -- A-B
                     ('0','1','0','0','1','0','0'), -- A-B-0
                     ('0','0','1','0','0','1','1')); -- B-A
  CONSTANT carry_AplBpl1_tbl : carry_result_array :=
                     ('0','1','1','1','1','1','0'); -- A+B+1
  CONSTANT carry_AmBm1_tbl : carry_result_array :=
                     ('1','1','0','1','1','0','0'); -- A-B-1

  SIGNAL        A,B,Zout    : bit_vector (7 downto 0);
  SIGNAL        Cin,Cout    : bit;
  SIGNAL        Zexp        : bit_vector (7 downto 0);
  SIGNAL        Error,Cexp  : bit;
  SIGNAL        Zerr,Cerr   : bit;
  SIGNAL        TestVecNo   : integer;

  -- Enter your name in the (  )
  TYPE mname IS (TENG);
  SIGNAL nm : mname := mname'VAL(0);


  --  Enter the COMPONENT declaration and configuration for your
  --    8 bit ALU here 
COMPONENT   alt_8bit_alu  IS
	    PORT (	alu_op  :  IN  operations;
			A, B         :  IN  BIT_VECTOR ( 7 downto  0);
			Cin	     :  IN  BIT;
			Zout	     :  OUT  BIT_VECTOR ( 7 downto  0);
			Cout         :  OUT  BIT  );
END  COMPONENT;

FOR AB3 : alt_8bit_alu  USE ENTITY WORK.alt_8bit_alu(behavioral);

BEGIN  --  test 

  --  Enter the instantiation for your 8 bit ALU
  --  Note that the inputs are per the diagram in lecture (slide 8)
  --     except that only A and B are driven by the applytest process
  --     Also note that A and B are Bit vectors!!!
  --  Also note that the P, K, and R signals are gone.  You must now
  --     use the signal Oper to control the operation of the ALU.
  --  The carry input is Cin, the carry output Cout
  --  The data output of the alu slice is Zout which is also now a bit
  --     vector.

AB3: alt_8bit_alu PORT MAP(Oper, A, B, Cin, Zout, Cout);


  applytests : PROCESS

    PROCEDURE wait_n_check IS
      variable cor_res : bit_vector (7 downto 0);
      variable cor_cout : bit;
    BEGIN
      WAIT for 90 ns;
      IF (oper = op_AplusBwC and Cin = '1') THEN
        cor_res := AplBpl1_tbl(TestVecNo);
        cor_cout := carry_AplBpl1_tbl(TestVecNo);
      ELSIF (oper = op_AminBwC and Cin = '1') THEN
        cor_res := AmBm1_tbl(TestVecNo);
        cor_cout := carry_AmBm1_tbl(TestVecNo);
      ELSE
        cor_res := res_tbl(oper,TestVecNo);
        cor_cout := carry_res_tbl(oper,TestVecNo);
      END IF;
      IF (cor_res /= Zout OR cor_cout /= Cout) THEN
           Error <= '1', '0' after 10 ns;
      END IF;
      IF (cor_res /= Zout) THEN Zerr <= '1','0' AFTER 10 ns; END IF;
      IF (cor_cout /= Cout) THEN Cerr <= '1','0' AFTER 10 ns; END IF;
      Zexp <= cor_res; Cexp <= cor_cout;
      WAIT for 10 ns;
    END wait_n_check;

  BEGIN  --  PROCESS applytests 
    outter: FOR i IN 0 TO 21 LOOP
    Oper <= oper_tbl(i);

    IF (i >= 11 and i <= 16) THEN Cin <= '1';
                             ELSE Cin <= '0';
    END IF;
  
    A <= "00000000"; B <= "00000000"; TestVecNo <= 1;
    wait_n_check;
    A <= "00000000"; B <= "11111111"; TestVecNo <= 2;
    wait_n_check;
    A <= "11111111"; B <= "00000000"; TestVecNo <= 3;
    wait_n_check;
    A <= "11111111"; B <= "11111111"; TestVecNo <= 4;
    wait_n_check;
    A <= "01010101"; B <= "10101010"; TestVecNo <= 5;
    wait_n_check;
    A <= "11110000"; B <= "00001111"; TestVecNo <= 6;
    wait_n_check;
    A <= "10110010"; B <= "00101011"; TestVecNo <= 7;
    wait_n_check;

    IF (i = 18 or i = 20) THEN
      Cin <= '1';
      A <= "00000000"; B <= "00000000"; TestVecNo <= 1;
      wait_n_check;
      A <= "00000000"; B <= "11111111"; TestVecNo <= 2;
      wait_n_check;
      A <= "11111111"; B <= "00000000"; TestVecNo <= 3;
      wait_n_check;
      A <= "11111111"; B <= "11111111"; TestVecNo <= 4;
      wait_n_check;
      A <= "01010101"; B <= "10101010"; TestVecNo <= 5;
      wait_n_check;
      A <= "11110000"; B <= "00001111"; TestVecNo <= 6;
      wait_n_check;
      A <= "10110010"; B <= "00101011"; TestVecNo <= 7;
      wait_n_check;
    END IF;

    END LOOP outter;
    WAIT;
  END PROCESS applytests;

  nm<= mname'VAL(0);

END test;