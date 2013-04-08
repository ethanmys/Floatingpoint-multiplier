-- Engineer: Ethan Kho
-- Function: Floating point multiplier.The multiplier will accept IEEE Standard 754
-- single precision inputs and produce single precision output.  It will suppor
-- NaN, ±∞, ±0, normalized numbers, and denormalized numbers.

----------------------------------------------------------
-- Enter the ENTITY for your FPM here
-- Be sure to do the correct library and pacakage mappings
----------------------------------------------------------
LIBRARY  IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.std_logic_arith.all;
USE IEEE.std_logic_unsigned.ALL;
USE work.step6_package.ALL;

ENTITY FPM IS 
   PORT ( A, B: IN std_logic_vector (31 downto 0);
          latch: IN std_ulogic;
          drive: IN std_ulogic;
          C: OUT std_logic_vector (31 downto 0) 
          );
      END FPM;
      
       


----------------------------------------------------------
-- Enter the ARCHITECTURE for your FPM here
----------------------------------------------------------
ARCHITECTURE behavioral OF FPM IS



BEGIN
                
       
        
        
        PROCESS (latch, drive, A, B)
           
                      
                     
            --First version
            PROCEDURE A_multi_B ( A, B : IN BIT_VECTOR(23 downto 0) ;
                                  P : OUT BIT_VECTOR (47 downto 0)) IS
            VARIABLE A_temp, B_temp, P_temp, Ztemp : BIT_VECTOR(23 downto 0);
            
            VARIABLE Cin, Couttemp : BIT;
            BEGIN                       
            A_temp := A; B_temp := B; P_temp :=(others=>'0');
        
            FOR i IN A'reverse_range LOOP
            IF(B_temp(0) ='1' ) THEN 
            binadd(P_temp, A_temp, '0', Ztemp, CoutTemp);
            P_temp := Ztemp; Cin := CoutTemp;
            END IF;                      
            B_temp := B_temp SRL 1;
            B_temp(23):= P_temp(0);
            P_temp := P_temp SRL 1;
            P_temp(23) := Cin;
            
            END LOOP;   
            P := P_temp & B;                        
                                 
            END PROCEDURE  A_multi_B;
            
            --Second Version
            PROCEDURE AmB ( A, B : IN BIT_VECTOR(23 downto 0) ;
                                  P : OUT BIT_VECTOR (47 downto 0)) IS
            VARIABLE A_temp, P_temp, Ztemp : BIT_VECTOR(47 downto 0);
            VARIABLE B_temp: BIT_VECTOR(23 downto 0);
            VARIABLE Cin, Couttemp : BIT;
            BEGIN                       
            A_temp(23 downto 0) := A; B_temp := B; P_temp :=(others=>'0');
        
            FOR i IN B'reverse_range LOOP
            IF(B_temp(0) ='1' ) THEN 
            binadd(P_temp, A_temp, '0', Ztemp, CoutTemp);
            P_temp := Ztemp; 
            END IF;                      
            A_temp := A_temp SLL 1;
            
            B_temp := B_temp SRL 1;
            
            
            END LOOP;   
            P := P_temp ;                        
                                 
            END PROCEDURE  AmB;
            
            
           
           
           VARIABLE A_REG, B_REG, C_REG :std_logic_vector (31 downto 0);  
           
           VARIABLE A_M_S, B_M_S : std_logic_vector (23 downto 0);
           VARIABLE A_M_B, B_M_B : bit_vector (23 downto 0);
           VARIABLE A_EXP, B_EXP, R_EXP, T_EXP, EXP, N_EXP : INTEGER ; 
           VARIABLE P_REG_S :std_logic_vector (47 downto 0);
           VARIABLE P_REG_B :bit_vector (47 downto 0);
           
           
           BEGIN
                      
            --latch input signal
	        IF ((latch'LAST_VALUE = '0' OR latch'LAST_VALUE = 'L') AND
           (latch = '1'  OR  latch = 'H')  AND latch'EVENT) THEN -- trigger @ rising edge
       
            A_REG := A;
            B_REG := B;
            
            
            A_EXP :=conv_integer(A_REG(30 downto 23));
            B_EXP :=conv_integer(B_REG(30 downto 23));
            T_EXP := (A_EXP+B_EXP-127);
            N_EXP := A_EXP+B_EXP-126;
                
            
             
           END IF;
           
           --drive output signal
           IF (drive = '0'  OR  drive = 'L') THEN -- trigger @ low
               IF (A_REG(31)=B_REG(31)) THEN C(31) <= '0';
               ELSE C(31) <='1'; END IF;
               
               IF ((A_REG(30 downto 0)="1111111100000000000000000000001" OR 
                   B_REG(30 downto 0)="1111111100000000000000000000001")) 
               THEN 
                   C(31 downto 0) <= "01111111100000000000000000000001"; --A=NaN or B=NaN, C=NaN   
	            
	            ELSIF((A_REG(30 downto 0)="1111111100000000000000000000000" OR 
	                  B_REG(30 downto 0)="1111111100000000000000000000000")) THEN          
                     IF(A_REG(30 downto 0)="0000000000000000000000000000000" OR 
	                    B_REG(30 downto 0)="0000000000000000000000000000000")
	                    THEN C(31 downto 0) <= "01111111100000000000000000000001";
                     ELSE C(30 downto 0)<="1111111100000000000000000000000"; END IF; 
               
               ELSIF (A_REG(30 downto 0)="0000000000000000000000000000000" OR 
	                   B_REG(30 downto 0)="0000000000000000000000000000000") THEN
	                   IF(A_REG(30 downto 0)="1111111100000000000000000000000" OR
	                      B_REG(30 downto 0)="1111111100000000000000000000000") THEN
	                       C(30 downto 0) <= "1111111100000000000000000000001";
	                       ELSE C(30 downto 0) <= "0000000000000000000000000000000"; END IF;
	            ELSIF (A_REG(30 downto 0)="0111111100000000000000000000000" OR 
	                   B_REG(30 downto 0)="0111111100000000000000000000000") THEN
	                   IF(A_REG(30 downto 0)="0111111100000000000000000000000")THEN
	                       C(30 downto 0) <= B_REG(30 downto 0);  
	                       ELSE C(30 downto 0) <= A_REG(30 downto 0); END IF;
	            ELSIF ((A_EXP = 0) AND (B_EXP = 0)) THEN --DNORM*DNORM
                      C(30 downto 0) <= "0000000000000000000000000000000"; 
                 
               
              
                    
               ELSIF ((A_EXP=0)OR(B_EXP=0)) THEN -- DENORM
                     
                    

                     
                     IF(A_EXP=0)THEN
                     A_M_S(22 downto 0) := A_REG(22 downto 0); --std_logic_vector
                     A_M_S(23) := '0';
                     ELSE 
                     A_M_S(22 downto 0) := A_REG(22 downto 0); --std_logic_vector
                     A_M_S(23) := '1'; END IF;
                     
                     IF(B_EXP=0)THEN
                     B_M_S(22 downto 0) := B_REG(22 downto 0); --std_logic_vector
                     B_M_S(23) := '0';
                     ELSE 
                     B_M_S(22 downto 0) := B_REG(22 downto 0); --std_logic_vector
                     B_M_S(23) := '1'; END IF;
                     
                     
                     
                     A_M_B := to_bitvector(A_M_S); --convert to bit_vector
                     B_M_B := to_bitvector(B_M_S);
                     AmB(B_M_B, A_M_B, P_REG_B); 
                     
                          
                     IF((P_REG_B(47 downto 46) = "10") OR (P_REG_B(47 downto 46) = "11"))
                         THEN
                         EXP := (A_EXP+B_EXP-126+1);
                         IF(EXP<0) THEN    --treat unormalized case
                             P_REG_B := P_REG_B SLL EXP;
                              
                             C(30 downto 23) <= conv_std_logic_vector(0,8);
                             C(22 downto 0) <= to_stdlogicvector(P_REG_B(46 downto 24));
                         ELSE
                         
                         C(30 downto 23) <= conv_std_logic_vector(EXP,8);
                         C(22 downto 0) <= to_stdlogicvector(P_REG_B(46 downto 24)); END IF; 
                     ELSIF(P_REG_B(47 downto 46) = "01")
                         THEN 
                         EXP := A_EXP+B_EXP-126;
                         IF(EXP<0) THEN    --treat unormalized case
                             P_REG_B := P_REG_B SLL EXP;
                              
                             C(30 downto 23) <= conv_std_logic_vector(0,8);
                             C(22 downto 0) <= to_stdlogicvector(P_REG_B(46 downto 24));
                         ELSE
                         
                         C(30 downto 23) <= conv_std_logic_vector(EXP,8);
                         C(22 downto 0) <= to_stdlogicvector(P_REG_B(45 downto 23)); END IF;
                     
                     
                     
                     ELSIF(P_REG_B(47 downto 46) = "00") THEN
                         
                           
                         FOR i IN 0 TO 45  LOOP  --start normalize
                         
                             IF(P_REG_B(46)='1') THEN EXIT; END IF;
                             IF(N_EXP<=0) THEN EXIT; END IF;
                                                     
                             P_REG_B := P_REG_B SLL 1;
                             
                             
                             N_EXP := N_EXP-1;
                             
                             
                         
                         
                         END LOOP;
                         
                         
                         IF(N_EXP<0) THEN    --treat unormalized case
                             P_REG_B := P_REG_B SLL N_EXP;
                              
                             C(30 downto 23) <= conv_std_logic_vector(0,8);
                             C(22 downto 0) <= to_stdlogicvector(P_REG_B(46 downto 24));
                         ELSE
                         
                         C(30 downto 23) <= conv_std_logic_vector(N_EXP,8);
                         C(22 downto 0) <= to_stdlogicvector(P_REG_B(45 downto 23));
                     END IF;
                     
                        
            END IF;   
                
                ELSIF (((A_EXP >= 1)OR(A_EXP <= 254)) AND ((B_EXP >= 1)OR(B_EXP <= 254))) THEN
                                       
                     
                  
                  
                  
                  A_M_S(22 downto 0) := A_REG(22 downto 0); --std_logic_vector
                  A_M_S(23) := '1';
                  B_M_S(22 downto 0) := B_REG(22 downto 0); --std_logic_vector
                  B_M_S(23) := '1';
                  
                  
                  A_M_B := to_bitvector(A_M_S); --convert to bit_vector
                  B_M_B := to_bitvector(B_M_S);
                  AmB(B_M_B, A_M_B, P_REG_B); 
                  
                  IF((P_REG_B(47 downto 46) = "10") OR (P_REG_B(47 downto 46) = "11"))
                      THEN  R_EXP := (A_EXP+B_EXP-127+1);
                      C(30 downto 23) <= conv_std_logic_vector(R_EXP,8);
                      C(22 downto 0) <= to_stdlogicvector(P_REG_B(46 downto 24));
                  ELSIF(P_REG_B(47 downto 46) = "01")
                      THEN  R_EXP := (A_EXP+B_EXP-127);
                      C(30 downto 23) <= conv_std_logic_vector(R_EXP,8);
                      C(22 downto 0) <= to_stdlogicvector(P_REG_B(45 downto 23));
                                 
                  END IF;
                
              
                       
                END IF;                      
                         

                    
                 ELSE 
                 C <= "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"; --drive high impedence on bus  
                
           
      
           END IF;    
            
            
            
            
        END PROCESS;
        

END behavioral;


