-------------------------------------------------------------------------
--
-- I M P L E M E N T A Ç Ã O   P A R C I A L  D O  M I P S   (nov/2010)
--
--  ImPoRtAnTe :   VERSÃO  SEM MULTIPLICAÇÃO/DIVISÃO
--
--  Professores     Fernando Moraes / Ney Calazans
--
--  ==> The top-level processor entity is MRstd
--  21/06/2010 - Bug corrigido no mux que gera op1 - agora recebe npc e
--		não pc.
--  17/11/2010 (Ney) - Bugs corrigidos:
--	1 - Decodificação das instruções BGEZ e BLEZ estava incompleta
--		Modificadas linhas 395 e 396 abaixo
--	2 - Definição de que linhas escolhem o registrador a ser escrito
--	nas instruções de deslocamento (SSLL, SLLV, SSRA, SRAV, SSRL e SRLV)
--		Acrescentadas linhas 325 a 327 abaixo
-------------------------------------------------------------------------

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- package with basic types
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.Std_Logic_1164.all;

package p_MRstd is  
    
    -- inst_type defines the instructions decodeable by the control unit
    type inst_type is  
            ( ADDU, SUBU, AAND, OOR, XXOR, NNOR, SSLL, SLLV, SSRA, SRAV, SSRL, SRLV,
            ADDIU, ANDI, ORI, XORI, LUI, LBU, LW, SB, SW, SLT, SLTU, SLTI,
            SLTIU, BEQ, BGEZ, BLEZ, BNE, J, JAL, JALR, JR, MULTU, DIVU, MFHI, MFLO, NOP, 
            invalid_instruction);
 
    type microinstruction is record
            CY1:   std_logic;       -- command of the first stage
            CY2:   std_logic;       --    "    of the second stage
            walu:  std_logic;       --    "    of the third stage
            wmdr:  std_logic;       --    "    of the fourth stage
            wpc:   std_logic;       -- PC write enable
            wreg:  std_logic;       -- register bank write enable
            ce:    std_logic;       -- Chip enable and R_W controls
            rw:    std_logic;
            bw:    std_logic;       -- Byte-word control (mem write only)
            i:     inst_type;       -- operation specification
            ini_mult: std_logic;
            ini_div: std_logic;
    end record;
         
end p_MRstd;


--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Generic register  
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;

entity regnbit is
           generic( INIT_VALUE : STD_LOGIC_VECTOR(31 downto 0) := (others=>'0') );
           port(  ck, rst, ce : in std_logic;
                  D : in  STD_LOGIC_VECTOR (31 downto 0);
                  Q : out STD_LOGIC_VECTOR (31 downto 0)
               );
end regnbit;

architecture regn of regnbit is 
begin

  process(ck, rst)
  begin
       if rst = '1' then
              Q <= INIT_VALUE(31 downto 0);
       elsif ck'event and ck = '0' then
           if ce = '1' then
              Q <= D; 
           end if;
       end if;
  end process;
        
end regn;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Register Bank (R0..R31) - 31 GENERAL PURPOSE 16-bit REGISTERS
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.Std_Logic_1164.all;
use ieee.STD_LOGIC_UNSIGNED.all;   
use work.p_MRstd.all;

entity reg_bank is
       port( ck, rst, wreg :    in std_logic;
             AdRs, AdRt, adRd : in std_logic_vector( 4 downto 0);
             RD : in std_logic_vector(31 downto 0);
             R1, R2: out std_logic_vector(31 downto 0) 
           );
end reg_bank;

architecture reg_bank of reg_bank is
   type bank is array(0 to 31) of std_logic_vector(31 downto 0);
   signal reg : bank ;                            
   signal wen : std_logic_vector(31 downto 0) ;
begin            

    g1: for i in 0 to 31 generate        

        -- Remember register $0 is the constant 0, not a register.
        -- This is implemented by never enabling writes to register $0
        wen(i) <= '1' when i/=0 and adRD=i and wreg='1' else '0';
         
        -- Remember register $29, the stack pointer, points to some place
        -- near the bottom of the data memory, not the usual place 
		-- assigned by the MIPS simulator!!
        g2: if i=29 generate -- SP ---  x10010000 + x800 -- top of stack
           r29: entity work.regnbit generic map(INIT_VALUE=>x"10010800")    
                                  port map(ck=>ck, rst=>rst, ce=>wen(i), D=>RD, Q=>reg(i));
        end generate;  
                
        g3: if i/=29 generate 
           rx: entity work.regnbit port map(ck=>ck, rst=>rst, ce=>wen(i), D=>RD, Q=>reg(i));                    
        end generate;
                   
   end generate g1;   
    

    R1 <= reg(CONV_INTEGER(AdRs));    -- source1 selection 

    R2 <= reg(CONV_INTEGER(AdRt));    -- source2 selection 
   
end reg_bank;



--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- ALU - operation depends only on the current instruction 
--       (decoded in the control unit)
--
-- 22/11/2004 - subtle error correctionwas done for J!
-- Part of the work for J has been done before, by shifting IR(15 downto 0)
-- left by two bits before writing data to the IMED register 
--
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.std_logic_arith.all;
use work.p_MRstd.all;

entity alu is
       port( op1, op2 : 	in std_logic_vector(31 downto 0);
             outalu :   	out std_logic_vector(31 downto 0);   
             op_alu :			in inst_type   
           );
end alu;

architecture alu of alu is 
   signal menorU, menorS : std_logic ;
begin
  
    menorU <=  '1' when op1 < op2 else '0';
    menorS <=  '1' when ieee.Std_Logic_signed."<"(op1,  op2) else '0' ; -- signed
    
    outalu <=  
        op1 - op2                                when  op_alu=SUBU                     else
        op1 and op2                              when  op_alu=AAND  or op_alu=ANDI     else 
        op1 or  op2                              when  op_alu=OOR   or op_alu=ORI      else 
        op1 xor op2                              when  op_alu=XXOR  or op_alu=XORI     else 
        op1 nor op2                              when  op_alu=NNOR                     else 
        op2(15 downto 0) & x"0000"               when  op_alu=LUI                      else
        (0=>menorU, others=>'0')                 when  op_alu=SLTU  or op_alu=SLTIU    else   -- signed
        (0=>menorS, others=>'0')                 when  op_alu=SLT   or op_alu=SLTI     else   -- unsigned
        op1(31 downto 28) & op2(27 downto 0)     when  op_alu=J     or op_alu=JAL      else 
        op1                                      when  op_alu=JR    or op_alu=JALR     else 
        to_StdLogicVector(to_bitvector(op1) sll  CONV_INTEGER(op2(10 downto 6)))   when  op_alu=SSLL   else 
        to_StdLogicVector(to_bitvector(op2) sll  CONV_INTEGER(op1(5 downto 0)))    when  op_alu=SLLV   else 
        to_StdLogicVector(to_bitvector(op1) sra  CONV_INTEGER(op2(10 downto 6)))   when  op_alu=SSRA   else 
        to_StdLogicVector(to_bitvector(op2) sra  CONV_INTEGER(op1(5 downto 0)))    when  op_alu=SRAV   else 
        to_StdLogicVector(to_bitvector(op1) srl  CONV_INTEGER(op2(10 downto 6)))   when  op_alu=SSRL   else 
        to_StdLogicVector(to_bitvector(op2) srl  CONV_INTEGER(op1(5 downto 0)))    when  op_alu=SRLV   else 
        op1 + op2;    -- default for ADDU,ADDIU,LBU,LW,SW,SB,BEQ,BGEZ,BLEZ,BNE    

end alu;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- multiplica
--	breve explição...
--	op1 vai mutiplicar op2 como?
--	primeiro se espera uma sinal de start ser gerado pela unit.controle,
--	quando ocorrido o hardware da multiplicaçao coloca '0's no reg_Hi e 
--	o valor de op1 reg_Lo, ficando com VVVVVV:(obs: fiz o exemplo com 9 downto 0 para nao ficar muito grando)
--		reg_Hi = 00000000000 (reg hi tem um bit a mais) 
--		reg_Lo = 0000000101 <<<< valor de op1
--	Depois disso é hora de fazer a multiplicaçao (soma sucessivas, olhar PS = Soma)
--	para isso se verifica se o bit 0 do reg_Lo é '1' se sim entao se soma o op2 em reg_Hi
--		op2 = 0000010101
--		reg_Hi = 00000000000 + '0' & 0000010101; 
--		reg_Hi = 00000010101
--	Depois passa para a etapa de deloca em que se move um bit para direita do reg_Hi para o reg_Lo
--		reg_Hi = 0 & 0000001010--1 <<< esse bit vai para o reg_Lo
--		reg_Lo = 1 & 000000010---1 <<< esse bit nao exite mais
--	obtemos entao:
--		reg_Hi = 00000001010
--		reg_Lo = 1000000010
--	apartir daqui fica em loop o soma e o desloca 32 vezes
--		reg_Lo = 1000000010 = bit 0 = '0' nao acontece a soma 
--		reg_Hi = 00000001010
--	
--	desloca:
--		reg_Hi = 0 & 0000000101--0 <<< esse bit vai para o reg_Lo
--		reg_Lo = 0 & 100000001---0 <<< esse bit nao exite mais
--	obtemos entao:
--		reg_Hi = 00000000101
--		reg_Lo = 0100000001
--	soma:
--		reg_Lo = 0100000001 = bit 0 = '1' acontece a soma
--		reg_Hi = 00000000101 + '0' & 0000010101;
--		reg_Hi = 00000011010
--	agora desloca-se 32 vezes ate ter: 
--		reg_Lo = 00001101001 <<<< resultado final 
--
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity multiplica is
	port (
		ck        : in STD_LOGIC;
		start     : in STD_LOGIC; 
		op1       : in STD_LOGIC_VECTOR(31 downto 0); 
		op2       : in STD_LOGIC_VECTOR(31 downto 0); 
		end_mult  : out STD_LOGIC; 
		P_Hi      : out STD_LOGIC_VECTOR(31 downto 0);
		A_Lo      : out STD_LOGIC_VECTOR(31 downto 0) 
	);
end multiplica;

architecture arq_mult of multiplica is
 
	type type_states is (Inicio, Soma, Desloca);
	signal PS, NS : type_states;

	signal reg_Hi : std_logic_vector(32 downto 0);
	signal reg_Lo : std_logic_vector(31 downto 0);
	signal cont : integer;
begin
	
	process (ck)
	begin
		if ck'EVENT and ck = '1' then
			PS <= NS;
		end if;
	end process;

	process (PS, start, cont)
	begin
		case PS is
			when Inicio => 
				if start = '1' then
					NS <= Soma;
				else
					NS <= Inicio;
				end if;
			when Soma => 
				NS <= Desloca;
			when Desloca => 
				if (cont = 32) then
					NS <= Inicio;
				else
					NS <= Soma;
				end if;
		end case; 
	end process;

	process (ck, start) 
	begin
		if ck'EVENT and ck = '1' then
			case PS is
				when Inicio => 
					if start = '1' then
						reg_Hi <= (others => '0'); 	-- reg_HI possui o resultado da multiplicação
						reg_Lo <= op1; 				-- reg_LO possui o valor que esta sendo multiplicado
						cont <= 0; 					-- conta ate 32 quando chegar no trinta e dois acaba o somador
					end if;
					end_mult <= '0';

				when Soma => 
					if reg_Lo(0) = '1' then
						reg_Hi <= reg_Hi + ('0' & op2);
					end if;
					cont <= cont + 1;

				when Desloca => 
					if cont = 32 then
						end_mult <= '1';
					end if;
					reg_Hi <= '0' & reg_Hi (32 downto 1); -- desloca para a direita uma casa
					reg_Lo <= reg_Hi(0) & reg_Lo (31 downto 1); -- desloca para a direita uma casa e add o primeiro bit (0) do hi no 31 do low

			end case;
		end if;
	end process;
	 
	P_Hi <= reg_Hi(31 downto 0);
	A_Lo <= reg_Lo;

end arq_mult;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- divisao --- nao ta funfando 
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.Std_Logic_unsigned.all;

entity divide is                  
      port( ck 				:	in  std_logic;
            start 			:   in  std_logic; 
            op1				:   in  std_logic_vector(31 downto 0);
            op2				:	in  std_logic_vector(31 downto 0);
            end_div 		:	out std_logic;
            divisao, resto 	:	out std_logic_vector(31 downto 0));
end divide;

architecture arq_div of divide is   
		type State_type is (Inicio, Desloca, subtrai, esc_quoc, restaura, fim);
		signal PS, NS: State_type;

		signal reg_Hi 		:	std_logic_vector(32 downto 0);
		signal reg_Lo 		:	std_logic_vector(31 downto 0);
--		signal regP 		:	std_logic_vector(64 downto 0); -- tentar substituir por reg_Hi reg_Lo 
--		signal regB 		:	std_logic_vector(32   downto 0);
		signal diferenca 	:	std_logic_vector(32   downto 0);
		signal cont 		:	integer;
begin


   process(start, ck)
   begin    
     if start='1'then
         reg_Hi 	<= (others=>'0');
         reg_Lo 	<= op2;
        -- regB  		<= '0' & op1;
         cont  		<= 0;
         endop 		<= '0';

     elsif ck'event and ck='1' then 
     
            if PS=desloca then
               -- regP  <= regP(63 downto 0) & regP(64);
                reg_Hi <= reg_Hi (31 downto 0) & reg_Lo (31);
				reg_Lo <= reg_Lo (30 downto 0) & reg_Hi (32);

            elsif PS=subtrai then  
            	
            	diferenca <= reg_Hi - ('0' & op1); --- parte hi do regP - regB(divisor)

                if diferenca(32)='1' then  
                      reg_Lo(0)<='0';
                else
                      reg_Lo(0)<='1';
                      reg_Hi <= diferenca;
                end if;
                
                cont <= cont + 1;
                
            elsif PS=termina then
                      resto   <= reg_Hi;
                      divisao <= reg_Lo;     
            end if;
        end if;       
    end process;
   
    end_div  <= '1' when PS=termina else '0';
   
   -- maquina de estados para controlar a DIVISAO
 		process (ck)
    begin
    		if ck'event and ck='1' then  
                PS <= NS;
      	end if;
   	end process;


  	process (start, PS, cont)
  	begin
		   case PS is
		      when inicio   =>  
		      				if start='1' then  
		      						NS <= desloca;  
		      				else   
		      						NS <= inicio;   
		      				end if;
		      when desloca  =>  
		      				NS <= subtrai;
		      when subtrai  =>  
		      				if cont=32 then 
                            	NS <= termina; 
                            else 
                            	NS <= desloca;  
                            end if;
		      when termina 	=>   
		      				NS <= fim;
		   end case; 
   	end process;



end arq_div;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Datapath structural description
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.Std_Logic_signed.all; -- needed for comparison instructions SLTxx
use IEEE.Std_Logic_arith.all; -- needed for comparison instructions SLTxx
use work.p_MRstd.all;
   
entity datapath is
      port(  ck, rst :     in std_logic;
             i_address :   out std_logic_vector(31 downto 0);
             instruction : in std_logic_vector(31 downto 0);
             d_address :   out std_logic_vector(31 downto 0);
             data :        inout std_logic_vector(31 downto 0);  
             uins :        in microinstruction;
             end_mult, end_div	:		out std_logic;
             IR_OUT :      out std_logic_vector(31 downto 0)
          );
end datapath;

architecture datapath of datapath is
    signal incpc, pc, npc, IR,  result, R1, R2, RA, RB, RIN, ext16, cte_im, IMED, op1, op2, 
           outalu, RALU, MDR, mdr_int, dtpc, D_Lo, D_Hi, Hi, Lo, mult_Lo, mult_Hi, quociente, resto : std_logic_vector(31 downto 0) := (others=> '0'); -- P_Lo R_Hi
    
    signal adD, adS : std_logic_vector(4 downto 0) := (others=> '0');    
    signal inst_branch, inst_grupo1, inst_grupoI, Hi_Lo_en, end_mult_en, end_div_en: std_logic;   
    signal salta : std_logic := '0';
begin

   -- auxiliary signals 
   inst_branch  <= '1' when uins.i=BEQ or uins.i=BGEZ or uins.i=BLEZ or uins.i=BNE else 
                  '0';
                  
   inst_grupo1  <= '1' when uins.i=ADDU or uins.i=SUBU or uins.i=AAND
                         or uins.i=OOR or uins.i=XXOR or uins.i=NNOR else
                   '0';

   inst_grupoI  <= '1' when uins.i=ADDIU or uins.i=ANDI or uins.i=ORI or uins.i=XORI else
                   '0';

   --==============================================================================
   -- first_stage
   --==============================================================================
  
   incpc <= pc + 4;
  
   RNPC: entity work.regnbit port map(ck=>ck, rst=>rst, ce=>uins.CY1, D=>incpc, Q=>npc);     
           
   RIR: entity work.regnbit  port map(ck=>ck, rst=>rst, ce=>uins.CY1, D=>instruction, Q=>IR);

   IR_OUT <= ir ;    -- IR is the datapath output signal to carry the instruction
             
   i_address <= pc;  -- connects PC output to the instruction memory address bus
   
   
   --==============================================================================
   -- second stage
   --==============================================================================
                
   -- The then clause is only used for logic shifts with shamt field       
   adS <= IR(20 downto 16) when uins.i=SSLL or uins.i=SSRA or uins.i=SSRL else 
          IR(25 downto 21);
          
   REGS: entity work.reg_bank(reg_bank) port map
        (ck=>ck, rst=>rst, wreg=>uins.wreg, AdRs=>adS, AdRt=>ir(20 downto 16), adRD=>adD,  
         Rd=>RIN, R1=>R1, R2=>R2);
    
   -- sign extension 
   ext16 <=  x"FFFF" & IR(15 downto 0) when IR(15)='1' else
             x"0000" & IR(15 downto 0);
    
   -- Immediate constant
   cte_im <= ext16(29 downto 0)  & "00"     when inst_branch='1'     else
                -- branch address adjustment for word frontier
             "0000" & IR(25 downto 0) & "00" when uins.i=J or uins.i=JAL else
                -- J/JAL are word addressed. MSB four bits are defined at the ALU, not here!
             x"0000" & IR(15 downto 0) when uins.i=ANDI or uins.i=ORI  or uins.i=XORI else
                -- logic instructions with immediate operand are zero extended
             ext16;
                -- The default case is used by addiu, lbu, lw, sbu and sw instructions
             
   -- second stage registers
   REG_S:  entity work.regnbit port map(ck=>ck, rst=>rst, ce=>uins.CY2, D=>R1,     Q=>RA);

   REG_T:  entity work.regnbit port map(ck=>ck, rst=>rst, ce=>uins.CY2, D=>R2,     Q=>RB);
  
   REG_IM: entity work.regnbit port map(ck=>ck, rst=>rst, ce=>uins.CY2, D=>cte_im, Q=>IMED);
 
 
  --==============================================================================
   -- third stage
   --==============================================================================
                      
   -- select the first ALU operand                           
   op1 <= npc  when inst_branch='1' else 
          RA; 
     
   -- select the second ALU operand
   op2 <= RB when inst_grupo1='1' or uins.i=SLTU or uins.i=SLT or uins.i=JR 
                  or uins.i=SLLV or uins.i=SRAV or uins.i=SRLV else 
          IMED; 
                 
   -- ALU instantiation
   inst_alu: entity work.alu port map (op1=>op1, op2=>op2, outalu=>outalu, op_alu=>uins.i);
                                   
   -- ALU register
   REG_alu: entity work.regnbit  port map(ck=>ck, rst=>rst, ce=>uins.walu, D=>outalu, Q=>RALU);               
 
   -- evaluation of conditions to take the branch instructions
   salta <=  '1' when ( (RA=RB  and uins.i=BEQ)  or (RA>=0  and uins.i=BGEZ) or
                        (RA<=0  and uins.i=BLEZ) or (RA/=RB and uins.i=BNE) )  else
             '0';
                  
   -- mult/div

	inst_mult: entity work.multiplica 
   						port map (ck=>ck, start=>uins.ini_mult, op1=>RA, op2=>RB, end_mult=>end_mult_en, P_Hi=>mult_Hi, A_Lo=>mult_Lo);


	inst_div: entity work.divide 
   						port map (ck=>ck, start=>uins.ini_div, op1=>RA, op2=>RB, end_div=>end_div_en, resto=>resto, divisao=>quociente);

	end_mult <=	end_mult_en;
	end_div <= end_div_en;

	D_Hi <= mult_Hi when uins.i=MULTU else 
   		  	 resto; 
   	D_Lo <= mult_Lo when uins.i=MULTU else 
   		   quociente; 

   Hi_Lo_en <= '1' when (uins.walu='1' and ((uins.i=DIVU and end_div_en='1') or (uins.i=MULTU and end_mult_en='1'))) else '0';

   REG_HI: entity work.regnbit  
   				port map(ck=>ck, rst=>rst, ce=>HiLo_en, D=>D_Hi, Q=>Hi);               
   REG_LO: entity work.regnbit  
   				port map(ck=>ck, rst=>rst, ce=>HiLo_en, D=>D_Lo, Q=>Lo);      
   --==============================================================================
   -- fourth stage
   --==============================================================================
     
   d_address <= RALU;
    
   -- tristate to control memory write    
   data <= RB when (uins.ce='1' and uins.rw='0') else (others=>'Z');  

   -- single byte reading from memory  -- SUPONDO LITTLE ENDIAN
   mdr_int <= data when uins.i=LW  else
              x"000000" & data(7 downto 0);
       
   RMDR: entity work.regnbit  port map(ck=>ck, rst=>rst, ce=>uins.wmdr, D=>mdr_int, Q=>MDR);                 
  
 	 result <=   Hi when uins.i=MFHI else
               Lo when uins.i=MFLO else
               MDR when uins.i=LW  or uins.i=LBU else
               RALU;
   --==============================================================================
   -- fifth stage
   --==============================================================================

   -- signal to be written into the register bank
   RIN <= npc when (uins.i=JALR or uins.i=JAL) else result;
   
   -- register bank write address selection
   adD <= "11111"               when uins.i=JAL else -- JAL writes in register $31
         IR(15 downto 11)       when inst_grupo1='1' or uins.i=SLTU or uins.i=SLT
                                                     or uins.i=JALR  
						     or uins.i=SSLL or uins.i=SLLV
						     or uins.i=SSRA or uins.i=SRAV
						     or uins.i=SSRL or uins.i=SRLV
						     or uins.i=MFHI or uins.i=MFLO else --- endereso salvar no banco de dados
         IR(20 downto 16) -- inst_grupoI='1' or uins.i=SLTIU or uins.i=SLTI 
        ;                 -- or uins.i=LW or  uins.i=LBU  or uins.i=LUI, or default
    
   dtpc <= result when (inst_branch='1' and salta='1') or uins.i=J    or uins.i=JAL or uins.i=JALR or uins.i=JR  
           else npc;
   
   -- Code memory starting address: beware of the OFFSET! 
   -- The one below (x"00400000") serves for code generated 
   -- by the MARS simulator
   rpc: entity work.regnbit generic map(INIT_VALUE=>x"00400000")   
                            port map(ck=>ck, rst=>rst, ce=>uins.wpc, D=>dtpc, Q=>pc);

end datapath;

--------------------------------------------------------------------------
--------------------------------------------------------------------------
--  Control Unit behavioral description 
--------------------------------------------------------------------------
--------------------------------------------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;
use work.p_MRstd.all;

entity control_unit is
        port(   ck, rst : in std_logic;          
                uins : out microinstruction;
                end_mult, end_div : in std_logic;
                ir : in std_logic_vector(31 downto 0)
             );
end control_unit;
                   
architecture control_unit of control_unit is
   type type_state is (Sidle, Sfetch, Sreg, Salu, Swbk, Sld, Sst, Ssalta);
   signal PS, NS : type_state;
   signal i : inst_type;      
begin
      
    ----------------------------------------------------------------------------------------
    -- BLOCK (1/3) - INSTRUCTION DECODING and ALU operation definition.
    -- This block generates 1 Output Function of the Control Unit
    ----------------------------------------------------------------------------------------
    i <=   NOP		when ir(31 downto 0) =x"00000000" else
    			 ADDU   when ir(31 downto 26)="000000" and ir(10 downto 0)="00000100001" else
           SUBU   when ir(31 downto 26)="000000" and ir(10 downto 0)="00000100011" else
           AAND   when ir(31 downto 26)="000000" and ir(10 downto 0)="00000100100" else
           OOR    when ir(31 downto 26)="000000" and ir(10 downto 0)="00000100101" else
           XXOR   when ir(31 downto 26)="000000" and ir(10 downto 0)="00000100110" else
           NNOR   when ir(31 downto 26)="000000" and ir(10 downto 0)="00000100111" else
           SSLL   when ir(31 downto 21)="00000000000" and ir(5 downto 0)="000000" else
           SLLV   when ir(31 downto 26)="000000" and ir(10 downto 0)="00000000100" else
           SSRA   when ir(31 downto 21)="00000000000" and ir(5 downto 0)="000011" else
           SRAV   when ir(31 downto 26)="000000" and ir(10 downto 0)="00000000111" else
           SSRL   when ir(31 downto 21)="00000000000" and ir(5 downto 0)="000010" else
           SRLV   when ir(31 downto 26)="000000" and ir(10 downto 0)="00000000110" else
           ADDIU  when ir(31 downto 26)="001001" else
           ANDI   when ir(31 downto 26)="001100" else
           ORI    when ir(31 downto 26)="001101" else
           XORI   when ir(31 downto 26)="001110" else
           LUI    when ir(31 downto 26)="001111" else
           LW     when ir(31 downto 26)="100011" else
           LBU    when ir(31 downto 26)="100100" else
           SW     when ir(31 downto 26)="101011" else
           SB     when ir(31 downto 26)="101000" else
           SLTU   when ir(31 downto 26)="000000" and ir(5 downto 0)="101011" else
           SLT    when ir(31 downto 26)="000000" and ir(5 downto 0)="101010" else
           SLTIU  when ir(31 downto 26)="001011"                             else
           SLTI   when ir(31 downto 26)="001010"                             else
           BEQ    when ir(31 downto 26)="000100" else
           BGEZ   when ir(31 downto 26)="000001" and ir(20 downto 16)="00001" else
           BLEZ   when ir(31 downto 26)="000110" and ir(20 downto 16)="00000" else
           BNE    when ir(31 downto 26)="000101" else
           J      when ir(31 downto 26)="000010" else
           JAL    when ir(31 downto 26)="000011" else
           JALR   when ir(31 downto 26)="000000"  and ir(20 downto 16)="00000"
                                           				and ir(10 downto 0) = "00000001001" else
           JR     when ir(31 downto 26)="000000" and ir(20 downto 0)="000000000000000001000" else
           MULTU  when ir(31 downto 26)="000000" and ir(15 downto 0)=x"0019" else
           DIVU   when ir(31 downto 26)="000000" and ir(15 downto 0)=x"001B" else
           MFHI   when ir(31 downto 16)=x"0000" and ir(10 downto 0)="00000010000" else
           MFLO   when ir(31 downto 16)=x"0000" and ir(10 downto 0)="00000010010" else
           invalid_instruction ; -- IMPORTANT: default condition is invalid instruction;
        
    assert i /= invalid_instruction
          report "******************* INVALID INSTRUCTION *************"
          severity error;
                   
    uins.i <= i;    -- this instructs the alu to execute its expected operation, if any

    ----------------------------------------------------------------------------------------
    -- BLOCK (2/3) - DATAPATH REGISTERS load control signals generation.
    ----------------------------------------------------------------------------------------
    uins.CY1   <= '1' when PS=Sfetch         else '0';
            
    uins.CY2   <= '1' when PS=Sreg           else '0';
  
    uins.walu  <= '1' when PS=Salu           else '0';
                
    uins.wmdr  <= '1' when PS=Sld            else '0';
  
    uins.wreg   <= '1' when (PS=Swbk or (PS=Ssalta and (i=JAL or i=JALR))) and i /= NOP else  '0'; -- nop nao escreve nos banco de registrador (tem que testar)
   
    uins.rw    <= '0' when PS=Sst            else  '1';
                  
    uins.ce    <= '1' when PS=Sld or PS=Sst  else '0';
    
    uins.bw    <= '0' when PS=Sst and i=SB   else '1';
      
    uins.wpc   <= '1' when PS=Swbk or PS=Sst or PS=Ssalta  
    																				 or (PS=Salu and ((i=DIVU and end_div='1') or (i=MULTU and end_mult='1'))) else
   			  				'0';

   	uins.ini_mult  <= '1' when PS=Sreg and i=MULTU else '0';
  
    uins.ini_div  <= '1' when PS=Sreg and i=DIVU else '0';
    ---------------------------------------------------------------------------------------------
    -- BLOCK (3/3) - Sequential part of the control unit - two processes implementing the
    -- Control Unit state register and the next-state (combinational) function
    --------------------------------------------------------------------------------------------- 
    process(rst, ck)
    begin
       if rst='1' then
            PS <= Sidle;          -- Sidle is the state the machine stays while processor is being reset
       elsif ck'event and ck='1' then
       
            if PS=Sidle then
                  PS <= Sfetch;
            else
                  PS <= NS;
            end if;
                
       end if;
    end process;
     
     
    process(PS, i, end_mult, end_div)
    begin
       case PS is         
      
            when Sidle=>NS <= Sidle; -- reset being active, the processor do nothing!       

            -- first stage:  read the current instruction 
            --
            when Sfetch=>NS <= Sreg;  
     
            -- second stage: read the register banck and store the mask (when i=stmsk)
            --
            when Sreg=>NS <= Salu;  
             
            -- third stage: alu operation 
            --
            when Salu =>if i=LBU  or i=LW then 
                                NS <= Sld;  
                          elsif i=SB or i=SW then 
                                NS <= Sst;
                          elsif i=J or i=JAL or i=JALR or i=JR or i=BEQ
                                    or i=BGEZ or i=BLEZ  or i=BNE then 
                                NS <= Ssalta;
                          elsif ((i=MULTU and end_mult='0') or (i=DIVU and end_div='0')) then
                                NS <= Salu;
                          elsif ((i=DIVU and end_div='1') or (i=MULTU and end_mult='1')) then  
                                NS <= Sfetch;  
                          else 
                                NS <= Swbk; 
                          end if;
                         
            -- fourth stage: data memory operation  
            --
            when Sld=>  NS <= Swbk; 
            
            -- fifth clock cycle of most instructions  - GO BACK TO FETCH
            -- 
            when Sst | Ssalta | Swbk=>NS <= Sfetch;
  
       end case;

    end process;
    
end control_unit;

--------------------------------------------------------------------------
-- Top-level instantiation of the MRstd Datapath and Control Unit
--------------------------------------------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;
use work.p_MRstd.all;

entity MRstd is
    port( clock, reset: in std_logic;
          ce, rw, bw: out std_logic;
          i_address, d_address: out std_logic_vector(31 downto 0);
          instruction: in std_logic_vector(31 downto 0);
          data: inout std_logic_vector(31 downto 0));
end MRstd;

architecture MRstd of MRstd is
      signal IR: std_logic_vector(31 downto 0);
      signal uins: microinstruction;
      signal end_mult, end_div : std_logic;
 begin

     dp: entity work.datapath   
         port map( ck=>clock, rst=>reset, IR_OUT=>IR, uins=>uins, i_address=>i_address, 
                   instruction=>instruction, d_address=>d_address,  data=>data,
                   end_mult=>end_mult, end_div=>end_div);

     ct: entity work.control_unit port map( ck=>clock, rst=>reset, IR=>IR, end_mult => end_mult, end_div => end_div, uins=>uins);
         
     ce <= uins.ce;
     rw <= uins.rw; 
     bw <= uins.bw;
     
end MRstd;
