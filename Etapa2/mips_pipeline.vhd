-------------------------------------------------------------------------
--
-- I M P L E M E N T A � � O   P A R C I A L  D O  M I P S   (nov/2010)
--
--  ImPoRtAnTe :   VERS�O  SEM MULTIPLICA��O/DIVIS�O
--
--  Professores     Fernando Moraes / Ney Calazans
--
--  ==> The top-level processor entity is MRstd
--  21/06/2010 - Bug corrigido no mux que gera op1 - agora recebe npc e
--		n�o pc.
--  17/11/2010 (Ney) - Bugs corrigidos:
--	1 - Decodifica��o das instru��es BGEZ e BLEZ estava incompleta
--		Modificadas linhas 395 e 396 abaixo
--	2 - Defini��o de que linhas escolhem o registrador a ser escrito
--	nas instru��es de deslocamento (SSLL, SLLV, SSRA, SRAV, SSRL e SRLV)
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
           ADDIU, ANDI, ORI, XORI, LUI, LBU, LW, SB, SW, SLT, SLTU, SLTI, SLTIU,
           BEQ, BGEZ, BLEZ, BNE, J, JAL, JALR, JR, MULTU, DIVU, MFHI, MFLO, NOP, --incremento das 5 novas instrucoes
           invalid_instruction);

   type microinstruction is record
            wreg:       std_logic;       -- register bank write enable
            ce:         std_logic;       -- Chip enable and R_W controls			
            rw:         std_logic;
            bw:         std_logic;       -- Byte-word control (mem write only)
            ULAFonte:   std_logic_vector(1 downto 0);       
            RegDst:     std_logic;  
            i:          inst_type;       -- operation specification
            ini_mult:   std_logic;      -- sinal de controle para incio mult
            ini_div:    std_logic;      --sinal de controle para incio divi
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
    

    R1 <= RD when adRd = AdRs else
	 		 reg(CONV_INTEGER(AdRs));    -- source1 selection 

    R2 <= RD when AdRt = adRd else 
	  		reg(CONV_INTEGER(AdRt));    -- source2 selection 
   
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
-- hardware para executar a multiplicacao por etapas
--	op1 vai mutiplicar op2 como?
--	atraves de soma sucessivas 
--	numero de somas/execucoes = numero de bits entre os operandos
-- antes de tudo, se coloca o valor de op1 no Low
-- depois disso, comecam as n execucoes, sendo que cada execucao consiste em:
-- primeiro, se o ultimo bit do Low = '1', entao se soma op2 em High, 
-- caso contrario, nada se altera
-- segundo, desloca para a direita o high e o low "concatenados"
-- 
-- Exemplo
-- op1 = 00110 (6)
-- op2 = 00101 (5)
--
--      reg_Hi reg_Lo
--       00000 00110  -> move-se o valor de op1 para o low
--
-- 1     00000 00110  reg_Lo = 0 nada altera 
--       00000 00011  >> Desloca para direita
-- 
-- 2     00101 00011  reg_Lo = 1 entao op2 eh somado ao reg_Hi 
--       00010 10001  >> desloca
--
-- 3     00111 10001  reg_Lo = 1 entao op2 eh somado ao reg_Hi
--       00011 11000  >> desloca
--                  
-- 4     00011 11000 reg_Lo = 0 nada altera
--       00001 11100 >> desloca
--
-- 5     00001 11100 reg_Lo = 0 nada altera
--       00000 11110 >> ultimo desloque antes de terminar 
--
-- termina a multiplicacao e obtemos o resultado 30
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

   -- maquina de estados da multiplicacao
	process (PS, start, cont)
	begin
		case PS is
			when Inicio => 
				if start = '1' then -- quando sinal de start eh ativo a maquina passa para o estado de soma
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
		if ck'event and ck = '1' then
			case PS is
				when Inicio => 
					if start = '1' then            -- inicia os signal quando o start for ativado
						reg_Hi <= (others => '0');  -- zera o Hi
						reg_Lo <= op1; 				
						cont <= 0;                 -- conta ate 32 quando chegar no 32 acaba o somador
					end if;
					end_mult <= '0';

				when Soma => 	
					if reg_Lo(0) = '1' then        -- se o primeiro bit do reg Lo for 1 devese somar o valor de op2 no reg Hi
						reg_Hi <= reg_Hi + ('0' & op2);
					end if;
					cont <= cont + 1;              -- incrementa o cont

				when Desloca => 
					if cont = 32 then              -- quando cont for igual a 32 a multiplicacao deve acabar
						end_mult <= '1';
					end if;
					reg_Hi <= '0' & reg_Hi (32 downto 1);            -- desloca para a direita uma casa
					reg_Lo <= reg_Hi(0) & reg_Lo (31 downto 1); 	    -- desloca para a direita uma casa e add o primeiro bit (0) do hi no 31 do low

			end case;
		end if;
	end process;
	 
	P_Hi <= reg_Hi(31 downto 0);					-- a parte do Reg_Hi vai para saida P_Hi
	A_Lo <= reg_Lo;									-- a parte do Reg_Lo contem o resultado da multiplicacao e vai para a saida A_Lo

end arq_mult;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- divisao 
-- op2 vai dividir op1 como?
-- atraves de subtracao sucessivas 
-- Exemplo
-- op1 = 11011
-- op2 = 00101
--
--      reg_Hi reg_Lo
--1      00000 11011
--       00001 10110
--       00001 10110  diferenca = 11100 nada altera 
-- 
--2      00011 01100
--       00011 01100  diferenca = 11110 nada altera 
--
--3      00110 11000
--       00001 11001  diferenca = 00001 entao altera o primeiro bit  
--                    para '1' e aparte alta recebe o valor da diferenca
--                  
--4      00011 10010
--       00011 10010  diferenca = 11110 nada altera 
--
--5      00111 00100
--       00010 00101  diferenca = 00010 entao altera o primeiro bit  
--                    para '1' e aparte alta recebe o valor da diferenca
--
-- obtemos o resultado 5 (quociente) no reg_Lo 
-- e como resto da divisao temos o valor 2 no reg_Hi
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.Std_Logic_unsigned.all;

entity divide is                  
      port( ck               :   in  std_logic;
            start            :   in  std_logic; 
            op1              :   in  std_logic_vector(31 downto 0);
            op2	           :   in  std_logic_vector(31 downto 0);
            end_div          :   out std_logic;
            divisao, resto   :   out std_logic_vector(31 downto 0)
          );
end divide;

architecture arq_div of divide is   
		type State_type is (inicio, desloca, subtrai, termina);
		signal PS, NS: State_type;

		signal reg_Hi         :	std_logic_vector(32 downto 0);
		signal reg_Lo         :	std_logic_vector(31 downto 0);
		signal diferenca      :	std_logic_vector(32 downto 0);
		signal cont           :	integer;
begin

   process(start, ck)
   begin    
     if PS= inicio and start='1' then       -- inicia os signal quando o start for ativado
         reg_Hi 	<= (others=>'0');      
         reg_Lo 	<= op1;
         cont     <= 0;                  -- contador de 1 ate 31

     elsif ck'event and ck='1' then 
     
            if PS=desloca then  
                reg_Hi <= reg_Hi (31 downto 0) & reg_Lo (31);
                reg_Lo <= reg_Lo (30 downto 0) & reg_Hi (32); 

            elsif PS=subtrai then  
                if diferenca(32)='1' then  
                      reg_Lo(0)<='0';
                else
                      reg_Lo(0)<='1';
                      reg_Hi <= diferenca;
                end if;
                
                cont <= cont + 1;                      
            end if;
        end if;       
    end process;
    
    diferenca <= reg_Hi - ('0' & op2);    
    resto   <= reg_Hi(31 downto 0);
    divisao <= reg_Lo;     
    end_div  <= '1' when PS=termina else '0';
   
 	process (ck)
    begin
    		if ck'event and ck='1' then  
                PS <= NS;
      	end if;
   	end process;

    -- maquina de estados da divisao
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
		      	 if cont=31 then 
                NS <= termina; 
             else 
                NS <= desloca;  
            end if;
		     
          when termina 	=>   
	      				NS <= inicio;
		   end case; 
   	end process;



end arq_div;


--++++++++++++++++++++++++++
-- BI/DI
--++++++++++++++++++++++++++

library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.Std_Logic_signed.all; -- needed for comparison instructions SLTxx
use IEEE.Std_Logic_arith.all; -- needed for comparison instructions SLTxx
use work.p_MRstd.all;
   
entity BI_DI is
      port(  ck, rst :              in std_logic;
             D_incpc :              in std_logic_vector(31 downto 0);
             D_instruction :        in std_logic_vector(31 downto 0);
             npc :                  out std_logic_vector(31 downto 0);
             ir :                   out std_logic_vector(31 downto 0);
             rs :                   out std_logic_vector(4 downto 0);
             rt :                   out std_logic_vector(4 downto 0);
             rd :                   out std_logic_vector(4 downto 0);
             ext :                  out std_logic_vector(15 downto 0)
          );
end BI_DI;

architecture arq_BI_DI of BI_DI is

   signal en : std_logic;
   signal instruction : std_logic_vector (31 downto 0);

begin
   en <= '1';
	
   RNPC: entity work.regnbit 
               port map(ck=>ck, rst=>rst, ce=>en, D=>D_incpc, Q=>npc);
   RIR: entity work.regnbit  
               port map(ck=>ck, rst=>rst, ce=>en, D=>D_instruction, Q=>instruction);
					
   ir <= instruction;
   rs <= instruction(25 downto 21);
   rt <= instruction(20 downto 16);
   rd <= instruction(15 downto 11);
   ext <= instruction(15 downto 0);

end arq_BI_DI;

--++++++++++++++++++++++++++++++++
-- DI_EX
--++++++++++++++++++++++++++++++++

library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.Std_Logic_signed.all; -- needed for comparison instructions SLTxx
use IEEE.Std_Logic_arith.all; -- needed for comparison instructions SLTxx
use work.p_MRstd.all;
   
entity DI_EX is
      port( ck, rst :               in std_logic;
            in_uins :               in microinstruction;
            D_incpc :               in std_logic_vector(31 downto 0);
            D_R1 :                  in std_logic_vector(31 downto 0);
            D_R2 :                  in std_logic_vector(31 downto 0);
            D_IMED :                in std_logic_vector(31 downto 0);
            D_rs :                  in std_logic_vector(4 downto 0);
            D_rd :                  in std_logic_vector(4 downto 0);
            D_rt :                  in std_logic_vector(4 downto 0);

            uins_EX :               out microinstruction;
            npc :                   out std_logic_vector(31 downto 0);
            RA :                    out std_logic_vector(31 downto 0);
            RB :                    out std_logic_vector(31 downto 0);
            IMEDS :                 out std_logic_vector(31 downto 0);
            rs :                    out std_logic_vector(4 downto 0);
            rd :                    out std_logic_vector(4 downto 0);
            rt :                    out std_logic_vector(4 downto 0)
          );
end DI_EX;

architecture arq_DI_EX of DI_EX is
	
	signal en : std_logic;
begin
   en <= '1';
	

   RNPC: entity work.regnbit 
               port map(ck=>ck, rst=>rst, ce=>en, D=>D_incpc, Q=>npc);

   REG_S:  entity work.regnbit 
               port map(ck=>ck, rst=>rst, ce=>en, D=>D_R1, Q=>RA);

   REG_T:  entity work.regnbit 
               port map(ck=>ck, rst=>rst, ce=>en, D=>D_R2, Q=>RB);

   REG_EXT:  entity work.regnbit 
               port map(ck=>ck, rst=>rst, ce=>en, D=>D_IMED, Q=>IMEDs);
					
	process(ck, rst)
    begin
         if rst = '1' then
            rs <= (others => '0');
            rd <= (others => '0');
            rt <= (others => '0');
         elsif ck'event and ck = '0' then
             if en = '1' then
               uins_EX <= in_uins;
               rs <= D_rs;
               rd <= D_rd;
               rt <= D_rt;
             end if;
         end if;
end process;

end arq_DI_EX;

---------
-- EX_MEM
--------
library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.Std_Logic_signed.all; -- needed for comparison instructions SLTxx
use IEEE.Std_Logic_arith.all; -- needed for comparison instructions SLTxx
use work.p_MRstd.all;
   
entity EX_MEM is
      port( ck, rst :               in std_logic;
            in_uins :               in microinstruction;
            D_outAlu :              in std_logic_vector(31 downto 0);
            D_EscMem :              in std_logic_vector(31 downto 0);
            D_rd :                  in std_logic_vector(4 downto 0);

            uins_Mem :              out microinstruction;
            RALU :                  out std_logic_vector(31 downto 0);
            EscMem :                out std_logic_vector(31 downto 0);
            rd :                    out std_logic_vector(4 downto 0)
      );
end EX_MEM;

architecture arq_EX_MEM of EX_MEM is
	
	signal en : std_logic;
begin
   en <= '1';
	
   REG_alu: entity work.regnbit  
               port map(ck=>ck, rst=>rst, ce=>en, D=>D_outAlu, Q=>RALU);  
	
   REG_EscMem:  entity work.regnbit 
               port map(ck=>ck, rst=>rst, ce=>en, D=>D_EscMem, Q=>EscMem);

					
   process(ck, rst)
    begin
         if rst = '1' then
            rd <= (others => '0');
         elsif ck'event and ck = '0' then
            if en = '1' then
               uins_Mem <= in_uins;
               rd <= D_rd;
            end if;
         end if;
   end process;

end arq_EX_MEM;

------
-- Mem_ER
-------
library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.Std_Logic_signed.all; -- needed for comparison instructions SLTxx
use IEEE.Std_Logic_arith.all; -- needed for comparison instructions SLTxx
use work.p_MRstd.all;
   
entity Mem_ER is
      port(  ck, rst :              in std_logic;
             in_uins :              in microinstruction;
             D_RALU :               in std_logic_vector(31 downto 0);
             D_MDR :                in std_logic_vector(31 downto 0);
             D_rd :                 in std_logic_vector(4 downto 0);
             
             uins_ER :              out microinstruction;
             RALU_ER :              out std_logic_vector(31 downto 0);
             MDR :                  out std_logic_vector(31 downto 0);
             rd :                   out std_logic_vector(4 downto 0)
          );
end Mem_ER;

architecture arq_Mem_ER of Mem_ER is
	
   signal en : std_logic;
begin
   en <= '1';
	
   REG_RALU: entity work.regnbit  
               port map(ck=>ck, rst=>rst, ce=>en, D=>D_RALU, Q=>RALU_ER);  
	
   RMDR:  entity work.regnbit 
               port map(ck=>ck, rst=>rst, ce=>en, D=>D_MDR, Q=>MDR);

					
   process(ck, rst)
      begin
         if rst = '1' then
            rd <= (others => '0');
         elsif ck'event and ck = '0' then
            if en = '1' then
               uins_ER <= in_uins;
               rd <= D_rd;
            end if;
         end if;
   end process;

end arq_Mem_ER;

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
      port(  ck, rst :              in std_logic;
             i_address :            out std_logic_vector(31 downto 0);
             instruction :          in std_logic_vector(31 downto 0);
             d_address :            out std_logic_vector(31 downto 0);
             data :                 inout std_logic_vector(31 downto 0);  
             uins :                 in microinstruction;
             end_mult, end_div	:  out std_logic;
             IR_OUT :               out std_logic_vector(31 downto 0)
          );
end datapath;

architecture datapath of datapath is
    signal incpc, pc, IR, npc_DI, npc_EX, result, R1, R2, RA, RB, RIN, ext16, cte_im, IMED, op1, op2, 
           outalu, RALU, MDR, mdr_int, dtpc, D_Lo, D_Hi, Hi, Lo, mult_Lo, EscMem, RALU_ER, Routalu,
           mult_Hi, quociente, resto : std_logic_vector(31 downto 0) := (others=> '0'); 
    
	 
    signal ext : std_logic_vector(15 downto 0) := (others => '0');
	-- signal IR : std_logic_vector(5 downto 0) := (others => '0');
    
    signal inst_branch, inst_grupo1, inst_grupoI, Hi_Lo_en, end_mult_en, end_div_en: std_logic;   
    signal salta : std_logic := '0';

    signal adD, adS, adS_DI, adD_DI, adRT, adS_EX, adD_EX, adD_ER, adRT_EX, adD_MEM : std_logic_vector(4 downto 0) := (others=> '0');
    signal uins_EX, uins_MEM, uins_ER :      microinstruction; -- sinais de controle das etapas EX MEM e ER

begin

   
   --==============================================================================
   -- first_stage
	-- primeiro estagio deve buscar uma nova instrução a cada borda do clock
   --==============================================================================
  
   incpc <= pc + 4; -- incrementa o pc 
	
     -- salva o valor incrementado
  
	dtpc <= incpc; --result when (inst_branch='1' and salta='1') or uins.i=J or uins.i=JAL or uins.i=JALR or uins.i=JR  
		--else incpc; -- mux pc
   
   -- Code memory starting address: beware of the OFFSET! 
   -- The one below (x"00400000") serves for code generated 
   -- by the MARS simulator
   rpc: entity work.regnbit generic map(INIT_VALUE=>x"00400000")   
 								  port map(ck=>ck, rst=>rst, ce=>'1', D=>dtpc, Q=>pc); -- reg PC
  
   i_address <= pc;  -- connects PC output to the instruction memory address bus
           
	-- barreira BI/DI
	Bi_Di : entity work.BI_DI
					port map(
					ck => ck, rst => rst, D_incpc => incpc, D_instruction => instruction, npc => npc_DI,
					ir => ir, rs => adS_DI,	rt => adRT, rd => adD_DI, ext => ext);
   
   --==============================================================================
   -- second stage
   --==============================================================================
                
   -- The then clause is only used for logic shifts with shamt field       
   
	IR_OUT <= ir;
	       
   REGS: entity work.reg_bank(reg_bank) port map
        (ck=>ck, rst=>rst, wreg=>uins.wreg, AdRs=>adS_DI, AdRt=>adRT, adRD=>adD_ER,  
         Rd=>RIN, R1=>R1, R2=>R2); -- EacReg =~ wreg
    
   -- sign extension 
   ext16 <=  x"FFFF" & ext when ext(15)='1' else
             x"0000" & ext;
				 

	Di_Ex : entity work.DI_EX
			 	   port map( ck => ck, rst => rst, in_uins => uins, D_incpc => npc_DI, D_R1 => R1, D_rt => adRT,
			 					 D_R2 => R2, D_IMED => ext16, D_rs => adS_DI, D_rd => adD_DI, uins_EX => uins_EX,
			 					 npc => npc_EX, RA => RA, RB => RB, IMEDS => IMED, rs => adS_EX, rd => adD_EX, rt => adRT_EX);         
	
 
 
  --==============================================================================
   -- third stage
   --==============================================================================
	-- Immediate constant
	
	adD <= "11111" when uins_EX.i=JAL else
	   	  adD_EX when uins_EX.RegDst = '1' else
			  adRT_EX;
	 
                -- The default case is used by addiu, lbu, lw, sbu and sw instructions
	-- auxiliary signals 
                   
   -- select the first ALU operand                           
   op1 <= npc_EX  when uins_EX.ULAFonte = "11" else 
          RA; 
     
   -- select the second ALU operand
   op2 <= RB when uins_EX.ULAFonte = "00" else --- <<<<< melhorar isso depois (isso vai para extençao de sinal)
			IMED(29 downto 0)  & "00" when uins_EX.ULAFonte = "11" else
					 -- branch address adjustment for word frontier
				 "0000" & adRT_EX & adS_EX & IMED(15 downto 0) & "00" when uins_EX.i=J or uins_EX.i=JAL else
					 -- J/JAL are word addressed. MSB four bits are defined at the ALU, not here!
				 x"0000" & IMED(15 downto 0) when uins_EX.i=ANDI or uins_EX.i=ORI  or uins_EX.i=XORI else
					 -- logic instructions with immediate operand are zero extended
				 IMED; 
                 
   -- ALU instantiation
   inst_alu: entity work.alu port map (op1=>op1, op2=>op2, outalu=>Routalu, op_alu=>uins_EX.i);
                                   
	outalu <=   Hi when uins_EX.i=MFHI else                -- para executar a instrucao MFHi e MFLo foram acresentadas
               Lo when uins_EX.i=MFLO else
               Routalu; 
 
   -- evaluation of conditions to take the branch instructions
   salta <=  '1' when ( (RA=RB  and uins_EX.i=BEQ)  or (RA>=0  and uins_EX.i=BGEZ) or
                        (RA<=0  and uins_EX.i=BLEZ) or (RA/=RB and uins_EX.i=BNE) )  else
             '0';
                  
   -- multiplicador
   inst_mult: entity work.multiplica 
                port map (ck=>ck, start=>uins_EX.ini_mult, op1=>RA, op2=>RB, 
					 				end_mult=>end_mult_en, P_Hi=>mult_Hi, A_Lo=>mult_Lo);

   -- divisor
   inst_div: entity work.divide 
                port map (ck=>ck, start=>uins_EX.ini_div, op1=>RA, op2=>RB, 
					 				end_div=>end_div_en, resto=>resto, divisao=>quociente);

   end_mult <=	end_mult_en;   -- sinal de saida da datapath para o control_unit do mult
   end_div  <= end_div_en;    -- sinal de saida da datapath para o control_unit do div

   D_Hi <= mult_Hi when uins_EX.i=MULTU else    -- mux para saber de onde vem o valor do REG_Hi
           resto; 
   D_Lo <= mult_Lo when uins_EX.i=MULTU else    -- mux para saber de onde vem o valor do REG_Lo
           quociente; 

   Hi_Lo_en <= '1' when (((uins_EX.i=DIVU and end_div_en='1') or              -- signal para gerar um sinal de escrita no REG_Hi e REG_Lo
                                            (uins_EX.i=MULTU and end_mult_en='1'))) else '0'; 

   -- Hi register
   REG_HI: entity work.regnbit  
  				port map(ck=>ck, rst=>rst, ce=>Hi_Lo_en, D=>D_Hi, Q=>Hi);

   -- Lo register               
   REG_LO: entity work.regnbit  
  				port map(ck=>ck, rst=>rst, ce=>Hi_Lo_en, D=>D_Lo, Q=>Lo);  
				
				
	Ex_Mem : entity work.EX_MEM
			 	   port map( ck => ck, rst => rst, in_uins => uins_EX, D_outAlu => outalu,
  				 				 D_EscMem => RB, D_rd => adD,	 uins_Mem => uins_MEM, RALU => RALU,
  				 	 			 EscMem => EscMem, rd => adD_MEM);	
						    
   --==============================================================================
   -- fourth stage
   --==============================================================================
     
   d_address <= RALU;
    
   -- tristate to control memory write    
   data <= EscMem when (uins_MEM.ce='1' and uins_MEM.rw='0') else (others=>'Z');  

   -- single byte reading from memory  -- SUPONDO LITTLE ENDIAN
   mdr_int <= data when uins_MEM.i=LW  else
              x"000000" & data(7 downto 0);
                        
  

					
	Mem_ER : entity work.Mem_ER
			 	   port map( ck => ck, rst => rst, in_uins => uins_MEM, D_RALU => RALU,
  				 				 D_MDR => mdr_int, D_rd => adD_MEM,	 uins_ER => uins_ER, RALU_ER => result,
  				 	 			 MDR => MDR, rd => adD_ER);				

   --==============================================================================
   -- fifth stage
   --==============================================================================

	-- signal to be written into the register bank
 	RIN <= npc_EX when (uins.i=JALR or uins.i=JAL) else 
			 MDR  when uins_MEM.i=LW  or uins.i=LBU else
			 result;
   
   -- register bank write address selection
   --adD <= "11111"               when uins.i=JAL else -- JAL writes in register $31
   --      IR(15 downto 11)       when inst_grupo1='1' or uins.i=SLTU or uins.i=SLT
   --                                                  or uins.i=JALR  
	--					     or uins.i=SSLL or uins.i=SLLV
	--					     or uins.i=SSRA or uins.i=SRAV
	--					     or uins.i=SSRL or uins.i=SRLV
	--					     or uins.i=MFHI or uins.i=MFLO else --- endereso salvar no banco de dados
   --      IR(20 downto 16) -- inst_grupoI='1' or uins.i=SLTIU or uins.i=SLTI 
   --     ;                 -- or uins.i=LW or  uins.i=LBU  or uins.i=LUI, or default
    


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
	signal inst_font : std_logic_vector(1 downto 0) ;
begin
      
    ----------------------------------------------------------------------------------------
    -- BLOCK (1/3) - INSTRUCTION DECODING and ALU operation definition.
    -- This block generates 1 Output Function of the Control Unit
    ----------------------------------------------------------------------------------------
    i <=   NOP		when ir(31 downto 0)=x"00000000" else
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
  
    uins.ULAFonte <= inst_font;
	 inst_font <= "01" when i=ADDIU or i=ANDI or i=ORI or i=XORI or i=LUI or i=SLTU or 
					  			  i=SLTI or i=LW or i=LBU or i=SW or i=SB else
					 "11" when i=BEQ or i=BGEZ or i=BLEZ or i=BNE else
  					 "00";
		
	 uins.RegDst <= '1' when inst_font = "00" else '0';

    uins.wreg   <= '1' when ((inst_font /= "11" or (i=JAL or i=JALR))) and i /= NOP else  '0'; -- nop nao escreve nos banco de registrador 
   
    uins.rw    <= '0' when i=SB or i=SW else  '1';
                  
    uins.ce    <= '1' when i=LBU  or i=LW or i=SB or i=SW  else '0';
    
    uins.bw    <= '0' when i=SB  else '1';
      
   -- uins.wpc   <= '1' when ck'event and ck = '0' else '0'; --when PS=Swbk or PS=Sst or PS=Ssalta  
   							--			  or (PS=Salu and ((i=DIVU and end_div='1') or (i=MULTU and end_mult='1'))) else
   			  				--'0';

    uins.ini_mult  <= '1' when i=MULTU else '0';
  
    uins.ini_div  <= '1' when i=DIVU else '0';
    ---------------------------------------------------------------------------------------------
    -- BLOCK (3/3) - Sequential part of the control unit - two processes implementing the
    -- Control Unit state register and the next-state (combinational) function
    --------------------------------------------------------------------------------------------- 
--    process(rst, ck)
--    begin
--       if rst='1' then
--            PS <= Sidle;          -- Sidle is the state the machine stays while processor is being reset
--       elsif ck'event and ck='1' then
--       
--            if PS=Sidle then
--                  PS <= Sfetch;
--            else
--                  PS <= NS;
--            end if;
--                
--       end if;
--    end process;
     
     
--    process(PS, i, end_mult, end_div)
--    begin
--       case PS is         
---      
---            when Sidle=>NS <= Sidle; -- reset being active, the processor do nothing!       
--
--            -- first stage:  read the current instruction 
--            --
--            when Sfetch=>NS <= Sreg;  
--     
            -- second stage: read the register banck and store the mask (when i=stmsk)
            --
--            when Sreg=>NS <= Salu;  
             
            -- third stage: alu operation 
            --
--            when Salu =>if i=LBU  or i=LW then 
--                                NS <= Sld;  
--                          elsif i=SB or i=SW then 
--                                NS <= Sst;
--                         elsif i=J or i=JAL or i=JALR or i=JR or i=BEQ
--                                    or i=BGEZ or i=BLEZ  or i=BNE then 
--                             NS <= Ssalta;
--                          elsif ((i=MULTU and end_mult='0') or (i=DIVU and end_div='0')) then
--                                NS <= Salu;
--                          elsif ((i=DIVU and end_div='1') or (i=MULTU and end_mult='1')) then  
--                                NS <= Sfetch;  
--                          else 
--                                NS <= Swbk; 
--                          end if;
                         
            -- fourth stage: data memory operation  
            --
--            when Sld=>  NS <= Swbk; 
            
            -- fifth clock cycle of most instructions  - GO BACK TO FETCH
            -- 
--            when Sst | Ssalta | Swbk=>NS <= Sfetch;
  
--       end case;

--    end process;
    
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
