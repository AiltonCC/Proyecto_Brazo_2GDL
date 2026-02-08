library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity Brazo2GL is
    Port (
        CLK        : in  STD_LOGIC;     			-- Reloj principal del sistema
        RST      	 : in  STD_LOGIC;      			-- Señal de reset
        UP		    : in  STD_LOGIC; 				-- Incrementar angulo
		  DN		    : in  STD_LOGIC; 				-- Decrementar angulo
        pwm_outB1  : out STD_LOGIC;       		-- Salida PWM BRAZO 1
		  pwm_outB2  : out STD_LOGIC;       		-- Salida PWM BRAZO 2
		  CARGAR		 : in STD_LOGIC;					-- BOTON PARA CARGAR DATOS
		  SEL	  		 : in STD_LOGIC;					-- BOTON PARA SELECCIONAR BRAZO
		  INICIAR	 : in STD_LOGIC;					-- BOTON PARA INICIAR SECUENCIA
		  RX			 :	IN STD_LOGIC;					-- COMUNICACION RX
		  SEG 		 : out  STD_LOGIC_VECTOR (7 downto 0);
        AN 			 : out  STD_LOGIC_VECTOR (4 downto 0)
    );
end Brazo2GL;

architecture PWM of Brazo2GL is
    --PWM CONFIG
	 constant clk_frec : integer := 50000000; 					-- Frecuencia del reloj 
    constant pwm_frec   : integer := 50; 	   					-- Frecuencia de PWM 
    constant frec  : integer := clk_frec / pwm_frec; 			-- Cuenta máxima para 50 Hz
	 
	 --ANGULOS
	 signal   angulo	 : UNSIGNED(7 downto 0);
	 signal anguloB1 		 :   unsigned (7 downto 0);
	 signal anguloB2 		 :   unsigned (7 downto 0);
	 signal ang_bcd  : unsigned(11 downto 0); -- 3 dígitos BCD

	 --PWM
    signal contador      : integer range 0 to frec-1 := 0; 	-- Contador
    signal duty_cycleB1   : integer range 0 to frec := 0;   	-- Ciclo de trabajo
	 signal duty_cycleB2   : integer range 0 to frec := 0;   	-- Ciclo de trabajo
    signal pwm_level    : integer := 0;       -- Nivel de PWM (1-2ms)
	 
	 --DESPLIEGUE
	SIGNAL DFREQ: INTEGER RANGE 0 TO 50000;
	SIGNAL SELL:INTEGER RANGE 0 TO 4;
	SIGNAL MUX: STD_LOGIC_VECTOR (3 DOWNTO 0);
	SIGNAL NREG: INTEGER RANGE 0 TO 15;
	
	--BRAZO
	SIGNAL BRAZO : INTEGER RANGE 1 TO 2;
	
	 -- Definición del TIPO DE MEMORIA (el array de N registros)
	constant N_REGS : integer := 3;
   type Register_Array is array (0 to N_REGS - 1) of std_logic_vector(7 downto 0);
	signal addr     : unsigned(3 downto 0) := (others => '0'); -- Dirección del registro a escribir/leer
	signal addrRX   : unsigned(3 downto 0) := (others => '0'); -- Dirección del registro a escribir/leer
	
    -- Declaración de la SEÑAL DE MEMORIA
    signal mem : Register_Array := (others => (others => '0')); -- Inicializado a cero
	 
	--VARIABLES PARA ANTIREBOTE
	SIGNAL T_ESTABLE	:	INTEGER RANGE 0 TO 500_000;
	SIGNAL UP_AN	:	STD_LOGIC;
	SIGNAL UP_OK	:	STD_LOGIC;
	SIGNAL UP_ONAN	:	STD_LOGIC;

	--VARIABLES PARA ANTIREBOTE
	SIGNAL T_ESTABLE1	:	INTEGER RANGE 0 TO 500_000;
	SIGNAL DN_AN	:	STD_LOGIC;
	SIGNAL DN_OK	:	STD_LOGIC;
	SIGNAL DN_ONAN	:	STD_LOGIC;

	--VARIABLES PARA ANTIREBOTE
	SIGNAL T_ESTABLE2	:	INTEGER RANGE 0 TO 500_000;
	SIGNAL INICIAR_AN	:	STD_LOGIC;
	SIGNAL INICIAR_OK	:	STD_LOGIC;
	SIGNAL INICIAR_ONAN	:	STD_LOGIC;

	--VARIABLES PARA ANTIREBOTE
	SIGNAL T_ESTABLE3	:	INTEGER RANGE 0 TO 500_000;
	SIGNAL CARGAR_AN	:	STD_LOGIC;
	SIGNAL CARGAR_OK	:	STD_LOGIC;
	SIGNAL CARGAR_ONAN	:	STD_LOGIC;

	--VARIABLES PARA ANTIREBOTE
	SIGNAL T_ESTABLE4	:	INTEGER RANGE 0 TO 500_000;
	SIGNAL SEL_AN	:	STD_LOGIC;
	SIGNAL SEL_OK	:	STD_LOGIC;
	SIGNAL SEL_ONAN	:	STD_LOGIC;
	
	--VARIABLES PARA LA COMINICACION UART
	CONSTANT BAUD_RATE_MAX 	: INTEGER := 5207; -- 5207 si cuentas de 0 a 5207 (5208 ciclos).
	SIGNAL BITRATE_RX	:	INTEGER RANGE 0 TO 5207 := 0;
	SIGNAL NBIT	:	INTEGER :=0;
	SIGNAL INI_RX	:	STD_LOGIC := '0';

	--VARIABLES PARA INICIAR RX
	SIGNAL RX_D1,RX_D2	:	STD_LOGIC;
	SIGNAL DATO		: 	STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0');

begin

--ANTIREBOTE INCREMENTAR ANGULO
UP_AN <= UP WHEN RISING_EDGE(CLK);
T_ESTABLE <= 0 WHEN (UP_AN = '0' AND UP = '1') OR (UP_AN = '1' AND UP = '0') OR RST = '1' OR T_ESTABLE = 500_000
					ELSE T_ESTABLE +1 WHEN RISING_EDGE(CLK);
UP_OK <= UP WHEN T_ESTABLE = 499_999 AND RISING_EDGE(CLK);
UP_ONAN <= UP_OK WHEN RISING_EDGE(CLK);

--ANTIREBOTE PARA DECREMENTAR ANGULO
DN_AN <= DN WHEN RISING_EDGE(CLK);
T_ESTABLE1 <= 0 WHEN (DN_AN = '0' AND DN = '1') OR (DN_AN = '1' AND DN = '0') OR RST = '1' OR T_ESTABLE1 = 500_000
					ELSE T_ESTABLE1 +1 WHEN RISING_EDGE(CLK);
DN_OK <= DN WHEN T_ESTABLE1 = 499_999 AND RISING_EDGE(CLK);
DN_ONAN <= DN_OK WHEN RISING_EDGE(CLK);

--ANTIREBOTE PARA INICIAR
INICIAR_AN <= INICIAR WHEN RISING_EDGE(CLK);
T_ESTABLE2 <= 0 WHEN (INICIAR_AN = '0' AND INICIAR = '1') OR (INICIAR_AN = '1' AND INICIAR = '0') OR RST = '1' OR T_ESTABLE2 = 500_000
					ELSE T_ESTABLE2 +1 WHEN RISING_EDGE(CLK);
INICIAR_OK <= INICIAR WHEN T_ESTABLE2 = 499_999 AND RISING_EDGE(CLK);
INICIAR_ONAN <= INICIAR_OK WHEN RISING_EDGE(CLK);

--ANTIREBOTE PARA CARGAR
CARGAR_AN <= CARGAR WHEN RISING_EDGE(CLK);
T_ESTABLE3 <= 0 WHEN (CARGAR_AN = '0' AND CARGAR = '1') OR (CARGAR_AN = '1' AND CARGAR = '0') OR RST = '1' OR T_ESTABLE3 = 500_000
					ELSE T_ESTABLE3 +1 WHEN RISING_EDGE(CLK);
CARGAR_OK <= CARGAR WHEN T_ESTABLE3 = 499_999 AND RISING_EDGE(CLK);
CARGAR_ONAN <= CARGAR_OK WHEN RISING_EDGE(CLK);

--ANTIREBOTE PARA SELECCIONAR BRAZO
SEL_AN <= SEL WHEN RISING_EDGE(CLK);
T_ESTABLE4 <= 0 WHEN (SEL_AN = '0' AND SEL = '1') OR (SEL_AN = '1' AND SEL = '0') OR RST = '1' OR T_ESTABLE4 = 500_000
					ELSE T_ESTABLE4 +1 WHEN RISING_EDGE(CLK);
SEL_OK <= SEL WHEN T_ESTABLE4 = 499_999 AND RISING_EDGE(CLK);
SEL_ONAN <= SEL_OK WHEN RISING_EDGE(CLK);

--DETECCIÓN DE FLANCO (START BIT)
PROCESS (CLK)
BEGIN
    IF RISING_EDGE(CLK) THEN
        RX_D1 <= RX;
        RX_D2 <= RX_D1;
    END IF;
END PROCESS;

--COMUNICACION
--PROCESS (CLK, RST, angulo)
--variable write_addrRX : integer range 0 to N_REGS - 1;
--BEGIN
--    IF RST = '1' THEN
--        INI_RX <= '0';
--        BITRATE_RX <= 0;
--        NBIT <= 0;
--        --angulo <= (OTHERS => '0');
--		  addrRX <= X"0";
--
--    ELSIF RISING_EDGE(CLK) THEN
--
--        IF INI_RX = '0' AND RX_D2 = '1' AND RX_D1 = '0' THEN
--            INI_RX <= '1';
--            BITRATE_RX <= BAUD_RATE_MAX / 2; -- Iniciamos en la mitad del ciclo para muestrear en el centro del Start Bit.
--            NBIT <= 0; 								-- Contador de bits a 0 (el primer bit es el Start Bit)
--				IF	addrRX = X"2" THEN
--					addrRX <= X"0";
--				END IF;
--        
--        -- Lógica de Recepción (cuando INI_RX_FLAG = '1')
--        ELSIF INI_RX = '1' THEN						
--            -- Contador de Baud Rate
--            IF BITRATE_RX = BAUD_RATE_MAX THEN
--                BITRATE_RX <= 0; 	-- Reinicia el contador de Baud Rate
--                NBIT <= NBIT + 1; 	-- Avanza al siguiente bit
--					 
--                IF NBIT = 0 THEN
--                    NULL;
--						  
--                ELSIF NBIT >= 1 AND NBIT <= 8 THEN
--                    angulo(NBIT - 1) <= RX; -- Bit 1 va a angulo(0), Bit 2 va a angulo(1), etc.
--
--                -- Muestreo de Stop Bit (NBIT_CNT = 9)
--                ELSIF NBIT = 9 THEN
--                    write_addrRX := to_integer(unsigned(addrRX));
--						  mem(write_addrRX) <= std_LOGIC_VECTOR(angulo); -- Transfiere el angulo completo a la memoria
--                    
--                    -- Finaliza la recepción
--                    INI_RX <= '0';
--                    NBIT <= 0;
--                    addrRX <= addrRX + 1;
--                END IF;
--                
--            ELSE
--                BITRATE_RX <= BITRATE_RX + 1; -- Incrementa el contador de Baud Rate
--            END IF;
--        END IF;
--    END IF;
--END PROCESS;

 -- PROCESO PARA CONTROL MANUAL Y GENERACION PWM
    process(CLK, RST, CARGAR, INICIAR, SEL, UP, DN, angulo)
	 -- Conversión del vector de dirección a un entero
        variable write_addr : integer range 0 to N_REGS - 1;
    begin
	 --RESET
        if RST = '1' then
            contador <= 0;
            pwm_outB1 <= '0';
				pwm_outB2 <= '0';
				BRAZO <= 1;
				addr <= X"0";
				angulo <= X"00";
				anguloB1 <= X"00";
				anguloB2 <= X"00";
				mem(write_addr) <= X"00";
				INI_RX <= '0';
			   BITRATE_RX <= 0;
			   NBIT <= 0;
			   addrRX <= X"0";
	
        elsif rising_edge(CLK) then
		  
			--COMINICACION UART RX
					IF INI_RX = '0' AND RX_D2 = '1' AND RX_D1 = '0' THEN
						INI_RX <= '1';
						BITRATE_RX <= BAUD_RATE_MAX / 2; -- Iniciamos en la mitad del ciclo para muestrear en el centro del Start Bit.
						NBIT <= 0; 								-- Contador de bits a 0 (el primer bit es el Start Bit)
						angulo <= X"00";
						IF	addrRX = X"3" THEN
							addrRX <= X"0";
						END IF;
				  
				  -- Lógica de Recepción (cuando INI_RX_FLAG = '1')
				  ELSIF INI_RX = '1' THEN						
						-- Contador de Baud Rate
						IF BITRATE_RX = BAUD_RATE_MAX THEN
							 BITRATE_RX <= 0; 	-- Reinicia el contador de Baud Rate
							 NBIT <= NBIT + 1; 	-- Avanza al siguiente bit
							 
							 IF NBIT = 0 THEN
								  NULL;
								  
							 ELSIF NBIT >= 1 AND NBIT <= 8 THEN
								  angulo(NBIT - 1) <= RX; -- Bit 1 va a angulo(0), Bit 2 va a angulo(1), etc.

							 -- Muestreo de Stop Bit (NBIT_CNT = 9)
							 ELSIF NBIT = 9 THEN
								  write_addr := to_integer(unsigned(addrRX));
								  mem(write_addr) <= std_LOGIC_VECTOR(angulo); -- Transfiere el angulo completo a la memoria
								  
								  -- Finaliza la recepción
								  INI_RX <= '0';
								  NBIT <= 0;
								  addrRX <= addrRX + 1;
							 END IF;
							 
						ELSE
							 BITRATE_RX <= BITRATE_RX + 1; -- Incrementa el contador de Baud Rate
						END IF;
				  END IF;
		  
			--SELECCION DEL BRAZO			
				  if SEL_ONAN = '0' AND SEL_OK =  '1' then
						--conteo <= X"000";
						angulo <= X"00";
						if BRAZO = 2 then
							BRAZO <= 1;
							addr <= addr - 1;
							--addr <= X"0";
						else
							BRAZO <= BRAZO + 1;
							addr <= addr + 1;
						end if;
						
			--INCREMENTA EL ANGULO			
				  elsif UP_ONAN = '0' AND UP_OK =  '1' then
						if angulo < X"B4" then
								angulo <= angulo + 1;
						end if;
						
			--DECREMENTA EL ANGULO			
				  elsif DN_ONAN = '0' AND DN_OK =  '1' then
						if angulo > X"00" then						
							angulo <= angulo - 1;
						end if;
						
			--CARGA DE LOS DATOS			
				  elsif CARGAR_ONAN = '0' AND CARGAR_OK =  '1' then
						write_addr := to_integer(unsigned(addr));
						mem(write_addr) <= std_LOGIC_VECTOR(angulo);
						
			--INICIO DE LA SECUENCIA			
				  elsif (INICIAR_ONAN = '0' AND INICIAR_OK =  '1') OR mem(2) = X"FF" then
						anguloB1 <= unsigned(mem(0));
						anguloB2 <= unsigned(mem(1));
						mem(2) <= X"00";
					END IF;
					
			--PERIODO DEL PWM		
					if contador < frec - 1 then	--Cuenta los ciclos de reloj para completar un periodo del PWM.
						 contador <= contador + 1;
					else
						 contador <= 0;
					end if;

			-- Generación de la señal PWM BRAZO 1
					if contador < duty_cycleB1 then
						 pwm_outB1 <= '1';
					else
						 pwm_outB1 <= '0';
					end if;
					
			-- Generación de la señal PWM BRAZO 2
					if contador < duty_cycleB2 then
						 pwm_outB2 <= '1';
					else
						 pwm_outB2 <= '0';
					end if;
				
        end if;
		  
		  duty_cycleB1 <= 125_000 - 550 * to_integer(anguloB1);
			duty_cycleB2 <= 125_000 - 550 * to_integer(anguloB2);
    end process;
	 
--CONVERSION BIN TO BCD
process(angulo)
    variable bcd : unsigned(11 downto 0);
    variable i   : integer;
begin
    bcd := (others => '0');

    for i in 7 downto 0 loop
        if bcd(3 downto 0) > 4 then
            bcd(3 downto 0) := bcd(3 downto 0) + 3;
        end if;
        if bcd(7 downto 4) > 4 then
            bcd(7 downto 4) := bcd(7 downto 4) + 3;
        end if;
        if bcd(11 downto 8) > 4 then
            bcd(11 downto 8) := bcd(11 downto 8) + 3;
        end if;

        bcd := bcd(10 downto 0) & angulo(i);
    end loop;

    ang_bcd <= bcd;
end process;

--DESPLIEGUE EN EL DISPLAY 7 SEG
DFREQ <= 0 WHEN DFREQ=50000 ELSE
		DFREQ+1 WHEN RISING_EDGE(CLK);
		
SELL<= SELL+1 WHEN DFREQ=49999 AND RISING_EDGE(CLK);

AN<=	"00001" WHEN SELL = 0 ELSE
		"00010" WHEN SELL = 1 ELSE
		"00100" WHEN SELL = 2 ELSE
		"01000" WHEN SELL = 3 ELSE
		"10000";
		
SEG<= X"3F" WHEN MUX = X"0" ELSE
		X"06" WHEN MUX = X"1" ELSE
		X"5B" WHEN MUX = X"2" ELSE
		X"4F" WHEN MUX = X"3" ELSE
		X"66" WHEN MUX = X"4" ELSE
		X"6D" WHEN MUX = X"5" ELSE
		X"7D" WHEN MUX = X"6" ELSE
		X"07" WHEN MUX = X"7" ELSE
		X"7F" WHEN MUX = X"8" ELSE
		X"6F" WHEN MUX = X"9" ELSE
		X"77" WHEN MUX = X"A" ELSE
		X"3F";
		
MUX <= X"A" WHEN SELL = 0 ELSE
		 X"1" WHEN SELL = 1 AND BRAZO = 1 ELSE
		 X"2" WHEN SELL = 1 AND BRAZO = 2 ELSE
		 std_LOGIC_VECTOR(ang_bcd(11 downto 8)) WHEN SELL = 2 ELSE
		 std_LOGIC_VECTOR(ang_bcd(7 downto 4)) WHEN SELL = 3 ELSE
		 std_LOGIC_VECTOR(ang_bcd(3 downto 0));
		
end architecture;