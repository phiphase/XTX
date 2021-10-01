
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library std;
use std.textio.all;

library work;
use work.pkg_xtx.all;
use work.pkg_tb_xtx.all;

entity tb_xtx_top is
end tb_xtx_top;

architecture bench of tb_xtx_top is
constant PATH : string := "Z:/XTX/FPGA/PRJ/pulse_shaper/solution1/csim/build/";
constant DIN_FILENAME : string := "din.dat";
constant REF_FILENAME : string := "hw_dout.dat";
constant DOUT_FILENAME : string :="rtl_dout.dat";
constant MAX_NUM_IP_SAMPLES : natural := 200;

constant pSimtimeout_count : integer := 40;   
constant simtimeout        : time := 100000000 ps;
constant REF_CLK_PERIOD    : time := 8000 ps;    -- 125 MHz
constant GLBL_CLK_PERIOD   : time := 8000 ps;    -- 125 MHz
constant DRP_CLK_PERIOD    : time := 8000 ps;    -- 125 MHz
constant AXI_CLK_PERIOD    : time := 16000 ps;   -- 62.5 MHz
constant BIT_CLK_PERIOD    : time := GLBL_CLK_PERIOD/40;  -- 5 GHz lane rate (40 x GLBL CLK frequency)

constant pLanes : integer := 4;
constant pAlign_buf_size : integer := 64;
-- F = 2 K = 32
constant pF : integer := 2;    -- F: Octets per frame
constant pK : integer := 32;   -- K: Frames per multiframe

-- Setup the link configuration parameters.
constant pDID      : std_logic_vector(7 downto 0) := x"55";
constant pADJCNT   : std_logic_vector(3 downto 0) := x"0";
constant pBID      : std_logic_vector(3 downto 0) := x"A";
constant pADJDIR   : std_logic := '0';
constant pPHADJ    : std_logic := '0';
constant pSCR      : std_logic := '0';
constant pL        : std_logic_vector(4 downto 0)  := std_logic_vector(to_unsigned((pLanes-1), 5));
constant pM        : std_logic_vector(7 downto 0)  := std_logic_vector(to_unsigned((pLanes-1), 8));
constant pCS       : std_logic_vector(1 downto 0)  := "10";
constant pN        : std_logic_vector(4 downto 0)  := std_logic_vector(to_unsigned(13,5));
constant pNt       : std_logic_vector(4 downto 0)  := std_logic_vector(to_unsigned(15,5));
constant pSUBCV    : std_logic_vector(2 downto 0)  := "001";
constant pJESDV    : std_logic_vector(2 downto 0)  := "001";
constant p_S       : std_logic_vector(4 downto 0)  := (others=>'0');
constant pHD       : std_logic := '0';
constant pCF       : std_logic_vector(4 downto 0)  := "00001";
constant pRES1     : std_logic_vector(7 downto 0)  := x"5A";
constant pRES2     : std_logic_vector(7 downto 0)  := x"A5";
constant pInit_len : integer := pF*pK*4;

-- Fixed symbols
constant pK_is_r   : std_logic_vector(7 downto 0) := x"1C"; -- K28_0
constant pK_is_a   : std_logic_vector(7 downto 0) := x"7C"; -- K28_3
constant pK_is_q   : std_logic_vector(7 downto 0) := x"9C"; -- K28_4
constant pK_is_k   : std_logic_vector(7 downto 0) := x"BC"; -- K28_5 

-- Pulse shaper
constant Rs         : real := 230.0;  -- Symbol rate, MHz
constant Fs         : real := 500.0;  -- DAC input sample rate, MHz
constant OSR        : real := 8.0;    -- Over-sampling ratio

-- Pulse shaper test parameters
constant pDPHI      : real := Rs * OSR/Fs;
constant pGAIN      : real := 0.9997;
constant pALPHA     : std_logic := '1';   -- alpha = 0.35
constant pOQPSK     : std_logic := '0';   -- not OQPSK

-- DUT Declaration
component XTX_top
   port (
      reset                  : IN STD_LOGIC;
   
      -- Clock ports
      refclk_p               : IN STD_LOGIC;
      refclk_n               : IN STD_LOGIC;      
      drpclk                 : IN STD_LOGIC;

      -- JESD204B DAC Interface   
      txp                    : out std_logic_vector(3 downto 0);
      txn                    : out std_logic_vector(3 downto 0);
      tx_sysref              : in std_logic;
      tx_sync                : in std_logic;
      tx_sysref_req          : out std_logic;
   
      -- AXI4-Lite interface (temporary)
      axi_aclk             : in std_logic;
      axi_aresetn          : in std_logic;
      axi_awaddr           : in std_logic_vector(11 downto 0);
      axi_awvalid          : in std_logic;
      axi_awready          : out std_logic;
      axi_wdata            : in std_logic_vector(31 downto 0);
      axi_wstrb            : in std_logic_vector(3 downto 0);
      axi_wvalid           : in std_logic;
      axi_wready           : out std_logic;
      axi_bresp            : out std_logic_vector(1 downto 0);
      axi_bvalid           : out std_logic;
      axi_bready           : in std_logic;
      axi_araddr           : in std_logic_vector(11 downto 0);
      axi_arvalid          : in std_logic;
      axi_arready          : out std_logic;
      axi_rdata            : out std_logic_vector(31 downto 0);
      axi_rresp            : out std_logic_vector(1 downto 0);
      axi_rvalid           : out std_logic;
      axi_rready           : in std_logic;
      
      -- Testbench interface for the pulse shaper
      -- DIN0 input FIFO interface
      din_0                : IN std_logic_vector(2 downto 0);
      din_0_empty_n        : IN STD_LOGIC;
      din_0_read           : OUT STD_LOGIC;
      -- DIN 1 input FIFO interface
      din_1                : IN std_logic_vector(2 downto 0);
      din_1_empty_n        : IN STD_LOGIC;
      din_1_read           : OUT STD_LOGIC;

      -- Run-time controls      
      dphi                 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
      gain                 : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
      oqpsk                : IN STD_LOGIC;
      alpha                : IN STD_LOGIC             
   );
end component;

-- Signals for the DUT
signal reset                : std_logic;
signal refclk_p             : std_logic := '0';
signal refclk_n             : std_logic := '1';
signal glblclk_p            : std_logic := '0';
signal glblclk_n            : std_logic := '1';
signal drpclk               : std_logic := '0';

signal txn                  : std_logic_vector(pLanes-1 downto 0);
signal txp                  : std_logic_vector(pLanes-1 downto 0);

signal tx_decoded_data      : std_logic_vector((pLanes*8)-1 downto 0);
signal tx_decoded_data_d    : std_logic_vector((pLanes*8)-1 downto 0);
signal tx_decoded_data_dd   : std_logic_vector((pLanes*8)-1 downto 0);
signal tx_decoded_data_ddd  : std_logic_vector((pLanes*8)-1 downto 0);
signal tx_decoded_is_k      : std_logic_vector(pLanes-1 downto 0);
signal tx_decoded_is_k_d    : std_logic_vector(pLanes-1 downto 0);
signal bc_ok                : std_logic;
signal tx_sync              : std_logic;

-- Lane 0
signal tx_sysref_req        : std_logic;
signal tx_bitclock          : std_logic;
signal counter              : unsigned(2 downto 0);
signal tx_octet_clock       : std_logic;
signal all_lanes_synced     : std_logic;
signal tx_sysref            : std_logic;
signal tx_aclk              : std_logic;

-- Pulse shaper
signal din_0                : std_logic_vector(2 downto 0) := (others=>'0');
signal din_0_empty_n        : STD_LOGIC := '1';
signal din_0_read           : STD_LOGIC;

-- DIN 1 input FIFO interface
signal din_1                : std_logic_vector(2 downto 0) := (others=>'0');
signal din_1_empty_n        : STD_LOGIC := '1';
signal din_1_read           : STD_LOGIC;

-- Run-time controls      
signal dphi                 : STD_LOGIC_VECTOR(31 DOWNTO 0) := std_logic_vector( to_signed( integer(real(ROUND((2.0**26) * pDPHI))),32));
signal gain                 : STD_LOGIC_VECTOR(7 DOWNTO 0) := std_logic_vector( to_unsigned(integer(real(FLOOR((2.0**8) * pGAIN))),8));
signal oqpsk                : STD_LOGIC := pOQPSK;
signal alpha                : STD_LOGIC := pALPHA;
      
-- AXI-Lite interface. Initialisation is weak pull-up/pull-down so that the inout
-- passing of the structure through the axi_write() and axi_read() procedures resolves.
signal axi : axi_t;

-- Input test vector arrays; one for each input stream din0 and din1. Filled from a file.
-- din0 holds even samples, din1 holds odd samples.
type din_t is array(0 to MAX_NUM_IP_SAMPLES-1) of unsigned(2 downto 0);
signal din0 : din_t;
signal din1 : din_t;

-- Testbench control signals
signal axiReset_done    : std_logic := '0';
signal simtimeout_count : integer := pSimtimeout_count;
signal dinCount         : natural := 0;
signal stimFileEnd      : boolean := false;   -- Indicates end of stimulus file, to signal to start checking
signal sampleI : std_logic_vector(15 downto 0);
signal sampleQ : std_logic_vector(15 downto 0);


component GLBL_VHD is
  generic (
    InstancePath : string := "*";
    ROC_WIDTH : integer := 100000;
    TOC_WIDTH : integer := 0
    );
end component;

begin

  -----------------------------------------------------------------------------
  -- Connect the Design Under Test
  -----------------------------------------------------------------------------
  i_dut : xtx_top
   port map
   (
      refclk_p                => refclk_p,
      refclk_n                => refclk_n,
      drpclk                  => drpclk,
      reset                   => reset,
      txp                     => txp,
      txn                     => txn,
      tx_sysref               => tx_sysref,
      tx_sysref_req           => tx_sysref_req,
      tx_sync                 => tx_sync,
      axi_aclk                => axi.aclk,
      axi_aresetn             => axi.aresetn,
      axi_awaddr              => axi.awaddr,
      axi_awvalid             => axi.awvalid,
      axi_awready             => axi.awready,
      axi_wdata               => axi.wdata,
      axi_wstrb               => axi.wstrb,
      axi_wvalid              => axi.wvalid,
      axi_wready              => axi.wready,
      axi_bresp               => axi.bresp,
      axi_bvalid              => axi.bvalid,
      axi_bready              => axi.bready,
      axi_araddr              => axi.araddr,
      axi_arvalid             => axi.arvalid,
      axi_arready             => axi.arready,
      axi_rdata               => axi.rdata,
      axi_rresp               => axi.rresp,
      axi_rvalid              => axi.rvalid,
      axi_rready              => axi.rready,     
      din_0                   => din_0,
      din_0_empty_n           => din_0_empty_n,
      din_0_read              => din_0_read,
      din_1                   => din_1,
      din_1_empty_n           => din_1_empty_n,
      din_1_read              => din_1_read,
      dphi                    => dphi,
      gain                    => gain,
      oqpsk                   => oqpsk,
      alpha                   => alpha      
  ); 
  
  tx_aclk <= glblclk_p;
     
  --i_glbl_vhd : GLBL_VHD;
  
   -- Generate the 125 MHz GTHE3 refclk
   p_refclk : process
   begin
      refclk_p <= '0';
      refclk_n <= '1';
      loop
         refclk_p <= '0';
         refclk_n <= '1';
         wait for REF_CLK_PERIOD/2;
         refclk_p <= '1';
         refclk_n <= '0';
         wait for REF_CLK_PERIOD/2;
      end loop;
   end process p_refclk;

   -- Generate the 125.0MHz Device Clock
   p_glblclk : process
   begin
      glblclk_p <= '0';
      glblclk_n <= '1';
      loop
         glblclk_p <= '0';
         glblclk_n <= '1';
         wait for GLBL_CLK_PERIOD/2;
         glblclk_p <= '1';
         glblclk_n <= '0';
         wait for GLBL_CLK_PERIOD/2;
      end loop;
   end process p_glblclk;
     
   -- Generate the 125MHz DRP Clock
   p_drpclk : process
   begin
         drpclk <= '0';
      loop
         drpclk <= '0';
         wait for DRP_CLK_PERIOD/2;
         drpclk <= '1';
         wait for DRP_CLK_PERIOD/2;
      end loop;
   end process p_drpclk;

   -- Generate the 62.5MHz CPU/AXI clk
   p_axiclk : process
   begin
      axi.aclk <= '0';
      loop
         axi.aclk <= '1';
         wait for AXI_CLK_PERIOD/2;
         axi.aclk <= '0';
         wait for AXI_CLK_PERIOD/2;
      end loop;
   end process p_axiclk;
  
  -- The following generates the bitclock for sampling the Tx data
  -- streams. It uses the lane0 transmit signal, waits a time
  -- to allow the stream to stabilise, then centres the tx_bitclock
  -- transition in the centre of the transmit eye. This signal is used
  -- as a sampling point.
   p_tx_bitclock : process
   begin
      tx_bitclock <= '0';
      wait until falling_edge(reset);
      wait until rising_edge(tx_sysref_req);
      wait for GLBL_CLK_PERIOD/2;
      wait until rising_edge(txp(0));  -- sample txp(0) to get an edge
      wait for BIT_CLK_PERIOD/2;
      loop
         wait for BIT_CLK_PERIOD/2;
         tx_bitclock <= not tx_bitclock;
      end loop;      
   end process p_tx_bitclock;
  
   -- The following generates the octet clock for sampling the Tx data stream
   -- octets. It is divided /10 from tx_bitclock and uses the all lanes synced
   -- signal to determine the start and the alignment of the valid data
   p_octet_clk : process(tx_bitclock)
   begin
      if rising_edge(tx_bitclock) then
         if ( all_lanes_synced /= '1') then
            counter <= (others=>'0');
            tx_octet_clock <= '0';
         else
            if ( counter = "100" ) then
               tx_octet_clock <=  not tx_octet_clock;
               counter <= (others=>'0');         
            else
               counter <= counter + 1;
            end if;
         end if;
      end if;
   end process p_octet_clk;
  
   -- SYSREF Generation
   -- This generate a periodic SYSREF with period = 4 Multiframes.
   p_sysref_gen : process
   variable sysref_count : unsigned(5 downto 0);
   variable sysref : std_logic;
   begin
      wait until rising_edge(tx_aclk);
      if (tx_sysref_req = '0') then
         sysref_count := (others=>'0');
         sysref       := '0';
         tx_sysref    <= '0';
      else
         sysref_count := sysref_count + 1;
         if (sysref_count = "111111") then
            sysref := '1';
         else
            sysref := '0';            
         end if;
         tx_sysref <= sysref after tSU;
      end if;
  end process p_sysref_gen;
  
  
  -- This process is a 3-tap shift register that stores the previous 2 octets
  -- of decoded data. If a character replacement occurs, we substitute the
  -- comma character for the corresponding octet in tx_decoded_data_ddd.
  p_tx_decoded_data : process(tx_octet_clock)
  begin
    if rising_edge(tx_octet_clock) then        
        tx_decoded_data_ddd <= tx_decoded_data_dd;
        tx_decoded_data_dd <= tx_decoded_data_d;
        tx_decoded_data_d <= tx_decoded_data;
        tx_decoded_is_k_d <= tx_decoded_is_k;
    end if;
  end process p_tx_decoded_data;
  
  -- The following code monitors each lane and detects initial sync. It then aligns the lanes
  -- before outputting data.
   p_decode_tx : process
      type code_buffer_t is array (pLanes-1 downto 0) of std_logic_vector(0 to 9);
      type decoded_data_t is array (pLanes-1 downto 0) of std_logic_vector(7 downto 0);
      type data_align_buf_t is array(pAlign_buf_size-1 downto 0) of std_logic_vector(7 downto 0);
      type is_k_var_align_buf_t is array(pAlign_buf_size-1 downto 0) of std_logic;
      type data_align_buf_tt is array (pLanes-1 downto 0) of data_align_buf_t;
      type is_k_var_align_buf_tt is array(pLanes-1 downto 0) of is_k_var_align_buf_t;
      type is_k_var_t is array(pLanes-1 downto 0) of std_logic;
      type initial_sync_t is array(pLanes-1 downto 0) of std_logic;
      type bit_count_t is array(pLanes-1 downto 0) of integer;
      type comma_count_t is array(pLanes-1 downto 0) of integer;
      type align_count_t is array(pLanes-1 downto 0) of integer;
      type ila_sync_t is array(pLanes-1 downto 0) of std_logic;
      type bc_ok_i_t is array(pLanes-1 downto 0) of std_logic;
      type ila_seen_t is array(pLanes-1 downto 0) of std_logic;
      
      variable code_buffer : code_buffer_t;
      variable decoded_data : decoded_data_t;
      variable data_align_buf : data_align_buf_tt;
      variable is_k_var_align_buf : is_k_var_align_buf_tt;
      variable is_k_var : is_k_var_t;
      variable initial_sync : initial_sync_t;
      variable bit_count : bit_count_t;
      variable comma_count : comma_count_t;
      variable align_count : align_count_t;
      variable ila_sync : ila_sync_t;
      variable bc_ok_i : bc_ok_i_t;
      variable ila_seen : ila_seen_t;
   
   begin    
      all_lanes_synced <= '0';

      for i in 0 to pLanes-1 loop    
         initial_sync(i) := '0';
         bit_count(i)    := 0;
         comma_count(i)  := 0;
         align_count(i)  := 0;
         ila_seen(i)     := '0';
      end loop;
      
      -- endless loop, sync'd to rising edge of tx_bitclock
      loop
         wait until rising_edge(tx_bitclock);

         -- process each lane in turn.
         for i in 0 to pLanes-1 loop
            code_buffer(i) := code_buffer(i)(1 to 9) & txp(i);
            -- comma detection
            if (is_comma(code_buffer(i)(0 to 9))) then
               if (comma_count(i) < 20) or (axiReset_done = '0') then          
                  comma_count(i) := comma_count(i) + 1;
                  initial_sync(i) := '0';
                  bc_ok_i(i) := '0';
               else
                  initial_sync(i) := '1';
                  bc_ok_i(i) := '1';    --BC seen
               end if;
            
               if (initial_sync(i)='0') then
                  bit_count(i) := 0;
               end if;
            end if;
            
            if (bit_count(i) = 0 and initial_sync(i)='1') then
               decode_8b10b(
               code_buffer(i)(0 to 9),
               decoded_data(i)(7 downto 0),
               is_k_var(i));
            end if;

            if (initial_sync(i)='1') then
               bit_count(i) := bit_count(i) + 1;
               if (bit_count(i) = 10) then
                  bit_count(i) := 0;
               end if;
            end if;

            --Waiting for ILAs
            if (ila_seen(i) = '0') then
               if (is_ila(code_buffer(i)(0 to 9))) then          
                  ila_sync(i) := '1';
                  ila_seen(i) := '1';
               elsif (axiReset_done = '0') then
                  ila_sync(i) := '0';
                  ila_seen(i) := '0';        
               else
                  ila_sync(i) := '0';
               end if;
            end if;

            if (ila_sync(i) /= '1') then
               align_count(i)  := 0;        
            elsif (all_lanes_synced /= '1') then           
               --increment the alignment counter
               align_count(i) := align_count(i) + 1;        
            else        
               --All lanes have synced
               --So now do a bitwise assignment to the output data word
               -- Save the current tx_decoded_data word for character replacement
               -- if a comma character is sent.
               for j in 0 to 7 loop
                  tx_decoded_data((i*8)+j) <= data_align_buf(i)(align_count(i))(j);
               end loop;
               tx_decoded_is_k(i) <= is_k_var_align_buf(i)(align_count(i));
            end if;

            --Buffer data and is_k into alignment shift register
            is_k_var_align_buf(i)(0) := is_k_var(i);
            --must do a bitwise copy
            for j in 0 to 7 loop
               data_align_buf(i)(0)(j) := decoded_data(i)(j);
            end loop;

            for j in 1 to pAlign_buf_size-1 loop
               is_k_var_align_buf(i)(j) := is_k_var_align_buf(i)(j-1);
               --must do a bitwise copy
               for k in 0 to 7 loop
                  data_align_buf(i)(j)(k) := data_align_buf(i)(j-1)(k);
               end loop;
            end loop;
         end loop;  -- for i

         --This will notify that BCs have been on all lanes
         --and can now assert SYNC
         bc_ok <= '1';
         for i in 0 to pLanes-1 loop
            if (bc_ok_i(i) /= '1') then
               bc_ok <= '0';
            end if;
         end loop;
         
         --Check if all lanes are synced yet
         all_lanes_synced <= '1';
         for i in 0 to pLanes-1 loop
            if (ila_sync(i) /= '1') then
               all_lanes_synced <= '0';
            end if; 
         end loop; -- for loop end
         
      end loop;  -- endless loop
   end process p_decode_tx;
   
   
  -- Read the DIN input stimulus file and split into two streams: din0 handles even index, din1 handles odd index
  p_din_file_read : process  
  file infile : text is in (PATH & DIN_FILENAME);
  variable buf : line;
  variable tmp : integer;
  variable ptr : natural := 0;
  variable myDinCount : natural := 0;
  begin
    while (ptr < MAX_NUM_IP_SAMPLES) and (not endfile(infile)) loop
        readline(infile, buf);            
        read(buf,tmp);
        myDinCount := myDinCount + 1;
        din0(ptr) <= to_unsigned(tmp, din0(0)'length);        
        readline(infile, buf);  
        read(buf,tmp);
        myDinCount := myDinCount + 1;
        din1(ptr) <= to_unsigned(tmp, din1(0)'length);          
        ptr := ptr + 1;                     
    end loop;
    report "Loaded input samples";
    file_close(infile);
    dinCount <= myDinCount;
    wait;
  end process p_din_file_read;

-- Process to feed samples to the din_0 input stream
p_din : process(refclk_p)
variable ptr0 : natural := 0;
variable ptr1 : natural := 0;
variable myDinCount : natural := 0;
begin
    if rising_edge(refclk_p) then
        din_0 <= std_logic_vector(din0(ptr0));
        din_1 <= std_logic_vector(din1(ptr1));
        if din_0_read='1' then
            if ptr0 < MAX_NUM_IP_SAMPLES then 
                ptr0 := ptr0 + 1;
                myDinCount := myDinCount + 1;
            end if;
        end if;        
        if din_1_read='1' then
            if ptr1 < MAX_NUM_IP_SAMPLES then 
                ptr1 := ptr1 + 1;
                myDinCount := myDinCount + 1;
            end if;
        end if;
        if myDinCount = dinCount then
            stimFileEnd <= true;
        end if;
     end if;    
end process p_din;

     
-- This is the main transmitter stimulus task
   p_tx_stimulus : process      
      type sample_t is array (pLanes-1 downto 0) of std_logic_vector(15 downto 0);
      type frame_t is array (pLanes-1 downto 0) of std_logic_vector(7 downto 0);      
      variable ptr : integer := 0;      
      variable sample : sample_t;
      variable frame : frame_t;
      file myRefFile : text;
      file myDoutFile : text;
      variable myDinCount : natural := 0;
      variable fstatus : FILE_OPEN_STATUS;
      variable readOK : boolean;
      variable buf : line;
      variable fileI : real;     -- reference I sample value (real number)
      variable fileQ : real;     -- reference Q sample value (real number)
      variable ch : character;   -- used for dummy read of ","
      variable refI : integer;   -- Reference (golden) I sample      
      variable refQ : integer;   -- Reference (golden) Q sample
      variable intI : integer;
      variable intQ : integer;
         
   begin
   
    
      -- Open the reference data CSV file
      file_open(fstatus, myRefFile, PATH & REF_FILENAME, READ_MODE);
      report "Golden reference input file status: " & FILE_OPEN_STATUS'image(fstatus);
      assert fstatus = OPEN_OK report "Failed to open golden reference file." severity failure;
      
      -- Open the RTL output results file
      file_open(fstatus, myDoutFile, PATH & DOUT_FILENAME, WRITE_MODE);
      report "RTL output results file status: " & FILE_OPEN_STATUS'image(fstatus);
      assert fstatus = OPEN_OK report "Failed to open RTL output results file." severity failure;
      
      tx_sync <= '0';

      while ( axiReset_done /= '1' ) loop --Wait for AXI to complete configuration set-up
         wait until rising_edge(tx_aclk);
      end loop;
      report "** GT Reset Done";
      wait for 100000 ps;
      
      report "** Wait for K28.5";
      while (bc_ok /= '1') loop
        wait until rising_edge(tx_bitclock);
      end loop;
      wait for 100000 ps;
      
      report "** Assert Sync";
      tx_sync <= '1';

      while ((tx_decoded_data /= repeat(pLanes, x"1C") or
             (tx_decoded_is_k /= repeat(pLanes,'1')))) loop
         wait until rising_edge(tx_bitclock);
      end loop;
      report "** ILA Start";
      while (tx_decoded_is_k(pLanes-1 downto 0) /= repeat(pLanes, '0')) loop
         wait until rising_edge(tx_bitclock);
      end loop;

      while ((tx_decoded_data /= repeat(pLanes, x"7C")) or
             (tx_decoded_is_k /= repeat(pLanes, '1'))) loop
         wait until rising_edge(tx_bitclock);
      end loop;
      report "*** End of Multi Frame 1";
      while (tx_decoded_is_k(pLanes-1 downto 0) /= repeat(pLanes, '0')) loop
         wait until rising_edge(tx_bitclock);
      end loop;

      while ((tx_decoded_data /= repeat(pLanes, x"7C")) or
             (tx_decoded_is_k /= repeat(pLanes, '1'))) loop
         wait until rising_edge(tx_bitclock);
      end loop;
      report "*** End of Multi Frame 2";
      while (tx_decoded_is_k(pLanes-1 downto 0) /= repeat(pLanes, '0')) loop
         wait until rising_edge(tx_bitclock);
      end loop;

      while ((tx_decoded_data /= repeat(pLanes, x"7C")) or
             (tx_decoded_is_k /= repeat(pLanes,'1'))) loop
         wait until rising_edge(tx_bitclock);
      end loop;
      report "*** End of Multi Frame 3";
      while (tx_decoded_is_k(pLanes-1 downto 0) /= repeat(pLanes, '0')) loop
         wait until rising_edge(tx_bitclock);
      end loop;

      while ((tx_decoded_data /= repeat(pLanes, x"7C")) or
             (tx_decoded_is_k /= repeat(pLanes, '1'))) loop
         wait until rising_edge(tx_bitclock);
      end loop;
      report "*** End of Multi Frame 4";
      report "** ILA Complete";
                 
      while not endfile(myRefFile) loop
      
          -- Skip K characters on all 4 lanes
          while tx_decoded_is_k(pLanes-1 downto 0) = repeat(pLanes, '1') loop
            wait until rising_edge(tx_octet_clock);
          end loop;
          
         --Construct frames for each lane. for sample n.
         --   lane 0: In(15:8)
         --   lane 1: In(7:0)
         --   lane 2: Qn(15:8)
         --   lane 3: Qn(7:0)
         --One octet per frame
         -- This is the format required by the TI DAC38RF84 (LMFSHd = 42111)
         wait until rising_edge(tx_octet_clock);
         
         -- If a comma character is present on a lane,
         -- a character replacement has occured. Use the octet from the previous frame.
         for i in 0 to pLanes-1 loop
            if tx_decoded_is_k_d(i)='0' then                                
                -- Do a bitwise copy of the octet
                for k in 0 to 7 loop
                   frame(i)(k) := tx_decoded_data_d((i*8)+k);
                end loop;               
            else
                for k in 0 to 7 loop
                    frame(i)(k) := tx_decoded_data_ddd((i*8)+k);
                 end loop;                                    
            end if;
         end loop;
         
         -- retrieve I/Q samples.          
        sampleI(15 downto 8) <= frame(0); -- Lane 0
        sampleI(7 downto 0)  <= frame(1); -- Lane 1
        sampleQ(15 downto 8) <= frame(2); -- Lane 2
        sampleQ(7 downto 0)  <= frame(3); -- Lane 3
                    
        -- Read the reference data file                        
        readline(myRefFile,buf);
        read(buf, fileI);
        read(buf, ch);
        read(buf,fileQ);
        refI := integer(round(fileI * 16384.0));  -- Convert from real to 2.14 format
        refQ := integer(round(fileQ * 16384.0));  -- integers.
        
        -- Write the output file
        intI := to_integer(signed(sampleI));
        intQ := to_integer(signed(sampleQ));
        write(buf, intI);
        write(buf, ',');
        write(buf, intQ);
        writeline(myDoutFile,buf);
        
        -- Do a check on match between reference and simulation output
        assert refI = to_integer(signed(sampleI)) report "I sample mis-match: got: " & integer'image(intI) &
                                     " should be: " & integer'image(refI) severity failure;
        assert refQ = to_integer(signed(sampleQ)) report "Q sample mis-match: got: " & integer'image(intQ) &
                                     " should be: " & integer'image(refQ) severity failure;                                 
         
      end loop;
      file_close(myDoutFile);
      file_close(myRefFile);
      
      report "** Test Passed";
      report "** Test completed successfully";
      assert false report "Simulation Finished" severity failure;
      wait;
    end process p_tx_stimulus; -- p_tx_stimulus  


-- Program the link configuration registers
   p_axi_stimulus : process
      variable register_val : std_logic_vector(31 downto 0);
   begin
      -- Set the default for the inputs from the DUT to be 'Z' so that they resolve in favour
      -- of the DUT's driver.
      axi.awready <= 'Z';
      axi.wready  <= 'Z';
      axi.bresp   <= (others=>'Z');
      axi.bvalid  <= 'Z';
      axi.arready <= 'Z';
      axi.rdata   <= (others=>'Z');
      axi.rresp   <= (others=>'Z');
      axi.rvalid  <= 'Z';
      axi.aclk    <= 'Z';
      
      -- Set some initial values for bus output
      axi.awaddr <= (others=>'0');
      axi.awvalid <= '0';
      axi.wdata <= (others=>'0');
      axi.wvalid <= '0';
      axi.bready <= '0';
      axi.araddr <= (others=>'0');
      axi.arvalid <= '0';
      axi.rready <= '0';
      axi.wstrb <= (others=>'1');
      
      axi.aresetn <= '1';
      reset <= '0';
      wait for 1000 ps;
      
      -- Generate the core reset.
      report "Resetting the core...";
      reset <= '1';
      axi.aresetn <= '0';
      wait for 400000 ps;
      axi.aresetn <= '1';
      reset <= '0';

      -- 0x000: Read Version
      axi_read(0, register_val, axi);
      report "Version = Major " & integer'image(to_integer(unsigned(register_val(31 downto 24)))) &
              " Minor "         & integer'image(to_integer(unsigned(register_val(23 downto 16)))) & 
              " Rev "           & integer'image(to_integer(unsigned(register_val(15 downto 8))));

      -- 0x004: Reset later once configured

      -- 0x008: Support ILA
      axi_write(2,x"00000001", axi);

      -- 0x00C: Scrambling dissabled
      axi_write(3,x"00000000", axi);

      -- 0x010: Sysref once
      axi_write(4,x"00000001", axi);

      -- 0x014: Multiframes in ILA = 4
      axi_write(5,x"00000003", axi);

      -- 0x018: Test mode = Normal operation
      axi_write(6,x"00000000", axi);

      -- 0x020: Octets per Frame F=2
      axi_write(8,x"00000001", axi);

      -- 0x024: Frames per Multiframe K=32
      axi_write(9,x"0000001F", axi);

      -- 0x028: Lanes in use
      axi_write(10,x"0000000F", axi);

      -- 0x02C: Device subclass 1
      axi_write(11,x"00000001", axi);

      -- 0x030: Rx only register

      -- 0x034: Rx only register

      -- 0x80C: L, DID, BID
      axi_write(515,"000" & pL & "000000000000" & pBID & pDID, axi );

      -- 0x810: CS, N', N, M
      axi_write(516, "000000" & pCS & "000" & pNt & "000" & pN & pM, axi);

      -- 0x814: CF, HD, S, SCR
      axi_write(517, "000" & pCF & "0000000" & pHD & "000" & p_S & "0000000" & pSCR, axi );

      -- 0x818: RES1, RES2 checksum generated automatically
      axi_write(518, x"0000" & pRES2 & pRES1, axi);

      -- Link configuration has changed so reset the interface
      -- 0x04: Write reset
      axi_write(1,x"00000001", axi);
      -- Now poll register until reset has cleared
      register_val := x"00000001";
      while ( register_val(0) /= '0' ) loop
        wait for tSU;    --wait for a time then read
        axi_read(1,register_val, axi);
      end loop;
      report "AXI Configuration and Reset complete....";

      axiReset_done <= '1';  --Signal to notify that AXI has been configured
      wait;
    end process p_axi_stimulus;
   
   -- Check for runaway simulation
   p_sim_timeout : process
   begin
      simtimeout_count <= pSimtimeout_count;
      while( simtimeout_count > 0 ) loop      
         wait for simtimeout;
         simtimeout_count <= simtimeout_count - 1;
      end loop;
      report "** SIMULATION TIMEOUT";
      assert false severity failure;
   end process p_sim_timeout; -- p_sim_timeout
  
end bench;