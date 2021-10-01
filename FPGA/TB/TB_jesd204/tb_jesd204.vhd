----------------------------------------------------------------------------------------------------
-- Copyright (C) 2018 Phiphase Limited
--
-- VHDL version of Xilinx Vivado JESD204 testbench
--
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.pkg_demo_tb.all;

entity demo_tb is
end demo_tb;

architecture bench of demo_tb is

constant NUM_RX_SAMPLES    : natural := 1000;
constant pSimtimeout_count : integer := 40;   
constant simtimeout        : time := 100000000 ps;
constant REF_CLK_PERIOD    : time := 8000 ps;    -- 125 MHz
constant GLBL_CLK_PERIOD   : time := 8000 ps;    -- 125 MHz
constant DRP_CLK_PERIOD    : time := 8000 ps;    -- 125 MHz
constant AXI_CLK_PERIOD    : time := 8000 ps;    -- 125 MHz
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

-- DUT Declaration
component jesd204_0_example_design
  -- Comment out the generic for post-implementation simulation
   --generic (
   --   pLanes : integer := 4
   --);
   port(
      refclk0p               : in std_logic;
      refclk0n               : in std_logic;

      glblclkp               : in std_logic;
      glblclkn               : in std_logic;

      drpclk                 : in std_logic;

      tx_reset               : in std_logic;
      tx_start_of_frame      : out std_logic_vector(3 downto 0);
      tx_start_of_multiframe : out std_logic_vector(3 downto 0);

      txp                    : out std_logic_vector(pLanes-1 downto 0);
      txn                    : out std_logic_vector(pLanes-1 downto 0);

      s_axi_aclk             : in std_logic;
      s_axi_aresetn          : in std_logic;
      s_axi_awaddr           : in std_logic_vector(11 downto 0);
      s_axi_awvalid          : in std_logic;
      s_axi_awready          : out std_logic;
      s_axi_wdata            : in std_logic_vector(31 downto 0);
      s_axi_wstrb            : in std_logic_vector(3 downto 0);
      s_axi_wvalid           : in std_logic;
      s_axi_wready           : out std_logic;
      s_axi_bresp            : out std_logic_vector(1 downto 0);
      s_axi_bvalid           : out std_logic;
      s_axi_bready           : in std_logic;
      s_axi_araddr           : in std_logic_vector(11 downto 0);
      s_axi_arvalid          : in std_logic;
      s_axi_arready          : out std_logic;
      s_axi_rdata            : out std_logic_vector(31 downto 0);
      s_axi_rresp            : out std_logic_vector(1 downto 0);
      s_axi_rvalid           : out std_logic;
      s_axi_rready           : in std_logic;
      -- Tx AXI common signals
      tx_aresetn             : out std_logic;
      tx_sysref              : in std_logic;
      tx_sync                : in std_logic
  );
end component;

-- Signals for the DUT
signal reset            : std_logic;
signal refclk0p         : std_logic := '0';
signal refclk0n         : std_logic := '1';
signal glblclkp         : std_logic := '0';
signal glblclkn         : std_logic := '1';
signal drpclk           : std_logic := '0';

signal txn              : std_logic_vector(pLanes-1 downto 0);
signal txp              : std_logic_vector(pLanes-1 downto 0);
signal tx_decoded_data  : std_logic_vector((pLanes*8)-1 downto 0);
signal tx_decoded_is_k  : std_logic_vector(pLanes-1 downto 0);
signal bc_ok            : std_logic;
signal tx_sync          : std_logic;

-- Lane 0
signal tx_aresetn       : std_logic;
signal tx_bitclock      : std_logic;
signal counter          : unsigned(2 downto 0);
signal tx_octet_clock   : std_logic;
signal all_lanes_synced : std_logic;
signal tx_sysref        : std_logic;
signal tx_aclk          : std_logic;

      
-- AXI-Lite interface. Initialisation is weak pull-up/pull-down so that the inout
-- passing of the structure through the axi_write() and axi_read() procedures resolves.
signal s_axi : axi_t;

signal axiReset_done    : std_logic := '0';
signal simtimeout_count : integer := pSimtimeout_count;

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
  i_dut : jesd204_0_example_design
  -- Comment out the generic for post-implementation simulation
  --generic map ( pLanes => pLanes)
  port map
  (
   refclk0p               => refclk0p,
   refclk0n               => refclk0n,
   glblclkp               => glblclkp,
   glblclkn               => glblclkn,
   drpclk                 => drpclk,
   tx_reset               => reset,
   tx_start_of_frame      => open,
   tx_start_of_multiframe => open,
   txp                    => txp,
   txn                    => txn,
   s_axi_aclk             => s_axi.aclk,
   s_axi_aresetn          => s_axi.aresetn,
   s_axi_awaddr           => s_axi.awaddr,
   s_axi_awvalid          => s_axi.awvalid,
   s_axi_awready          => s_axi.awready,
   s_axi_wdata            => s_axi.wdata,
   s_axi_wstrb            => s_axi.wstrb,
   s_axi_wvalid           => s_axi.wvalid,
   s_axi_wready           => s_axi.wready,
   s_axi_bresp            => s_axi.bresp,
   s_axi_bvalid           => s_axi.bvalid,
   s_axi_bready           => s_axi.bready,
   s_axi_araddr           => s_axi.araddr,
   s_axi_arvalid          => s_axi.arvalid,
   s_axi_arready          => s_axi.arready,
   s_axi_rdata            => s_axi.rdata,
   s_axi_rresp            => s_axi.rresp,
   s_axi_rvalid           => s_axi.rvalid,
   s_axi_rready           => s_axi.rready,

   --Tx AXI common signals
   tx_aresetn             => tx_aresetn, --tx_aresetn,
   tx_sysref              => tx_sysref,
   tx_sync                => tx_sync
  ); 
  
  tx_aclk <= glblclkp;
  
   
  i_glbl_vhd : GLBL_VHD;
  
   -- Generate the 250 MHz GTHE3 refclk
   p_refclk : process
   begin
      refclk0p <= '0';
      refclk0n <= '1';
      loop
         refclk0p <= '0';
         refclk0n <= '1';
         wait for REF_CLK_PERIOD/2;
         refclk0p <= '1';
         refclk0n <= '0';
         wait for REF_CLK_PERIOD/2;
      end loop;
   end process p_refclk;

   -- Generate the 200.0MHz Device Clock
   p_glblclk : process
   begin
      glblclkp <= '0';
      glblclkn <= '1';
      loop
         glblclkp <= '0';
         glblclkn <= '1';
         wait for GLBL_CLK_PERIOD/2;
         glblclkp <= '1';
         glblclkn <= '0';
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

   -- Generate the 100.0MHz CPU/AXI clk
   p_axiclk : process
   begin
      s_axi.aclk <= '0';
      loop
         s_axi.aclk <= '1';
         wait for AXI_CLK_PERIOD/2;
         s_axi.aclk <= '0';
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
      wait until rising_edge(tx_aresetn);
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
      if (tx_aresetn = '0') then
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
   
   
  
  
-- This is the main transmitter stimulus task
   p_tx_stimulus : process
      type control_t is array (pLanes-1 downto 0) of std_logic_vector(1 downto 0);
      type sample_t is array (pLanes-1 downto 0) of std_logic_vector(13 downto 0);
      type frame_t is array (pLanes-1 downto 0) of std_logic_vector(15 downto 0);
      variable pointer : unsigned(5 downto 0);
      variable counter: unsigned(1 downto 0);
      variable control : control_t;
      variable sample : sample_t;
      variable frame : frame_t;
   begin
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

      while (tx_decoded_is_k(pLanes-1 downto 0) /= repeat(pLanes, '0')) loop
         wait until rising_edge(tx_bitclock);
      end loop;

      --Now check that the recieved data matches the expected data
      for j in 0 to NUM_RX_SAMPLES-1 loop
         --Construct frames for each lane
         --Two octets per frame (pF = 2)
         for l in 0 to pF-1 loop        
            wait until rising_edge(tx_octet_clock);
            for i in 0 to pLanes-1 loop
               -- Do a bitwise copy of the octet
               for k in 0 to 7 loop
                  frame(i)((l*8)+k) := tx_decoded_data((i*8)+k);
               end loop;
            end loop;
         end loop;

         --test all is as expected
         for i in 0 to pLanes-1 loop
            pointer := to_unsigned(j + (i*2), pointer'length);
            counter := to_unsigned(j + (i*2), counter'length);

            -- de-map control word and sample from transmitted frame.
            control(i) := frame(i)(15 downto 14);
            sample(i)  := frame(i)(13 downto 0);

            if ( (unsigned(control(i)) /= counter ) or
                 ( signed(sample(i)) /= sine_lut64_14_bit(to_integer(pointer))) ) then
               assert false report "** Error in Transmitted data." severity failure;
            end if;            
         end loop; --for i
      end loop;  --for j

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
      s_axi.awready <= 'Z';
      s_axi.wready  <= 'Z';
      s_axi.bresp   <= (others=>'Z');
      s_axi.bvalid  <= 'Z';
      s_axi.arready <= 'Z';
      s_axi.rdata   <= (others=>'Z');
      s_axi.rresp   <= (others=>'Z');
      s_axi.rvalid  <= 'Z';
      s_axi.aclk    <= 'Z';
      
      -- Set some initial values for bus output
      s_axi.awaddr <= (others=>'0');
      s_axi.awvalid <= '0';
      s_axi.wdata <= (others=>'0');
      s_axi.wvalid <= '0';
      s_axi.bready <= '0';
      s_axi.araddr <= (others=>'0');
      s_axi.arvalid <= '0';
      s_axi.rready <= '0';
      s_axi.wstrb <= (others=>'1');
      
      s_axi.aresetn <= '1';
      reset <= '0';
      wait for 1000 ps;
      
      -- Generate the core reset.
      report "Resetting the core...";
      reset <= '1';
      s_axi.aresetn <= '0';
      wait for 400000 ps;
      s_axi.aresetn <= '1';
      reset <= '0';

      -- 0x000: Read Version
      axi_read(0, register_val, s_axi);
      report "Version = Major " & integer'image(to_integer(unsigned(register_val(31 downto 24)))) &
              " Minor "         & integer'image(to_integer(unsigned(register_val(23 downto 16)))) & 
              " Rev "           & integer'image(to_integer(unsigned(register_val(15 downto 8))));

      -- 0x004: Reset later once configured

      -- 0x008: Support ILA
      axi_write(2,x"00000001", s_axi);

      -- 0x00C: Scrambling dissabled
      axi_write(3,x"00000000", s_axi);

      -- 0x010: Sysref once
      axi_write(4,x"00000001", s_axi);

      -- 0x014: Multiframes in ILA = 4
      axi_write(5,x"00000003", s_axi);

      -- 0x018: Test mode = Normal operation
      axi_write(6,x"00000000", s_axi);

      -- 0x020: Octets per Frame F=2
      axi_write(8,x"00000001", s_axi);

      -- 0x024: Frames per Multiframe K=32
      axi_write(9,x"0000001F", s_axi);

      -- 0x028: Lanes in use
      axi_write(10,x"0000000F", s_axi);

      -- 0x02C: Device subclass 1
      axi_write(11,x"00000001", s_axi);

      -- 0x030: Rx only register

      -- 0x034: Rx only register

      -- 0x80C: L, DID, BID
      axi_write(515,"000" & pL & "000000000000" & pBID & pDID, s_axi );

      -- 0x810: CS, N', N, M
      axi_write(516, "000000" & pCS & "000" & pNt & "000" & pN & pM, s_axi);

      -- 0x814: CF, HD, S, SCR
      axi_write(517, "000" & pCF & "0000000" & pHD & "000" & p_S & "0000000" & pSCR, s_axi );

      -- 0x818: RES1, RES2 checksum generated automatically
      axi_write(518, x"0000" & pRES2 & pRES1, s_axi);

      -- Link configuration has changed so reset the interface
      -- 0x04: Write reset
      axi_write(1,x"00000001", s_axi);
      -- Now poll register until reset has cleared
      register_val := x"00000001";
      while ( register_val(0) /= '0' ) loop
        wait for tSU;    --wait for a time then read
        axi_read(1,register_val, s_axi);
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
