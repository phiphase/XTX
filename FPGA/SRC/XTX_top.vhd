----------------------------------------------------------------------------------
-- Copyright (C) 2018 Phiphase Limited
-- 
-- Create Date: 13.06.2018 16:34:22
-- Design Name: X-band Tx top level
-- Module Name: XTX_top - rtl
-- Project Name: XTX
-- Target Devices: Xilinx XCKU035
-- Tool Versions: 
-- Description: 
--     Top level entity and hierarchy for the X-band Tx
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;
library work;
use work.pkg_xtx.all;

entity XTX_top is
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
end XTX_top;

architecture rtl of XTX_top is   


-- JESD204B IP Core, 4 lanes
COMPONENT jesd204_0
  PORT (
    refclk_p               : IN STD_LOGIC;
    refclk_n               : IN STD_LOGIC;
    drpclk                 : IN STD_LOGIC;
    tx_core_clk_out        : OUT STD_LOGIC;
    s_axi_aclk             : IN STD_LOGIC;
    s_axi_aresetn          : IN STD_LOGIC;
    s_axi_awaddr           : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
    s_axi_awvalid          : IN STD_LOGIC;
    s_axi_awready          : OUT STD_LOGIC;
    s_axi_wdata            : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    s_axi_wstrb            : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
    s_axi_wvalid           : IN STD_LOGIC;
    s_axi_wready           : OUT STD_LOGIC;
    s_axi_bresp            : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    s_axi_bvalid           : OUT STD_LOGIC;
    s_axi_bready           : IN STD_LOGIC;
    s_axi_araddr           : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
    s_axi_arvalid          : IN STD_LOGIC;
    s_axi_arready          : OUT STD_LOGIC;
    s_axi_rdata            : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    s_axi_rresp            : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    s_axi_rvalid           : OUT STD_LOGIC;
    s_axi_rready           : IN STD_LOGIC;
    tx_reset               : IN STD_LOGIC;
    tx_sysref              : IN STD_LOGIC;
    tx_start_of_frame      : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
    tx_start_of_multiframe : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
    txp                    : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
    txn                    : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
    tx_aresetn             : OUT STD_LOGIC;
    tx_tdata               : IN STD_LOGIC_VECTOR(127 DOWNTO 0);
    tx_tready              : OUT STD_LOGIC;
    tx_sync                : IN STD_LOGIC
  );
END COMPONENT;

-- High speed pulse shaper 4 out, 2 in
COMPONENT pulse_shaper_top_0
  PORT (
    ap_clk            : IN STD_LOGIC;
    ap_rst_n          : IN STD_LOGIC;
    ap_start          : IN STD_LOGIC;
    ap_done           : OUT STD_LOGIC;
    ap_idle           : OUT STD_LOGIC;
    ap_ready          : OUT STD_LOGIC;
    din_0_V_V_dout    : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
    din_0_V_V_empty_n : IN STD_LOGIC;
    din_0_V_V_read    : OUT STD_LOGIC;
    din_1_V_V_dout    : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
    din_1_V_V_empty_n : IN STD_LOGIC;
    din_1_V_V_read    : OUT STD_LOGIC;
    dout_V_y_TVALID   : OUT STD_LOGIC;
    dout_V_y_TREADY   : IN STD_LOGIC;
    dout_V_y_TDATA    : OUT STD_LOGIC_VECTOR(127 DOWNTO 0);
    dphi_V            : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    gain_V            : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    oqpsk             : IN STD_LOGIC;
    alpha             : IN STD_LOGIC
  );
END COMPONENT;

signal   clk                    : STD_LOGIC;                         -- system core clock
--signal   drpclk                 : std_logic;                         -- Dynamic Reconfiguration Port clock

-- JESD204B
signal   jesd_axi               : axi_t;                             -- JESD204B Tx AXI bus
signal   tx_reset               : STD_LOGIC;                         -- Reset the JESD204B Tx
signal   tx_start_of_frame      : STD_LOGIC_VECTOR(3 DOWNTO 0);      -- JESD204B Tx diagnostics
signal   tx_start_of_multiframe : STD_LOGIC_VECTOR(3 DOWNTO 0);      -- JESD204B Tx diagnostics  
signal   tx_tready              : STD_LOGIC;                         -- JESD204B Tx ready
signal   tx_aresetn             : STD_LOGIC;                         -- JESD204B reset indication
signal   tx_tdata               : STD_LOGIC_VECTOR(127 DOWNTO 0) := (others=> '0');    -- JESD204B Tx input data

-- Pulse shaper
signal ap_rst_n           : std_logic;
signal ap_start           : std_logic;
signal ap_done            : std_logic;
signal ap_idle            : std_logic;
signal ap_ready           : std_logic;
signal tx_tvalid          : std_logic;  -- just for monitoring
signal pulse_shaper_tdata : std_logic_vector(127 downto 0);

begin

-- We use the tx_aresetn signal to request the generation of sysref (if not continuously generated)
tx_sysref_req <= tx_aresetn;

--pulse_shaper_tdata <= x"FFEEDDCCBBAA99887766554433221100";
-- Map the pulse shaper bytes into the order required for correct transmission to the DAC38RF84.
tx_tdata <= map_tdata(pulse_shaper_tdata);

-- Reset control process
p_reset : process(reset, clk)
begin
    if reset = '1' then
        ap_start <= '0';
        tx_reset <= '1';
        ap_rst_n <= '0';
    elsif rising_edge(clk) then
        tx_reset <= '0';
        ap_rst_n <= '1';
        -- Assert ap_start 1 clock cycle after ap_rst_n is cleared.
        if ap_rst_n ='1' then
            ap_start <= '1';
        end if;        
    end if;
end process p_reset;

-- Connect the JESD204B AXI to the top-level entity. This is temporary.
jesd_axi.aclk    <= axi_aclk;
jesd_axi.aresetn <= axi_aresetn;
jesd_axi.awaddr  <= axi_awaddr; 
jesd_axi.awvalid <= axi_awvalid;
axi_awready      <= jesd_axi.awready;
jesd_axi.wdata   <= axi_wdata;
jesd_axi.wstrb   <= axi_wstrb;
jesd_axi.wvalid  <= axi_wvalid;
axi_wready       <= jesd_axi.wready;
axi_bresp        <= jesd_axi.bresp;
axi_bvalid       <= jesd_axi.bvalid;
jesd_axi.bready  <= axi_bready;
jesd_axi.araddr  <= axi_araddr;
jesd_axi.arvalid <= axi_arvalid;
axi_arready      <= jesd_axi.arready;
axi_rdata        <= jesd_axi.rdata;
axi_rresp        <= jesd_axi.rresp;
axi_rvalid       <= jesd_axi.rvalid;
jesd_axi.rready  <= axi_rready;

-- Clocks
--drpclk <= clk;

i_jesd_tx : jesd204_0
   PORT MAP (
      refclk_p               => refclk_p,
      refclk_n               => refclk_n,
      drpclk                 => drpclk,
      tx_core_clk_out        => clk,
      s_axi_aclk             => jesd_axi.aclk,
      s_axi_aresetn          => jesd_axi.aresetn,
      s_axi_awaddr           => jesd_axi.awaddr,
      s_axi_awvalid          => jesd_axi.awvalid,
      s_axi_awready          => jesd_axi.awready,
      s_axi_wdata            => jesd_axi.wdata,
      s_axi_wstrb            => jesd_axi.wstrb,
      s_axi_wvalid           => jesd_axi.wvalid,
      s_axi_wready           => jesd_axi.wready,
      s_axi_bresp            => jesd_axi.bresp,
      s_axi_bvalid           => jesd_axi.bvalid,
      s_axi_bready           => jesd_axi.bready,
      s_axi_araddr           => jesd_axi.araddr,
      s_axi_arvalid          => jesd_axi.arvalid,
      s_axi_arready          => jesd_axi.arready,
      s_axi_rdata            => jesd_axi.rdata,
      s_axi_rresp            => jesd_axi.rresp,
      s_axi_rvalid           => jesd_axi.rvalid,
      s_axi_rready           => jesd_axi.rready,
      tx_reset               => tx_reset,
      tx_sysref              => tx_sysref,
      tx_start_of_frame      => tx_start_of_frame,
      tx_start_of_multiframe => tx_start_of_multiframe,
      txp                    => txp,
      txn                    => txn,
      tx_aresetn             => tx_aresetn,
      tx_tdata               => tx_tdata,
      tx_tready              => tx_tready,
      tx_sync                => tx_sync
  );

i_pulse_shaper_top : pulse_shaper_top_0
  PORT MAP (
    ap_clk            => clk,
    ap_rst_n          => ap_rst_n,
    ap_start          => ap_start,
    ap_done           => ap_done,
    ap_idle           => ap_idle,
    ap_ready          => ap_ready,
    din_0_V_V_dout    => din_0,
    din_0_V_V_empty_n => din_0_empty_n,
    din_0_V_V_read    => din_0_read,
    din_1_V_V_dout    => din_1,
    din_1_V_V_empty_n => din_1_empty_n,
    din_1_V_V_read    => din_1_read,
    dout_V_y_TVALID   => tx_tvalid,
    dout_V_y_TREADY   => tx_tready,
    dout_V_y_TDATA    => pulse_shaper_tdata,
    dphi_V => dphi,
    gain_V => gain,
    oqpsk => oqpsk,
    alpha => alpha
  );
  
end rtl;
