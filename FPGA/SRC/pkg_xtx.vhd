
------------------------------------------------------------------------------
-- Copyright (C) 2018 Phiphase Limited
-- pkg_xtx.vhd
--
-- Global constant and type definitions for the XTX FPGA
-- File : pkg_xtx.vhd
------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package pkg_xtx is
   -- JESD204B
   constant NUM_LANES : natural := 4;

  -- AXI Bus definition.
  type axi_t is record
      -- Outputs to DUT
      aclk             : std_logic;
      aresetn          : std_logic;
      awaddr           : std_logic_vector(11 downto 0);
      awvalid          : std_logic;
      awready          : std_logic;
      wdata            : std_logic_vector(31 downto 0);
      wvalid           : std_logic;
      wready           : std_logic;
      bresp            : std_logic_vector(1 downto 0);
      bvalid           : std_logic;
      bready           : std_logic;
      araddr           : std_logic_vector(11 downto 0);
      arvalid          : std_logic;
      arready          : std_logic;
      rdata            : std_logic_vector(31 downto 0);
      rresp            : std_logic_vector(1 downto 0);
      rvalid           : std_logic;            
      rready           : std_logic;
      wstrb            : std_logic_vector(3 downto 0);      
   end record;

----------------------------------------------------------------------------------------------------   
-- Function map_tdata()
--    This function maps the 128 bit packed data output from the pulse_shaper module to the 128 bit
--    tx_tdata input to the JESD204B IP core so that the samples are transmitted to the DAC in the
--    correct order.
--    The DAC38RF84, programmed as per Table 11 (data sheet) LMFSHd = 42111 requires the assignment
--    of 16 bit samples to lanes as follows:
--       Lane 0:  i0[15:8]
--       Lane 1:  i0[7:0]
--       Lane 2:  q0[15:8]
--       Lane 3:  q0[7:0]
--
--    The mapping of the JESD204B IP core's 128 bit input words to lanes is:
--       bits 127:96 on Lane 3
--       bits  95:64 on Lane 2
--       bits  63:32 on Lane 1
--       bits  31:0  on Lane 0
--     1 byte is transmitted per lane per frame so for n=0, lane 3 transmits bits 127:120,
--     lane 2 transmits bits 95:88, lane 1 transmits bits 63:56 and lane 0 transmits bits
--     31:24.  For n=1, lane 3 transmits bits 119:112, lane 2 transmits bits 87:80, lane 1
--     transmits 55:48 and lane 0 transmits 23:16. And so on for n=2,3.
--
--     In order to ensure that the byte transmission order is correct for the DAC, we need to
--     re-order the bits in the 128 bit words output from the pulse shaper into the order required
--     for transmission to the DAC. The mapping is:
--
--        din        dout
--     119:112  >   127:120            Q0[7:0]
--     103:96   >   119:112            Q1[7:0]
--     87:80 	>   111:104            Q2[7:0]
--     71:64	>   103:96             Q3[7:0]
--     127:120  >   95:88              Q0[15:8]
--     111:104  >   87:80              Q1[15:8]
--     95:88	>   79:72              Q2[15:8]
--     79:72	>   71:64              Q3[15:8]
--     55:48 	>   63:56              I0[7:0]
--     39:32 	>   55:48              I1[7:0]
--     23:16 	>   47:40              I2[7:0]
--     7:0 	    >   39:32              I3[7:0]
--     63:56 	>   31:24              I0[15:8]
--     47:40 	>   23:16              I1[15:8]
--     31:24 	>   15:8               I2[15:8]
--     15:8     >   7:0                I3[15:8]
----------------------------------------------------------------------------------------------------
function map_tdata(din : std_logic_vector(127 downto 0)) return std_logic_vector;
   
end package;

 
  
package body pkg_xtx is

   function map_tdata(din : std_logic_vector(127 downto 0)) return std_logic_vector is
   variable dout : std_logic_vector(127 downto 0);
   begin
      dout(127 downto 120) := din(119 downto 112);
      dout(119 downto 112) := din(103 downto 96);
      dout(111 downto 104) := din(87 downto 80);
      dout(103 downto 96)  := din(71 downto 64);
      dout(95 downto 88)   := din(127 downto 120);
      dout(87 downto 80)   := din(111 downto 104);
      dout(79 downto 72)   := din(95 downto 88);
      dout(71 downto 64)   := din(79 downto 72);
      dout(63 downto 56)   := din(55 downto 48);
      dout(55 downto 48)   := din(39 downto 32);
      dout(47 downto 40)   := din(23 downto 16);
      dout(39 downto 32)   := din(7 downto 0);
      dout(31 downto 24)   := din(63 downto 56);
      dout(23 downto 16)   := din(47 downto 40);
      dout(15 downto 8)    := din(31 downto 24);
      dout(7 downto 0)     := din(15 downto 8);
      return dout;
   end map_tdata;
 
end pkg_xtx;