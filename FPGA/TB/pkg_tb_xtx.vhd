
------------------------------------------------------------------------------
-- Copyright (C) 2018 Phiphase Limited
-- Translation of sin_lut_64_14bit.vh verilog include file from Xilinx
-- JESD204B test bench.
-- File : pkg_tb_xtx.vhd
------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.pkg_xtx.all;

package pkg_tb_xtx is

constant tSU               : time := 1000 ps;    -- setup/hold time for AXI lite

-- Defined in pkg_xtx
--  type axi_t is record
--      -- Outputs to DUT
--      aclk             : std_logic;
--      aresetn          : std_logic;
--      awaddr           : std_logic_vector(11 downto 0);
--      awvalid          : std_logic;
--      awready          : std_logic;
--      wdata            : std_logic_vector(31 downto 0);
--      wvalid           : std_logic;
--      wready           : std_logic;
--      bresp            : std_logic_vector(1 downto 0);
--      bvalid           : std_logic;
--      bready           : std_logic;
--      araddr           : std_logic_vector(11 downto 0);
--      arvalid          : std_logic;
--      arready          : std_logic;
--      rdata            : std_logic_vector(31 downto 0);
--      rresp            : std_logic_vector(1 downto 0);
--      rvalid           : std_logic;            
--      rready           : std_logic;
--      wstrb            : std_logic_vector(3 downto 0);      
--  end record;
  
  
  
  -- Helper task for the transmitter monitor processes
  procedure decode_8b10b( d10  : in std_logic_vector(0 to 9);
                          q8   : out std_logic_vector(7 downto 0);
                          is_k : out std_logic
                          );
                          
   -- Function determines if the supplied character is a comma character.
   function is_comma(codegroup : in std_logic_vector(0 to 9)) return boolean;
   
   -- Function determines if the supplied character is an ILA.
   function is_ila(codegroup : in std_logic_vector(0 to 9)) return boolean;
   
   -- Function to write the AXI-lite bus
   procedure axi_write ( offset        : in natural;
                         data          : in std_logic_vector(31 downto 0);
                         signal axi    : inout axi_t);
   
   procedure axi_read ( offset        : in natural;
                        data          : out std_logic_vector(31 downto 0);
                        signal axi    : inout axi_t);
   
-- Repeat functions to replace verilog's {N{}} replication construct.   
   function repeat(N: natural; V: std_logic_vector) return std_logic_vector;
   function repeat(N: natural; V: std_logic) return std_logic_vector;
   
end package;

 
  
package body pkg_tb_xtx is


   -- Helper task for the transmitter monitor processes
   procedure decode_8b10b( d10  : in std_logic_vector(0 to 9);
                           q8   : out std_logic_vector(7 downto 0);
                           is_k : out std_logic
                          ) is
   variable k28     : std_logic;
   variable d10_rev : std_logic_vector(9 downto 0);
   begin                          
      -- reverse the 10B codeword
      for i in 0 to 9 loop
        d10_rev(i) := d10(i);
      end loop;

      case (d10_rev(5 downto 0)) is
        when "000110" => q8(4 downto 0) := "00000";   --D.0
        when "111001" => q8(4 downto 0) := "00000";   --D.0        
        when "010001" => q8(4 downto 0) := "00001";   --D.1
        when "101110" => q8(4 downto 0) := "00001";   --D.1
        when "010010" => q8(4 downto 0) := "00010";   --D.2
        when "101101" => q8(4 downto 0) := "00010";   --D.2
        when "100011" => q8(4 downto 0) := "00011";   --D.3
        when "010100" => q8(4 downto 0) := "00100";   --D.4
        when "101011" => q8(4 downto 0) := "00100";   --D.4
        when "100101" => q8(4 downto 0) := "00101";   --D.5
        when "100110" => q8(4 downto 0) := "00110";   --D.6
        when "000111" => q8(4 downto 0) := "00111";   --D.7
        when "111000" => q8(4 downto 0) := "00111";   --D.7
        when "011000" => q8(4 downto 0) := "01000";   --D.8
        when "100111" => q8(4 downto 0) := "01000";   --D.8
        when "101001" => q8(4 downto 0) := "01001";   --D.9
        when "101010" => q8(4 downto 0) := "01010";   --D.10
        when "001011" => q8(4 downto 0) := "01011";   --D.11
        when "101100" => q8(4 downto 0) := "01100";   --D.12
        when "001101" => q8(4 downto 0) := "01101";   --D.13
        when "001110" => q8(4 downto 0) := "01110";   --D.14
        when "000101" => q8(4 downto 0) := "01111";   --D.15
        when "111010" => q8(4 downto 0) := "01111";   --D.15
        when "110110" => q8(4 downto 0) := "10000";   --D.16
        when "001001" => q8(4 downto 0) := "10000";   --D.16
        when "110001" => q8(4 downto 0) := "10001";   --D.17
        when "110010" => q8(4 downto 0) := "10010";   --D.18
        when "010011" => q8(4 downto 0) := "10011";   --D.19
        when "110100" => q8(4 downto 0) := "10100";   --D.20
        when "010101" => q8(4 downto 0) := "10101";   --D.21
        when "010110" => q8(4 downto 0) := "10110";   --D.22
        when "010111" => q8(4 downto 0) := "10111";   --D/K.23
        when "101000" => q8(4 downto 0) := "10111";   --D/K.23
        when "001100" => q8(4 downto 0) := "11000";   --D.24
        when "110011" => q8(4 downto 0) := "11000";   --D.24
        when "011001" => q8(4 downto 0) := "11001";   --D.25
        when "011010" => q8(4 downto 0) := "11010";   --D.26
        when "011011" => q8(4 downto 0) := "11011";   --D/K.27
        when "100100" => q8(4 downto 0) := "11011";   --D/K.27
        when "011100" => q8(4 downto 0) := "11100";   --D.28
        when "111100" => q8(4 downto 0) := "11100";   --K.28
        when "000011" => q8(4 downto 0) := "11100";   --K.28
        when "011101" => q8(4 downto 0) := "11101";   --D/K.29
        when "100010" => q8(4 downto 0) := "11101";   --D/K.29
        when "011110" => q8(4 downto 0) := "11110";   --D.30
        when "100001" => q8(4 downto 0) := "11110";   --D.30
        when "110101" => q8(4 downto 0) := "11111";   --D.31
        when "001010" => q8(4 downto 0) := "11111";   --D.31
        when others   => q8(4 downto 0) := "11110";   --CODE VIOLATION - return /E/
      end case;

      k28 := not((d10(2) or d10(3) or d10(4) or d10(5) or not(d10(8) xor d10(9))));

      case (d10_rev(9 downto 6)) is
         when "0010" => q8(7 downto 5) := "000";   --D/K.x.0
         when "1101" => q8(7 downto 5) := "000";   --D/K.x.0
         when "1001" =>
            if (k28='0') then
               q8(7 downto 5) := "001";             --D/K.x.1
            else
               q8(7 downto 5) := "110";             --K28.6
            end if;
         when "0110" =>
            if (k28='1') then
               q8(7 downto 5) := "001";             --K.28.1
            else
               q8(7 downto 5) := "110";             --D/K.x.6
            end if;
         when "1010" =>
            if (k28='0') then
               q8(7 downto 5) := "010";             --D/K.x.2
            else
               q8(7 downto 5) := "101";             --K28.5
            end if;
         when "0101" =>
            if (k28='1') then
               q8(7 downto 5) := "010";             --K28.2
            else
               q8(7 downto 5) := "101";             --D/K.x.5
            end if;
        when "0011" => q8(7 downto 5) := "011";     --D/K.x.3
        when "1100" => q8(7 downto 5) := "011";     --D/K.x.3
        when "0100" => q8(7 downto 5) := "100";     --D/K.x.4
        when "1011" => q8(7 downto 5) := "100";     --D/K.x.4
        when "0111" => q8(7 downto 5) := "111";     --D.x.7
        when "1000" => q8(7 downto 5) := "111";     --D.x.7
        when "1110" => q8(7 downto 5) := "111";     --D/K.x.7
        when "0001" => q8(7 downto 5) := "111";     --D/K.x.7
        when others => q8(7 downto 5) := "111";     --CODE VIOLATION - return /E/
      end case;

      is_k := ((d10(2) and d10(3) and d10(4) and d10(5))
           or not(d10(2) or d10(3) or d10(4) or d10(5))
           or ((d10(4) xor d10(5)) and ((d10(5) and d10(7) and d10(8) and d10(9))
           or not(d10(5) or d10(7) or d10(8) or d10(9)))));

    end decode_8b10b;  
  
   -- Function determines if the supplied character is a comma character.
   function is_comma(codegroup : in std_logic_vector(0 to 9)) return boolean is
   begin
      case codegroup(0 to 6) is
         when "0011111" => return true;
         when "1100000" => return true;
         when others => return false;
      end case;
   end is_comma;
   
   -- Function determines if the supplied character is a ILA character.
   function is_ila(codegroup : in std_logic_vector(0 to 9)) return boolean is
   begin
      case codegroup(0 to 9) is
         when "0011110100" => return true;
         when "1100001011" => return true;
         when others => return false;
      end case;
   end is_ila;
   
   
   -- AXI-Lite Write procedure: defined here so that we have access to the AXI signals.
   procedure axi_write( offset        : in natural;
                        data          : in std_logic_vector(31 downto 0);
                        signal axi    : inout axi_t
                        ) is
   variable addr : std_logic_vector(31 downto 0);
   variable resp : std_logic_vector(1 downto 0);
   begin      

      -- shift offset to account for AXI byte addressing
      addr := std_logic_vector(to_unsigned(offset, 30)) & "00";
      -- Drive Address & Data valid
      wait until rising_edge(axi.aclk);
      wait for tSU;
      axi.awaddr  <= addr(11 downto 0);
      axi.awvalid <= '1';
      axi.wdata   <= data;
      axi.wvalid  <= '1';
      axi.bready  <= '0';
      -- Address Response Phase
      wait until falling_edge(axi.aclk);
      while (axi.awready = '0') loop
        wait until falling_edge(axi.aclk);
      end loop;
      
      wait until rising_edge(axi.aclk);
      wait for tSU;
      axi.awaddr  <= (others=>'0');
      axi.awvalid <= '0';
      -- Data Response Phase
      wait until falling_edge(axi.aclk);
      while (axi.wready = '0') loop
        wait until falling_edge(axi.aclk);
      end loop;
      wait until rising_edge(axi.aclk);
      wait for tSU;
      axi.wdata   <= (others=>'0');
      axi.wvalid  <= '0';
      -- BRESP phase
      wait until falling_edge(axi.aclk);
      while (axi.bvalid = '0') loop
         wait until falling_edge(axi.aclk);
      end loop;
      wait until rising_edge(axi.aclk);
      resp := axi.bresp;
      if (resp /= "00") then
         report "Error AXI BRESP not equal 0";
      end if;
      wait for tSU;
      axi.bready <= '1';
      wait until rising_edge(axi.aclk);
      wait for tSU;
      axi.bready <= '0';

   end axi_write;
      
   
   -- AXI-Lite Read task
   procedure axi_read(offset        : in natural;
                      data          : out std_logic_vector(31 downto 0);
                      signal axi    : inout axi_t) is
   variable addr : std_logic_vector(31 downto 0);
   variable resp : std_logic_vector(1 downto 0);                         
   begin

      -- shift offset to account for AXI byte addressing
      addr := std_logic_vector(to_unsigned(offset, 30)) & "00";
      -- Drive Address valid
      wait until rising_edge(axi.aclk);
      wait for tSU;
      axi.araddr  <= addr(11 downto 0);
      axi.arvalid <= '1';
      axi.rready  <= '0';
      -- Address Response Phase
      wait until falling_edge(axi.aclk);
      while (axi.arready = '0') loop
         wait until falling_edge(axi.aclk);
      end loop;
      wait until rising_edge(axi.aclk);
      wait for tSU;
      axi.araddr  <= (others=>'0');
      axi.arvalid <= '0';
      axi.rready  <= '1';
      -- Read Data Phase
      wait until falling_edge(axi.aclk);
      while (axi.rvalid = '0') loop
         wait until falling_edge(axi.aclk);
      end loop;
      wait until rising_edge(axi.aclk);
      data := axi.rdata;
      resp := axi.rresp;
      if (resp /= "00") then
         report "Error AXI RRESP not equal 0";
      end if;
      wait for tSU;
      axi.rready  <= '0';
          
  end axi_read;
 
-- Function repeat() replaces the Verilog replication construct {N{}} 
   function repeat(N: natural; V: std_logic_vector) return std_logic_vector is
   constant L: natural := V'length;
   variable result: std_logic_vector(0 to N*L - 1);
   begin
      for i in 0 to N-1 loop
         result(i*L to i*L + L - 1) := V;
      end loop;
      return result;
   end;
   
   function repeat(N: natural; V: std_logic) return std_logic_vector is
   variable result: std_logic_vector(0 to N - 1);
   begin
      for i in 0 to N-1 loop
         result(i) := V;
      end loop;
      return result;
   end;
 
end pkg_tb_xtx;