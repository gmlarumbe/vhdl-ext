-------------------------------------------------------------------------------
-- Title      : AXI Lite Slave BFM
-- Project    :
-------------------------------------------------------------------------------
-- File       : axil_slave_bfm.vhd
-- Author     : Gonzalo Martinez Larumbe  <gonzalomlarumbe@gmail.com>
-- Company    :
-- Created    : 2020-02-12
-- Last update: 2020-02-12
-- Platform   : Debian 9.1
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description:
-------------------------------------------------------------------------------
-- Copyright (c) 2020
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2020-02-12  1.0      larumbe Created
-------------------------------------------------------------------------------

library IEEE;
library xil_defaultlib;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

package axil_slave_bfm is

    -----------------
    -- Write Types --
    -----------------
    type s_common_response_w_in is record
        s_axi_aclk    : std_logic;
        s_axi_awready : std_logic;
        s_axi_wready  : std_logic;
        s_axi_bvalid  : std_logic;
    end record s_common_response_w_in;

    type s_common_response_w_out is record
        s_axi_awaddr  : std_logic_vector(31 downto 0);
        s_axi_wdata   : std_logic_vector(31 downto 0);
        s_axi_wstrb   : std_logic_vector(3 downto 0);
        s_axi_awprot  : std_logic_vector(2 downto 0);
        s_axi_awvalid : std_logic;
        s_axi_wvalid  : std_logic;
        s_axi_bready  : std_logic;
    end record s_common_response_w_out;


    -- AXI Slave Write Request common signals and procedure--
    procedure slave_write_sim (
        signal axi_in   : in  s_common_response_w_in;
        signal axi_out  : out s_common_response_w_out;
        constant awaddr : in  std_logic_vector(31 downto 0);
        constant wdata  : in  std_logic_vector(31 downto 0)
        );


    ----------------
    -- Read Types --
    ----------------
    type s_common_response_r_in is record
        s_axi_aclk    : std_logic;
        s_axi_arready : std_logic;
        s_axi_rready  : std_logic;
        s_axi_rvalid  : std_logic;
    end record s_common_response_r_in;

    type s_common_response_r_out is record
        s_axi_araddr  : std_logic_vector(31 downto 0);
        s_axi_arvalid : std_logic;
        s_axi_rready  : std_logic;
    end record s_common_response_r_out;


    -- AXI Slave Read Request common signals and procedure--
    procedure slave_read_sim (
        signal axi_in   : in  s_common_response_r_in;
        signal axi_out  : out s_common_response_r_out;
        constant araddr : in  std_logic_vector(31 downto 0)
        );
end package axil_slave_bfm;




library IEEE;
library xil_defaultlib;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

package body axil_slave_bfm is

    procedure slave_write_sim (
        signal axi_in   : in  s_common_response_w_in;
        signal axi_out  : out s_common_response_w_out;
        constant awaddr : in  std_logic_vector(31 downto 0);
        constant wdata  : in  std_logic_vector(31 downto 0)
        ) is
    begin
        axi_out.s_axi_awaddr  <= awaddr;
        axi_out.s_axi_wdata   <= wdata;
        axi_out.s_axi_awvalid <= '1';
        axi_out.s_axi_wvalid  <= '1';
        axi_out.s_axi_awprot  <= b"000";
        axi_out.s_axi_wstrb   <= b"1111";
        axi_out.s_axi_bready  <= '0';

        wait until (axi_in.s_axi_bvalid = '1');
        axi_out.s_axi_awvalid <= '0';
        axi_out.s_axi_wvalid  <= '0';
        wait until rising_edge(axi_in.s_axi_aclk);
        axi_out.s_axi_bready  <= '1';
        wait until (axi_in.s_axi_bvalid = '0');
        axi_out.s_axi_bready  <= '0';
        wait until rising_edge(axi_in.s_axi_aclk);
    end slave_write_sim;


    procedure slave_read_sim (
        signal axi_in   : in  s_common_response_r_in;
        signal axi_out  : out s_common_response_r_out;
        constant araddr : in  std_logic_vector(31 downto 0)
        ) is
    begin
        axi_out.s_axi_araddr  <= araddr;
        axi_out.s_axi_arvalid <= '1';

        wait until (rising_edge(axi_in.s_axi_aclk) and axi_in.s_axi_arready = '1');
        axi_out.s_axi_arvalid <= '0';

        wait until (rising_edge(axi_in.s_axi_aclk) and axi_in.s_axi_rvalid = '1');
        axi_out.s_axi_rready <= '1';
        wait until (rising_edge(axi_in.s_axi_aclk));
        axi_out.s_axi_rready <= '0';
    end slave_read_sim;

end package body axil_slave_bfm;
