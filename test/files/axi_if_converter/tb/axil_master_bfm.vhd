-------------------------------------------------------------------------------
-- Title      : AXI Lite Master BFM
-- Project    :
-------------------------------------------------------------------------------
-- File       : axil_master_bfm.vhd
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

package axil_master_bfm is

    -----------------
    -- Write Types --
    -----------------
    type m_common_response_w_type_in is record
        m_axi_aclk    : std_logic;
        m_axi_awvalid : std_logic;
        m_axi_wvalid  : std_logic;
        m_axi_wdata   : std_logic_vector(31 downto 0);
        m_axi_bready  : std_logic;
    end record m_common_response_w_type_in;

    type m_common_response_w_type_out is record
        m_axi_awready : std_logic;
        m_axi_wready  : std_logic;
        m_axi_bvalid  : std_logic;
    end record m_common_response_w_type_out;


    procedure master_write_sim(
        signal common_in  : in  m_common_response_w_type_in;
        signal common_out : out m_common_response_w_type_out
        );

    ----------------
    -- Read Types --
    ----------------
    type m_common_response_r_type_in is record
        m_axi_aclk    : std_logic;
        m_axi_arvalid : std_logic;
        m_axi_rready  : std_logic;
    end record m_common_response_r_type_in;

    type m_common_response_r_type_out is record
        m_axi_arready : std_logic;
        m_axi_rvalid  : std_logic;
        m_axi_rdata   : std_logic_vector(31 downto 0);
    end record m_common_response_r_type_out;


    procedure master_read_sim(
        signal common_in  : in  m_common_response_r_type_in;
        signal common_out : out m_common_response_r_type_out;
        constant rdata    : in  std_logic_vector(31 downto 0)
        );

end package axil_master_bfm;


package body axil_master_bfm is

    -----------
    -- Write --
    -----------
    procedure master_write_sim (
        signal common_in  : in  m_common_response_w_type_in;
        signal common_out : out m_common_response_w_type_out
        ) is
    begin
        wait until (rising_edge(common_in.m_axi_aclk) and common_in.m_axi_awvalid = '1' and common_in.m_axi_wvalid = '1');
        common_out.m_axi_awready <= '1';
        common_out.m_axi_wready  <= '1';
        wait until (rising_edge(common_in.m_axi_aclk));
        common_out.m_axi_awready <= '0';
        common_out.m_axi_wready  <= '0';
        common_out.m_axi_bvalid  <= '1';
        wait until (rising_edge(common_in.m_axi_aclk) and common_in.m_axi_bready = '1');
        common_out.m_axi_bvalid  <= '0';
    end master_write_sim;

    ----------
    -- Read --
    ----------
    procedure master_read_sim (
        signal common_in  : in  m_common_response_r_type_in;
        signal common_out : out m_common_response_r_type_out;
        constant rdata    : in  std_logic_vector(31 downto 0)
        ) is
    begin
        wait until (rising_edge(common_in.m_axi_aclk) and common_in.m_axi_arvalid = '1');
        common_out.m_axi_arready <= '1';
        wait until (rising_edge(common_in.m_axi_aclk));
        common_out.m_axi_arready <= '0';
        common_out.m_axi_rvalid  <= '1';
        common_out.m_axi_rdata   <= rdata;
        wait until (rising_edge(common_in.m_axi_aclk) and common_in.m_axi_rready = '0');
        wait until (rising_edge(common_in.m_axi_aclk));
        common_out.m_axi_rvalid  <= '0';
    end master_read_sim;


end package body axil_master_bfm;
