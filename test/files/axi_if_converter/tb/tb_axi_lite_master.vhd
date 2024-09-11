-------------------------------------------------------------------------------
-- Title      : AXI Lite Master Testbench
-- Project    :
-------------------------------------------------------------------------------
-- File       : tb_axi_lite_master.vhd
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

library ieee;
use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;
library xil_defaultlib;
use xil_defaultlib.axil_master_bfm.all;
use xil_defaultlib.global_sim.all;

-------------------------------------------------------------------------------

entity tb_axi_lite_master is
end entity tb_axi_lite_master;

-------------------------------------------------------------------------------

architecture TB of tb_axi_lite_master is

    constant C_M_AXIL_MASTER_TARGET_BASE_ADDR : std_logic_vector := x"0000_0000";
    constant C_M_AXIL_MASTER_ADDR_WIDTH       : integer          := 32;
    constant C_M_AXIL_MASTER_DATA_WIDTH       : integer          := 32;

    signal soft_reset        : std_logic := '0';
    signal transaction_error : std_logic;

    signal write_request : std_logic                     := '0';
    signal write_data    : std_logic_vector(31 downto 0) := x"0000_0000";
    signal write_address : std_logic_vector(31 downto 0) := x"0000_0000";
    signal write_done    : std_logic;

    signal read_request    : std_logic                     := '0';
    signal read_address    : std_logic_vector(31 downto 0) := x"0000_0000";
    signal read_data       : std_logic_vector (31 downto 0);
    signal read_data_valid : std_logic;

    signal m_axi_aclk    : std_logic := '1';
    signal m_axi_aresetn : std_logic;
    signal m_axi_awaddr  : std_logic_vector(C_M_AXIL_MASTER_ADDR_WIDTH-1 downto 0);
    signal m_axi_awprot  : std_logic_vector(2 downto 0);
    signal m_axi_awvalid : std_logic;
    signal m_axi_awready : std_logic;
    signal m_axi_wdata   : std_logic_vector(C_M_AXIL_MASTER_DATA_WIDTH-1 downto 0);
    signal m_axi_wstrb   : std_logic_vector(C_M_AXIL_MASTER_DATA_WIDTH/8-1 downto 0);
    signal m_axi_wvalid  : std_logic;
    signal m_axi_wready  : std_logic;
    signal m_axi_bresp   : std_logic_vector(1 downto 0);
    signal m_axi_bvalid  : std_logic;
    signal m_axi_bready  : std_logic;
    signal m_axi_araddr  : std_logic_vector(C_M_AXIL_MASTER_ADDR_WIDTH-1 downto 0);
    signal m_axi_arprot  : std_logic_vector(2 downto 0);
    signal m_axi_arvalid : std_logic;
    signal m_axi_arready : std_logic;
    signal m_axi_rdata   : std_logic_vector(C_M_AXIL_MASTER_DATA_WIDTH-1 downto 0);
    signal m_axi_rresp   : std_logic_vector(1 downto 0);
    signal m_axi_rvalid  : std_logic;
    signal m_axi_rready  : std_logic;

    signal clk        : std_logic := '0';
    signal resetn     : std_logic := '0';
    signal stop_clock : std_logic := '0';

    -- BFM Signals
    signal common_in_r  : m_common_response_r_type_in;
    signal common_out_r : m_common_response_r_type_out;
    signal common_in_w  : m_common_response_w_type_in;
    signal common_out_w : m_common_response_w_type_out;


begin

    -- Instantiation
    DUT : entity xil_defaultlib.axi_lite_master
        generic map (
            C_M_AXIL_MASTER_TARGET_BASE_ADDR => C_M_AXIL_MASTER_TARGET_BASE_ADDR,
            C_M_AXIL_MASTER_ADDR_WIDTH       => C_M_AXIL_MASTER_ADDR_WIDTH,
            C_M_AXIL_MASTER_DATA_WIDTH       => C_M_AXIL_MASTER_DATA_WIDTH
            )
        port map (
            soft_reset        => soft_reset,
            transaction_error => transaction_error,

            write_request => write_request,
            write_data    => write_data,
            write_address => write_address,
            write_done    => write_done,

            read_request    => read_request,
            read_address    => read_address,
            read_data       => read_data,
            read_data_valid => read_data_valid,

            m_axi_aclk    => m_axi_aclk,
            m_axi_aresetn => m_axi_aresetn,
            m_axi_awaddr  => m_axi_awaddr,
            m_axi_awprot  => m_axi_awprot,
            m_axi_awvalid => m_axi_awvalid,
            m_axi_awready => m_axi_awready,
            m_axi_wdata   => m_axi_wdata,
            m_axi_wstrb   => m_axi_wstrb,
            m_axi_wvalid  => m_axi_wvalid,
            m_axi_wready  => m_axi_wready,
            m_axi_bresp   => m_axi_bresp,
            m_axi_bvalid  => m_axi_bvalid,
            m_axi_bready  => m_axi_bready,
            m_axi_araddr  => m_axi_araddr,
            m_axi_arprot  => m_axi_arprot,
            m_axi_arvalid => m_axi_arvalid,
            m_axi_arready => m_axi_arready,
            m_axi_rdata   => m_axi_rdata,
            m_axi_rresp   => m_axi_rresp,
            m_axi_rvalid  => m_axi_rvalid,
            m_axi_rready  => m_axi_rready
            );


    -- Clock/reset gen
    clk           <= (not clk and not stop_clock) after AXI_CLK_T/2;
    m_axi_aclk    <= clk;
    m_axi_aresetn <= resetn;

    -- BFM Connections
    common_in_r                                 <= (m_axi_aclk, m_axi_arvalid, m_axi_rready);
    (m_axi_arready, m_axi_rvalid, m_axi_rdata)  <= common_out_r;
    common_in_w                                 <= (m_axi_aclk, m_axi_awvalid, m_axi_wvalid, m_axi_wdata, m_axi_bready);
    (m_axi_awready, m_axi_wready, m_axi_bvalid) <= common_out_w;


    -- Stimuli
    main : process
        procedure initialize_sim is
        begin
            common_out_w <= ('0', '0', '0');
            m_axi_bresp  <= b"00";
            common_out_r <= ('1', '0', (others => '0'));
            m_axi_rresp  <= b"00";

            wait for (10*AXI_CLK_T);
            resetn <= '1';
            wait for (5*AXI_CLK_T);
        end procedure initialize_sim;


        procedure master_read (
            signal common_in_r  : in  m_common_response_r_type_in;
            signal common_out_r : out m_common_response_r_type_out;
            constant address    : in  std_logic_vector(31 downto 0);
            constant data       : in  std_logic_vector(31 downto 0)
            ) is
        begin
            read_request <= '1';
            read_address <= address;
            wait for (AXI_CLK_T);
            read_request <= '0';
            read_address <= (others => '0');
            master_read_sim(common_in_r, common_out_r, data);
            wait for (2*AXI_CLK_T);
            read_request <= '0';
        end procedure master_read;


        procedure master_write (
            signal common_in_w  : in  m_common_response_w_type_in;
            signal common_out_w : out m_common_response_w_type_out;
            constant address    : in  std_logic_vector(31 downto 0);
            constant data       : in  std_logic_vector(31 downto 0)
            ) is
        begin
            write_request <= '1';
            write_address <= address;
            write_data    <= data;
            wait for (AXI_CLK_T);
            write_request <= '0';
            write_address <= (others => '0');
            write_data    <= (others => '0');
            master_write_sim(common_in_w, common_out_w);
            wait for (2*AXI_CLK_T);
        end procedure master_write;


        procedure read_tests is
            variable ADDR          : std_logic_vector(31 downto 0);
            variable EXPECTED_DATA : std_logic_vector(31 downto 0);
        begin
            ADDR          := x"DEAD_BEEF";
            EXPECTED_DATA := x"1357_FCA8";
            master_read(common_in_r, common_out_r, ADDR, EXPECTED_DATA);
            wait for (10*AXI_CLK_T);

            ADDR          := x"BABE_CAFE";
            EXPECTED_DATA := x"0123_3210";
            master_read(common_in_r, common_out_r, ADDR, EXPECTED_DATA);
            wait for (10*AXI_CLK_T);
        end procedure read_tests;


        procedure write_tests is
            variable ADDR : std_logic_vector(31 downto 0);
            variable DATA : std_logic_vector(31 downto 0);
        begin
            ADDR := x"DEAD_BEEF";
            DATA := x"1357_FCA8";
            master_write(common_in_w, common_out_w, ADDR, DATA);
            wait for (10*AXI_CLK_T);

            ADDR := x"BABE_CAFE";
            DATA := x"0123_3210";
            master_write(common_in_w, common_out_w, ADDR, DATA);
            wait for (10*AXI_CLK_T);
        end procedure write_tests;


    begin
        initialize_sim;
        read_tests;
        write_tests;
        end_test_and_stop_clock(stop_clock);
    end process main;

end architecture TB;
