-------------------------------------------------------------------------------
-- Title      : AXI Lite Registers Testbench
-- Project    :
-------------------------------------------------------------------------------
-- File       : tb_axi_lite_regs.vhd
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
use xil_defaultlib.global.all;
use xil_defaultlib.global_sim.all;
use xil_defaultlib.axil_slave_bfm.all;

-------------------------------------------------------------------------------

entity tb_axi_lite_regs is
end entity tb_axi_lite_regs;

-------------------------------------------------------------------------------


architecture TB of tb_axi_lite_regs is

    signal soft_reset     : std_logic;
    signal system_enable  : std_logic;
    signal system_running : std_logic := '0';
    signal conv_op_lch    : std_logic;
    signal conv_op_rch    : std_logic;
    signal read_size_l    : unsigned(15 downto 0);
    signal read_size_r    : unsigned(15 downto 0);

    signal write_request : std_logic                     := '0';
    signal write_data    : std_logic_vector(31 downto 0) := x"0000_0000";
    signal write_address : std_logic_vector(31 downto 0) := x"0000_0000";
    signal write_done    : std_logic;

    signal read_request    : std_logic                     := '0';
    signal read_address    : std_logic_vector(31 downto 0) := x"0000_0000";
    signal read_data       : std_logic_vector (31 downto 0);
    signal read_data_valid : std_logic;

    signal transaction_error : std_logic;

    signal count_lch         : unsigned(31 downto 0);
    signal pattern_count_lch : unsigned(31 downto 0);
    signal count_rch         : unsigned(31 downto 0);
    signal pattern_count_rch : unsigned(31 downto 0);

    signal bram_overflow_error       : std_logic := '0';
    signal out_reg_underflow_error_l : std_logic := '0';
    signal out_reg_overflow_error_l  : std_logic := '0';
    signal out_reg_underflow_error_r : std_logic := '0';
    signal out_reg_overflow_error_r  : std_logic := '0';

    signal s_axi_aclk    : std_logic := '1';
    signal s_axi_aresetn : std_logic;
    signal s_axi_awaddr  : std_logic_vector(31 downto 0);
    signal s_axi_awprot  : std_logic_vector(2 downto 0);
    signal s_axi_awvalid : std_logic;
    signal s_axi_awready : std_logic;
    signal s_axi_wdata   : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal s_axi_wstrb   : std_logic_vector((C_S_AXI_DATA_WIDTH/8)-1 downto 0);
    signal s_axi_wvalid  : std_logic;
    signal s_axi_wready  : std_logic;
    signal s_axi_bresp   : std_logic_vector(1 downto 0);
    signal s_axi_bvalid  : std_logic;
    signal s_axi_bready  : std_logic;
    signal s_axi_araddr  : std_logic_vector(31 downto 0);
    signal s_axi_arprot  : std_logic_vector(2 downto 0);
    signal s_axi_arvalid : std_logic;
    signal s_axi_arready : std_logic;
    signal s_axi_rdata   : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal s_axi_rresp   : std_logic_vector(1 downto 0);
    signal s_axi_rvalid  : std_logic;
    signal s_axi_rready  : std_logic;

    signal stop_clock : std_logic := '0';

    -- BFM
    signal common_in_r  : s_common_response_r_in;
    signal common_out_r : s_common_response_r_out;
    signal common_in_w  : s_common_response_w_in;
    signal common_out_w : s_common_response_w_out;

begin

    -- Instances
    DUT : entity xil_defaultlib.axi_lite_regs
        generic map (
            C_S_AXI_DATA_WIDTH => C_S_AXI_DATA_WIDTH,
            C_S_AXI_ADDR_WIDTH => C_S_AXI_ADDR_WIDTH
            )
        port map (
            soft_reset => soft_reset,

            system_enable  => system_enable,
            system_running => system_running,
            conv_op_lch    => conv_op_lch,
            conv_op_rch    => conv_op_rch,
            read_size_l    => read_size_l,
            read_size_r    => read_size_r,

            write_request => write_request,
            write_data    => write_data,
            write_address => write_address,
            write_done    => write_done,

            read_request    => read_request,
            read_address    => read_address,
            read_data       => read_data,
            read_data_valid => read_data_valid,

            transaction_error => transaction_error,

            count_lch         => count_lch,
            pattern_count_lch => pattern_count_lch,
            count_rch         => count_rch,
            pattern_count_rch => pattern_count_rch,

            bram_overflow_error       => bram_overflow_error,
            out_reg_underflow_error_l => out_reg_underflow_error_l,
            out_reg_overflow_error_l  => out_reg_overflow_error_l,
            out_reg_underflow_error_r => out_reg_underflow_error_r,
            out_reg_overflow_error_r  => out_reg_overflow_error_r,

            s_axi_aclk    => s_axi_aclk,
            s_axi_aresetn => s_axi_aresetn,
            s_axi_awaddr  => s_axi_awaddr(C_S_AXI_ADDR_WIDTH-1 downto 0),
            s_axi_awprot  => s_axi_awprot,
            s_axi_awvalid => s_axi_awvalid,
            s_axi_awready => s_axi_awready,
            s_axi_wdata   => s_axi_wdata,
            s_axi_wstrb   => s_axi_wstrb,
            s_axi_wvalid  => s_axi_wvalid,
            s_axi_wready  => s_axi_wready,
            s_axi_bresp   => s_axi_bresp,
            s_axi_bvalid  => s_axi_bvalid,
            s_axi_bready  => s_axi_bready,
            s_axi_araddr  => s_axi_araddr(C_S_AXI_ADDR_WIDTH-1 downto 0),
            s_axi_arprot  => s_axi_arprot,
            s_axi_arvalid => s_axi_arvalid,
            s_axi_arready => s_axi_arready,
            s_axi_rdata   => s_axi_rdata,
            s_axi_rresp   => s_axi_rresp,
            s_axi_rvalid  => s_axi_rvalid,
            s_axi_rready  => s_axi_rready
            );


    -- Clock generation
    s_axi_aclk <= (not s_axi_aclk and not stop_clock) after AXI_CLK_T/2;

    -- BFM signal connection
    common_in_r                                                                                       <= (s_axi_aclk, s_axi_arready, s_axi_rready, s_axi_rvalid);
    (s_axi_araddr, s_axi_arvalid, s_axi_rready)                                                       <= common_out_r;
    common_in_w                                                                                       <= (s_axi_aclk, s_axi_awready, s_axi_wready, s_axi_bvalid);
    (s_axi_awaddr, s_axi_wdata, s_axi_wstrb, s_axi_awprot, s_axi_awvalid, s_axi_wvalid, s_axi_bready) <= common_out_w;


    -- Stimuli
    main : process

        procedure init_values is
        begin
            s_axi_aresetn <= '0';
            wait for (5*AXI_CLK_T);
            s_axi_aresetn <= '1';
            wait for (5*AXI_CLK_T);
        end init_values;


        procedure errors_status_reg is
        begin
            -- Read status reg
            read_status_reg(common_in_r, common_out_r);
            -- See errors
            bram_overflow_error       <= '1';
            out_reg_underflow_error_l <= '1';
            out_reg_overflow_error_l  <= '1';
            out_reg_underflow_error_r <= '1';
            out_reg_overflow_error_r  <= '1';
            -- Read again
            read_status_reg(common_in_r, common_out_r);
            bram_overflow_error       <= '0';
            out_reg_underflow_error_l <= '0';
            out_reg_overflow_error_l  <= '0';
            out_reg_underflow_error_r <= '0';
            out_reg_overflow_error_r  <= '0';
            read_status_reg(common_in_r, common_out_r);
        end procedure errors_status_reg;


        procedure system_running_status_reg is
        begin
            system_running <= '1';
            wait for (50*AXI_CLK_T);
            system_running <= '0';
        end procedure system_running_status_reg;

    begin

        init_values;
        read_control_reg(common_in_r, common_out_r);
        read_status_reg(common_in_r, common_out_r);
        read_version_reg(common_in_r, common_out_r);
        read_counters(common_in_r, common_out_r);
        write_control_reg_system_enable(common_in_w, common_out_w);
        write_control_reg_system_stop(common_in_w, common_out_w);
        write_control_reg_soft_reset(common_in_w, common_out_w);
        wait until soft_reset = '0';
        write_master_lite_write_request(common_in_w, common_out_w, x"0000_ADD0", x"BEBA_CAFE");
        write_master_lite_read_request(common_in_w, common_out_w, x"0000_ADD0");
        errors_status_reg;
        system_running_status_reg;
        write_converter_setup_reg(common_in_w, common_out_w, x"0000_0000");  -- Setup as S2MM
        write_converter_setup_reg(common_in_w, common_out_w, x"0000_0003");  -- Setup as MM2S
        read_counters(common_in_r, common_out_r);
        wait for (50*AXI_CLK_T);
        end_test_and_stop_clock(stop_clock);

    end process main;



    counters_proc : process is
    begin
        count_lch         <= (others => '0');
        pattern_count_lch <= (others => '0');
        count_rch         <= (others => '0');
        pattern_count_rch <= (others => '0');
        wait for (5*AXI_CLK_T);

        for i in 1 to 512 loop
            count_lch         <= count_lch + 1;
            pattern_count_lch <= pattern_count_lch + 1;
            count_rch         <= count_rch + 1;
            pattern_count_rch <= pattern_count_rch + 1;
            wait for (AXI_CLK_T);
        end loop;

        wait;
    end process counters_proc;


end architecture TB;
