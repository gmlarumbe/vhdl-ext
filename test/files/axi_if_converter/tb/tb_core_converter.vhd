-------------------------------------------------------------------------------
-- Title      : Core Converter Testbench
-- Project    :
-------------------------------------------------------------------------------
-- File       : tb_core_converter.vhd
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
use ieee.numeric_std.all;
use ieee.math_real.all;
library xil_defaultlib;
use xil_defaultlib.global.all;
use xil_defaultlib.global_sim.all;


entity tb_core_converter is
end entity tb_core_converter;


architecture TB of tb_core_converter is

    -- Seeds for random data generation
    constant SEED1_VALUE : integer := 1;
    constant SEED2_VALUE : integer := 999;

    -- DUT signals
    signal soft_reset : std_logic;
    signal conv_req   : conversion_req_t;
    signal conv_rsp   : conversion_rsp_t;

    signal fb_wr_burst_start : std_logic;
    signal fb_bw_counter     : std_logic_vector(7 downto 0);
    signal fb_wlast          : std_logic;
    signal fb_reduced_burst  : std_logic;
    signal fb_awlen          : std_logic_vector(7 downto 0);
    signal fb_burst_done     : std_logic;

    signal pattern_req      : std_logic            := '0';
    signal pattern_len      : unsigned(9 downto 0) := (others => '0');
    signal pattern_finished : std_logic;
    signal pattern_tlast    : std_logic            := '0';

    signal s_axis_aclk    : std_logic;
    signal s_axis_aresetn : std_logic;
    signal s_axis_tready  : std_logic;
    signal s_axis_tdata   : std_logic_vector(63 downto 0) := (others => '0');
    signal s_axis_tvalid  : std_logic                     := '0';
    signal s_axis_tkeep   : std_logic_vector(7 downto 0)  := (others => '0');
    signal s_axis_tlast   : std_logic                     := '0';

    signal m_axis_aclk    : std_logic;
    signal m_axis_aresetn : std_logic;
    signal m_axis_tdata   : std_logic_vector(63 downto 0);
    signal m_axis_tvalid  : std_logic;
    signal m_axis_tkeep   : std_logic_vector(7 downto 0);
    signal m_axis_tlast   : std_logic;
    signal m_axis_tready  : std_logic := '0';
    signal m_axis_tdest   : std_logic;

    signal m_axi_aclk    : std_logic;
    signal m_axi_aresetn : std_logic;
    signal m_axi_awid    : std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
    signal m_axi_awaddr  : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    signal m_axi_awlen   : std_logic_vector(7 downto 0);
    signal m_axi_awsize  : std_logic_vector(2 downto 0);
    signal m_axi_awburst : std_logic_vector(1 downto 0);
    signal m_axi_awlock  : std_logic;
    signal m_axi_awcache : std_logic_vector(3 downto 0);
    signal m_axi_awprot  : std_logic_vector(2 downto 0);
    signal m_axi_awqos   : std_logic_vector(3 downto 0);
    signal m_axi_awuser  : std_logic_vector(C_M_AXI_AWUSER_WIDTH-1 downto 0);
    signal m_axi_awvalid : std_logic;
    signal m_axi_awready : std_logic;
    signal m_axi_wdata   : std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
    signal m_axi_wstrb   : std_logic_vector(C_M_AXI_DATA_WIDTH/8-1 downto 0);
    signal m_axi_wlast   : std_logic;
    signal m_axi_wuser   : std_logic_vector(C_M_AXI_WUSER_WIDTH-1 downto 0);
    signal m_axi_wvalid  : std_logic;
    signal m_axi_wready  : std_logic;
    signal m_axi_bid     : std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
    signal m_axi_bresp   : std_logic_vector(1 downto 0);
    signal m_axi_buser   : std_logic_vector(C_M_AXI_BUSER_WIDTH-1 downto 0);
    signal m_axi_bvalid  : std_logic;
    signal m_axi_bready  : std_logic;
    signal m_axi_arid    : std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
    signal m_axi_araddr  : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    signal m_axi_arlen   : std_logic_vector(7 downto 0);
    signal m_axi_arsize  : std_logic_vector(2 downto 0);
    signal m_axi_arburst : std_logic_vector(1 downto 0);
    signal m_axi_arlock  : std_logic;
    signal m_axi_arcache : std_logic_vector(3 downto 0);
    signal m_axi_arprot  : std_logic_vector(2 downto 0);
    signal m_axi_arqos   : std_logic_vector(3 downto 0);
    signal m_axi_aruser  : std_logic_vector(C_M_AXI_ARUSER_WIDTH-1 downto 0);
    signal m_axi_arvalid : std_logic;
    signal m_axi_arready : std_logic;
    signal m_axi_rid     : std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
    signal m_axi_rdata   : std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
    signal m_axi_rresp   : std_logic_vector(1 downto 0);
    signal m_axi_rlast   : std_logic;
    signal m_axi_ruser   : std_logic_vector(C_M_AXI_RUSER_WIDTH-1 downto 0);
    signal m_axi_rvalid  : std_logic;
    signal m_axi_rready  : std_logic;

    signal internal_error : std_logic;

    -- TB signals
    signal clk        : std_logic := '1';
    signal resetn     : std_logic := '0';
    signal stop_clock : std_logic := '0';

    type test_stage_t is (
        INIT,
        WRITE_BURST,
        READ_BURST,
        READ_DATA_INTERRUPTED,
        WRITE_MULTIPLE_BURSTS,
        READ_MULTIPLE_BURSTS,
        WRITE_4K_BOUNDARY_TESTS,
        READ_4K_BOUNDARY_TESTS,
        PATTERN_REQ_TESTS,
        FINISH
        );

    signal test_stage : test_stage_t;

begin


    -- Clock generation
    clk         <= (not clk and not stop_clock) after AXI_CLK_T/2;
    m_axi_aclk  <= clk;
    m_axis_aclk <= clk;
    s_axis_aclk <= clk;


    -- Reset connections
    m_axi_aresetn  <= resetn;
    m_axis_aresetn <= resetn;
    s_axis_aresetn <= resetn;


    -- Instances
    DUT : entity xil_defaultlib.core_converter
        generic map (
            C_M_AXI_BURST_LEN    => C_M_AXI_BURST_LEN,
            C_M_AXI_ID_WIDTH     => C_M_AXI_ID_WIDTH,
            C_M_AXI_ADDR_WIDTH   => C_M_AXI_ADDR_WIDTH,
            C_M_AXI_DATA_WIDTH   => C_M_AXI_DATA_WIDTH,
            C_M_AXI_AWUSER_WIDTH => C_M_AXI_AWUSER_WIDTH,
            C_M_AXI_ARUSER_WIDTH => C_M_AXI_ARUSER_WIDTH,
            C_M_AXI_WUSER_WIDTH  => C_M_AXI_WUSER_WIDTH,
            C_M_AXI_RUSER_WIDTH  => C_M_AXI_RUSER_WIDTH,
            C_M_AXI_BUSER_WIDTH  => C_M_AXI_BUSER_WIDTH
            )
        port map (
            soft_reset => soft_reset,

            conv_req => conv_req,
            conv_rsp => conv_rsp,

            fb_wr_burst_start => fb_wr_burst_start,
            fb_bw_counter     => fb_bw_counter,
            fb_wlast          => fb_wlast,
            fb_reduced_burst  => fb_reduced_burst,
            fb_awlen          => fb_awlen,
            fb_burst_done     => fb_burst_done,

            pattern_req      => pattern_req,
            pattern_len      => pattern_len,
            pattern_finished => pattern_finished,
            pattern_tlast    => pattern_tlast,

            s_axis_aclk    => s_axis_aclk,
            s_axis_aresetn => s_axis_aresetn,
            s_axis_tready  => s_axis_tready,
            s_axis_tdata   => s_axis_tdata,
            s_axis_tvalid  => s_axis_tvalid,
            s_axis_tkeep   => s_axis_tkeep,
            s_axis_tlast   => s_axis_tlast,

            m_axis_aclk    => m_axis_aclk,
            m_axis_aresetn => m_axis_aresetn,
            m_axis_tdata   => m_axis_tdata,
            m_axis_tvalid  => m_axis_tvalid,
            m_axis_tkeep   => m_axis_tkeep,
            m_axis_tlast   => m_axis_tlast,
            m_axis_tready  => m_axis_tready,
            m_axis_tdest   => m_axis_tdest,

            m_axi_aclk    => m_axi_aclk,
            m_axi_aresetn => m_axi_aresetn,
            m_axi_awid    => m_axi_awid,
            m_axi_awaddr  => m_axi_awaddr,
            m_axi_awlen   => m_axi_awlen,
            m_axi_awsize  => m_axi_awsize,
            m_axi_awburst => m_axi_awburst,
            m_axi_awlock  => m_axi_awlock,
            m_axi_awcache => m_axi_awcache,
            m_axi_awprot  => m_axi_awprot,
            m_axi_awqos   => m_axi_awqos,
            m_axi_awuser  => m_axi_awuser,
            m_axi_awvalid => m_axi_awvalid,
            m_axi_awready => m_axi_awready,
            m_axi_wdata   => m_axi_wdata,
            m_axi_wstrb   => m_axi_wstrb,
            m_axi_wlast   => m_axi_wlast,
            m_axi_wuser   => m_axi_wuser,
            m_axi_wvalid  => m_axi_wvalid,
            m_axi_wready  => m_axi_wready,
            m_axi_bid     => m_axi_bid,
            m_axi_bresp   => m_axi_bresp,
            m_axi_buser   => m_axi_buser,
            m_axi_bvalid  => m_axi_bvalid,
            m_axi_bready  => m_axi_bready,
            m_axi_arid    => m_axi_arid,
            m_axi_araddr  => m_axi_araddr,
            m_axi_arlen   => m_axi_arlen,
            m_axi_arsize  => m_axi_arsize,
            m_axi_arburst => m_axi_arburst,
            m_axi_arlock  => m_axi_arlock,
            m_axi_arcache => m_axi_arcache,
            m_axi_arprot  => m_axi_arprot,
            m_axi_arqos   => m_axi_arqos,
            m_axi_aruser  => m_axi_aruser,
            m_axi_arvalid => m_axi_arvalid,
            m_axi_arready => m_axi_arready,
            m_axi_rid     => m_axi_rid,
            m_axi_rdata   => m_axi_rdata,
            m_axi_rresp   => m_axi_rresp,
            m_axi_rlast   => m_axi_rlast,
            m_axi_ruser   => m_axi_ruser,
            m_axi_rvalid  => m_axi_rvalid,
            m_axi_rready  => m_axi_rready,

            internal_error => internal_error
            );


    SlaveModel : entity xil_defaultlib.s_axi_model
        port map (
            s_axi_aclk     => clk,
            s_axi_aresetn  => resetn,
            s_axi_arid     => m_axi_arid,
            s_axi_araddr   => m_axi_araddr(9 downto 0),
            s_axi_arlen    => m_axi_arlen,
            s_axi_arsize   => m_axi_arsize,
            s_axi_arburst  => m_axi_arburst,
            s_axi_arlock   => m_axi_arlock,
            s_axi_arcache  => m_axi_arcache,
            s_axi_arprot   => m_axi_arprot,
            s_axi_arqos    => m_axi_arqos,
            s_axi_arregion => (others => '0'),
            s_axi_aruser   => m_axi_aruser,
            s_axi_arvalid  => m_axi_arvalid,
            s_axi_arready  => m_axi_arready,
            s_axi_rid      => m_axi_rid,
            s_axi_rdata    => m_axi_rdata,
            s_axi_rresp    => m_axi_rresp,
            s_axi_rlast    => m_axi_rlast,
            s_axi_ruser    => m_axi_ruser,
            s_axi_rvalid   => m_axi_rvalid,
            s_axi_rready   => m_axi_rready,
            s_axi_awid     => m_axi_awid,
            s_axi_awaddr   => m_axi_awaddr(9 downto 0),
            s_axi_awlen    => m_axi_awlen,
            s_axi_awsize   => m_axi_awsize,
            s_axi_awburst  => m_axi_awburst,
            s_axi_awlock   => m_axi_awlock,
            s_axi_awcache  => m_axi_awcache,
            s_axi_awprot   => m_axi_awprot,
            s_axi_awqos    => m_axi_awqos,
            s_axi_awregion => (others => '0'),
            s_axi_awuser   => m_axi_awuser,
            s_axi_awvalid  => m_axi_awvalid,
            s_axi_awready  => m_axi_awready,
            s_axi_wdata    => m_axi_wdata,
            s_axi_wstrb    => m_axi_wstrb,
            s_axi_wlast    => m_axi_wlast,
            s_axi_wuser    => m_axi_wuser,
            s_axi_wvalid   => m_axi_wvalid,
            s_axi_wready   => m_axi_wready,
            s_axi_bid      => m_axi_bid,
            s_axi_bresp    => m_axi_bresp,
            s_axi_buser    => m_axi_buser,
            s_axi_bvalid   => m_axi_bvalid,
            s_axi_bready   => m_axi_bready
            );



    -- Random data generator for AXI Stream slave interface
    s_axis_data_proc : process (s_axis_aclk) is
        variable seed1 : integer := SEED1_VALUE;
        variable seed2 : integer := SEED2_VALUE;

        impure function rand_slv(len : integer) return std_logic_vector is
            variable r   : real;
            variable slv : std_logic_vector(len - 1 downto 0);
        begin
            for i in slv'range loop
                uniform(seed1, seed2, r);
                slv(i) := '1' when r > 0.5 else '0';
            end loop;
            return slv;
        end function;

    begin
        if (rising_edge(s_axis_aclk)) then
            if (s_axis_aresetn = '0') then
                s_axis_tdata  <= (others => '0');
                s_axis_tvalid <= '0';
                s_axis_tkeep  <= x"FF";
                s_axis_tlast  <= '0';
            else
                s_axis_tvalid <= '1';
                if (s_axis_tready = '1') then
                    s_axis_tdata <= rand_slv(C_M_AXI_DATA_WIDTH);
                end if;
            end if;
        end if;

    end process s_axis_data_proc;


    -- Main Testbench process
    main : process

        procedure init_values is
        begin
            resetn        <= '0';
            soft_reset    <= '0';
            pattern_req   <= '0';
            pattern_len   <= (others => '0');
            pattern_tlast <= '1';
            m_axis_tready <= '0';

            conv_req.size    <= (others => '0');
            conv_req.address <= (others => '0');
            conv_req.op_type <= MM2S;
            conv_req.request <= '0';

            wait for (2*AXI_CLK_T);
            resetn        <= '1';
            soft_reset    <= '0';
            wait for (2*AXI_CLK_T);
            m_axis_tready <= '1';
        end procedure init_values;


        procedure delay_callback is
        begin
            wait for (4*AXI_CLK_T);
        end procedure delay_callback;


        procedure mm2s_op (
            constant size    : in natural;
            constant address : in natural
            ) is
        begin
            assert ((address mod 8) = 0) report "Address is not aligned with a 64-bit word" severity error;
            conv_req.op_type <= MM2S;
            conv_req.address <= std_logic_vector(to_unsigned(address, 32));
            conv_req.size    <= to_unsigned(size, 10);
            conv_req.request <= '0';
            wait for AXI_CLK_T;
            conv_req.request <= '1';
            wait for AXI_CLK_T;
            conv_req.request <= '0';
        end procedure mm2s_op;


        procedure s2mm_op (
            constant size    : in natural;
            constant address : in natural
            ) is
        begin
            assert ((address mod 8) = 0) report "Address is not aligned with a 64-bit word" severity error;
            conv_req.op_type <= S2MM;
            conv_req.address <= std_logic_vector(to_unsigned(address, 32));
            conv_req.size    <= to_unsigned(size, 10);
            conv_req.request <= '0';
            wait for AXI_CLK_T;
            conv_req.request <= '1';
            wait for AXI_CLK_T;
            conv_req.request <= '0';
        end procedure s2mm_op;


        procedure read_burst is
        begin
            test_stage <= READ_BURST;
            mm2s_op(size => 32, address => 0);
            wait until (conv_rsp.mm2s_done = '1');
            delay_callback;
        end procedure read_burst;


        procedure write_burst is
        begin
            test_stage <= WRITE_BURST;
            s2mm_op(size => 32, address => 0);
            wait until (conv_rsp.s2mm_done = '1');
            delay_callback;
        end procedure write_burst;


        procedure read_burst_pausing is
        begin
            test_stage    <= READ_DATA_INTERRUPTED;
            mm2s_op(size => 20, address => 32);
            wait for 10*AXI_CLK_T;
            m_axis_tready <= '0';
            wait for 4*AXI_CLK_T;
            m_axis_tready <= '1';
            wait until (conv_rsp.mm2s_done = '1');
            delay_callback;
        end procedure read_burst_pausing;


        procedure read_multiple_bursts is
        begin
            test_stage <= READ_MULTIPLE_BURSTS;
            mm2s_op(size => 32+32+12, address => 64);
            wait until (conv_rsp.mm2s_done = '1');
            delay_callback;
        end procedure read_multiple_bursts;


        procedure write_multiple_bursts is
        begin
            test_stage <= WRITE_MULTIPLE_BURSTS;
            s2mm_op(size => 32+32+12, address => 0);
            wait until (conv_rsp.s2mm_done = '1');
            delay_callback;
        end procedure write_multiple_bursts;


        procedure read_4k_boundary_only_short_burst is
        begin
            mm2s_op(size => 120, address => 4072);
            wait until (conv_rsp.mm2s_done = '1');
            delay_callback;
        end procedure read_4k_boundary_only_short_burst;


        procedure write_4k_boundary_only_short_burst is
        begin
            s2mm_op(size => 120, address => 4072);
            wait until (conv_rsp.s2mm_done = '1');
            delay_callback;
        end procedure write_4k_boundary_only_short_burst;


        procedure read_4k_boundary_full_and_short_bursts is
        begin
            mm2s_op(size => 64, address => 3824);
            wait until (conv_rsp.mm2s_done = '1');
            delay_callback;
        end procedure read_4k_boundary_full_and_short_bursts;


        procedure write_4k_boundary_full_and_short_bursts is
        begin
            s2mm_op(size => 64, address => 3824);
            wait until (conv_rsp.s2mm_done = '1');
            delay_callback;
        end procedure write_4k_boundary_full_and_short_bursts;


        procedure read_4k_boundary_word_beginning is
        begin
            mm2s_op(size => 1+31, address => 4088);
            wait until (conv_rsp.mm2s_done = '1');
            delay_callback;
        end procedure read_4k_boundary_word_beginning;


        procedure write_4k_boundary_word_beginning is
        begin
            s2mm_op(size => 1+31, address => 4088);
            wait until (conv_rsp.s2mm_done = '1');
            delay_callback;
        end procedure write_4k_boundary_word_beginning;


        procedure read_4k_boundary_word_end is
        begin
            mm2s_op(size => 31+1, address => 3848);
            wait until (conv_rsp.mm2s_done = '1');
            delay_callback;
        end procedure read_4k_boundary_word_end;


        procedure write_4k_boundary_word_end is
        begin
            s2mm_op(size => 31+1, address => 3848);
            wait until (conv_rsp.s2mm_done = '1');
            delay_callback;
        end procedure write_4k_boundary_word_end;


        procedure write_4k_boundary_tests is
        begin
            test_stage <= WRITE_4K_BOUNDARY_TESTS;
            write_4k_boundary_only_short_burst;
            write_4k_boundary_full_and_short_bursts;
            write_4k_boundary_word_beginning;
            write_4k_boundary_word_end;
        end procedure write_4k_boundary_tests;


        procedure read_4k_boundary_tests is
        begin
            test_stage <= READ_4K_BOUNDARY_TESTS;
            read_4k_boundary_only_short_burst;
            read_4k_boundary_full_and_short_bursts;
            read_4k_boundary_word_beginning;
            read_4k_boundary_word_end;
        end procedure read_4k_boundary_tests;


        procedure pattern_request (constant LENGTH : in natural) is
        begin
            wait until (rising_edge(m_axi_aclk));
            pattern_req <= '1';
            pattern_len <= to_unsigned(LENGTH, pattern_len'length);
            wait until (pattern_finished = '1');
            wait until (rising_edge(m_axi_aclk));
            pattern_req <= '0';
            pattern_len <= (others => '0');
            delay_callback;
        end procedure pattern_request;


        procedure pattern_request_pausing (
            constant LENGTH        : in natural;
            constant READY_2_START : in boolean;
            constant INIT_STOP     : in natural;
            constant END_STOP      : in natural
            ) is
        begin
            if (READY_2_START = true) then
                m_axis_tready <= '1';
            else
                m_axis_tready <= '0';
            end if;

            wait for (AXI_CLK_T);
            wait until (rising_edge(m_axi_aclk));

            pattern_req <= '1';
            pattern_len <= to_unsigned(LENGTH, pattern_len'length);

            for i in 1 to INIT_STOP loop
                wait until (rising_edge(m_axi_aclk));
            end loop;
            m_axis_tready <= '0';

            for i in 1 to END_STOP loop
                wait until (rising_edge(m_axi_aclk));
            end loop;
            m_axis_tready <= '1';

            wait until (pattern_finished = '1');
            wait for (1*AXI_CLK_T);

            pattern_req <= '0';
            pattern_len <= (others => '0');

            delay_callback;
        end procedure pattern_request_pausing;


        procedure pattern_request_tests is
        begin
            test_stage <= PATTERN_REQ_TESTS;
            pattern_request(1);
            pattern_request(2);
            pattern_request(5);
            pattern_request(20);
            pattern_request(1021);
            pattern_request_pausing(
                INIT_STOP     => 5,
                END_STOP      => 5,
                READY_2_START => true,
                LENGTH        => 10
                );
            pattern_request_pausing(
                INIT_STOP     => 5,
                END_STOP      => 1,
                READY_2_START => false,
                LENGTH        => 1
                );
        end procedure pattern_request_tests;


    begin
        test_stage <= INIT;
        init_values;

        write_burst;
        read_burst;
        read_burst_pausing;
        write_multiple_bursts;
        read_multiple_bursts;
        read_4k_boundary_tests;
        write_4k_boundary_tests;
        pattern_request_tests;

        test_stage <= FINISH;
        end_test_and_stop_clock(stop_clock);
    end process main;


end architecture TB;
