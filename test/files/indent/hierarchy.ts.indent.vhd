-------------------------------------------------------------------------------
-- Title      : hierarchy.vhd
-- Project    :
-------------------------------------------------------------------------------
-- File       : hierarchy.vhd
-- Author     : Gonzalo Larumbe  <gonzalomlarumbe@gmail.com>
-- Company    : gmlarumbe
-- Created    : 2023-08-23
-- Last update: 2023-08-23
-- Platform   : Archvm
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description:
-------------------------------------------------------------------------------
-- Copyright (c) gmlarumbe
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2023-08-23  1.0      gonz    Created
-------------------------------------------------------------------------------

library ieee;
library xil_defaultlib;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use xil_defaultlib.global.all;
use xil_defaultlib.input_buffer_types.all;

entity axi_if_converter is

    port (
        -- Non-axi modules clocks and resets
        clk        : in std_logic;
        resetn     : in std_logic;
        clk_fs_ext : in std_logic;

        -- Axi lite register interface
        s_axi_aclk    : in  std_logic;
        s_axi_aresetn : in  std_logic;
        s_axi_awaddr  : in  std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
        s_axi_awprot  : in  std_logic_vector(2 downto 0);
        s_axi_awvalid : in  std_logic;
        s_axi_awready : out std_logic;
        s_axi_wdata   : in  std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
        s_axi_wstrb   : in  std_logic_vector((C_S_AXI_DATA_WIDTH/8)-1 downto 0);
        s_axi_wvalid  : in  std_logic;
        s_axi_wready  : out std_logic;
        s_axi_bresp   : out std_logic_vector(1 downto 0);
        s_axi_bvalid  : out std_logic;
        s_axi_bready  : in  std_logic;
        s_axi_araddr  : in  std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
        s_axi_arprot  : in  std_logic_vector(2 downto 0);
        s_axi_arvalid : in  std_logic;
        s_axi_arready : out std_logic;
        s_axi_rdata   : out std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
        s_axi_rresp   : out std_logic_vector(1 downto 0);
        s_axi_rvalid  : out std_logic;
        s_axi_rready  : in  std_logic;

        -- To input buffer (input channels)
        s_axis_lch_aclk    : in  std_logic;
        s_axis_lch_aresetn : in  std_logic;
        s_axis_lch_tdata   : in  std_logic_vector(63 downto 0);
        s_axis_lch_tvalid  : in  std_logic;
        s_axis_lch_tkeep   : in  std_logic_vector(7 downto 0);
        s_axis_lch_tlast   : in  std_logic;
        s_axis_lch_tready  : out std_logic;

        s_axis_rch_aclk    : in  std_logic;
        s_axis_rch_aresetn : in  std_logic;
        s_axis_rch_tdata   : in  std_logic_vector(63 downto 0);
        s_axis_rch_tvalid  : in  std_logic;
        s_axis_rch_tkeep   : in  std_logic_vector(7 downto 0);
        s_axis_rch_tlast   : in  std_logic;
        s_axis_rch_tready  : out std_logic;

        -- Stream Output from converters
        m_axis_lch_aclk    : in  std_logic;
        m_axis_lch_aresetn : in  std_logic;
        m_axis_lch_tdata   : out std_logic_vector(63 downto 0);
        m_axis_lch_tvalid  : out std_logic;
        m_axis_lch_tkeep   : out std_logic_vector(7 downto 0);
        m_axis_lch_tlast   : out std_logic;
        m_axis_lch_tready  : in  std_logic;
        m_axis_lch_tdest   : out std_logic;

        m_axis_rch_aclk    : in  std_logic;
        m_axis_rch_aresetn : in  std_logic;
        m_axis_rch_tdata   : out std_logic_vector(63 downto 0);
        m_axis_rch_tvalid  : out std_logic;
        m_axis_rch_tkeep   : out std_logic_vector(7 downto 0);
        m_axis_rch_tlast   : out std_logic;
        m_axis_rch_tready  : in  std_logic;
        m_axis_rch_tdest   : out std_logic;

        -- Axi full R/W interfaces from converter
        m_axi_lch_aclk    : in  std_logic;
        m_axi_lch_aresetn : in  std_logic;
        m_axi_lch_awid    : out std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
        m_axi_lch_awaddr  : out std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
        m_axi_lch_awlen   : out std_logic_vector(7 downto 0);
        m_axi_lch_awsize  : out std_logic_vector(2 downto 0);
        m_axi_lch_awburst : out std_logic_vector(1 downto 0);
        m_axi_lch_awlock  : out std_logic;
        m_axi_lch_awcache : out std_logic_vector(3 downto 0);
        m_axi_lch_awprot  : out std_logic_vector(2 downto 0);
        m_axi_lch_awqos   : out std_logic_vector(3 downto 0);
        m_axi_lch_awuser  : out std_logic_vector(C_M_AXI_AWUSER_WIDTH-1 downto 0);
        m_axi_lch_awvalid : out std_logic;
        m_axi_lch_awready : in  std_logic;
        m_axi_lch_wdata   : out std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
        m_axi_lch_wstrb   : out std_logic_vector(C_M_AXI_DATA_WIDTH/8-1 downto 0);
        m_axi_lch_wlast   : out std_logic;
        m_axi_lch_wuser   : out std_logic_vector(C_M_AXI_WUSER_WIDTH-1 downto 0);
        m_axi_lch_wvalid  : out std_logic;
        m_axi_lch_wready  : in  std_logic;
        m_axi_lch_bid     : in  std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
        m_axi_lch_bresp   : in  std_logic_vector(1 downto 0);
        m_axi_lch_buser   : in  std_logic_vector(C_M_AXI_BUSER_WIDTH-1 downto 0);
        m_axi_lch_bvalid  : in  std_logic;
        m_axi_lch_bready  : out std_logic;
        m_axi_lch_arid    : out std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
        m_axi_lch_araddr  : out std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
        m_axi_lch_arlen   : out std_logic_vector(7 downto 0);
        m_axi_lch_arsize  : out std_logic_vector(2 downto 0);
        m_axi_lch_arburst : out std_logic_vector(1 downto 0);
        m_axi_lch_arlock  : out std_logic;
        m_axi_lch_arcache : out std_logic_vector(3 downto 0);
        m_axi_lch_arprot  : out std_logic_vector(2 downto 0);
        m_axi_lch_arqos   : out std_logic_vector(3 downto 0);
        m_axi_lch_aruser  : out std_logic_vector(C_M_AXI_ARUSER_WIDTH-1 downto 0);
        m_axi_lch_arvalid : out std_logic;
        m_axi_lch_arready : in  std_logic;
        m_axi_lch_rid     : in  std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
        m_axi_lch_rdata   : in  std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
        m_axi_lch_rresp   : in  std_logic_vector(1 downto 0);
        m_axi_lch_rlast   : in  std_logic;
        m_axi_lch_ruser   : in  std_logic_vector(C_M_AXI_RUSER_WIDTH-1 downto 0);
        m_axi_lch_rvalid  : in  std_logic;
        m_axi_lch_rready  : out std_logic;

        m_axi_rch_aclk    : in  std_logic;
        m_axi_rch_aresetn : in  std_logic;
        m_axi_rch_awid    : out std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
        m_axi_rch_awaddr  : out std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
        m_axi_rch_awlen   : out std_logic_vector(7 downto 0);
        m_axi_rch_awsize  : out std_logic_vector(2 downto 0);
        m_axi_rch_awburst : out std_logic_vector(1 downto 0);
        m_axi_rch_awlock  : out std_logic;
        m_axi_rch_awcache : out std_logic_vector(3 downto 0);
        m_axi_rch_awprot  : out std_logic_vector(2 downto 0);
        m_axi_rch_awqos   : out std_logic_vector(3 downto 0);
        m_axi_rch_awuser  : out std_logic_vector(C_M_AXI_AWUSER_WIDTH-1 downto 0);
        m_axi_rch_awvalid : out std_logic;
        m_axi_rch_awready : in  std_logic;
        m_axi_rch_wdata   : out std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
        m_axi_rch_wstrb   : out std_logic_vector(C_M_AXI_DATA_WIDTH/8-1 downto 0);
        m_axi_rch_wlast   : out std_logic;
        m_axi_rch_wuser   : out std_logic_vector(C_M_AXI_WUSER_WIDTH-1 downto 0);
        m_axi_rch_wvalid  : out std_logic;
        m_axi_rch_wready  : in  std_logic;
        m_axi_rch_bid     : in  std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
        m_axi_rch_bresp   : in  std_logic_vector(1 downto 0);
        m_axi_rch_buser   : in  std_logic_vector(C_M_AXI_BUSER_WIDTH-1 downto 0);
        m_axi_rch_bvalid  : in  std_logic;
        m_axi_rch_bready  : out std_logic;
        m_axi_rch_arid    : out std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
        m_axi_rch_araddr  : out std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
        m_axi_rch_arlen   : out std_logic_vector(7 downto 0);
        m_axi_rch_arsize  : out std_logic_vector(2 downto 0);
        m_axi_rch_arburst : out std_logic_vector(1 downto 0);
        m_axi_rch_arlock  : out std_logic;
        m_axi_rch_arcache : out std_logic_vector(3 downto 0);
        m_axi_rch_arprot  : out std_logic_vector(2 downto 0);
        m_axi_rch_arqos   : out std_logic_vector(3 downto 0);
        m_axi_rch_aruser  : out std_logic_vector(C_M_AXI_ARUSER_WIDTH-1 downto 0);
        m_axi_rch_arvalid : out std_logic;
        m_axi_rch_arready : in  std_logic;
        m_axi_rch_rid     : in  std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
        m_axi_rch_rdata   : in  std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
        m_axi_rch_rresp   : in  std_logic_vector(1 downto 0);
        m_axi_rch_rlast   : in  std_logic;
        m_axi_rch_ruser   : in  std_logic_vector(C_M_AXI_RUSER_WIDTH-1 downto 0);
        m_axi_rch_rvalid  : in  std_logic;
        m_axi_rch_rready  : out std_logic;

        -- Axi lite master auxiliary interface (configuration parameters)
        m_axi_conf_aclk    : in  std_logic;
        m_axi_conf_aresetn : in  std_logic;
        m_axi_conf_awaddr  : out std_logic_vector(C_M_AXIL_MASTER_ADDR_WIDTH-1 downto 0);
        m_axi_conf_awprot  : out std_logic_vector(2 downto 0);
        m_axi_conf_awvalid : out std_logic;
        m_axi_conf_awready : in  std_logic;
        m_axi_conf_wdata   : out std_logic_vector(C_M_AXIL_MASTER_DATA_WIDTH-1 downto 0);
        m_axi_conf_wstrb   : out std_logic_vector(C_M_AXIL_MASTER_DATA_WIDTH/8-1 downto 0);
        m_axi_conf_wvalid  : out std_logic;
        m_axi_conf_wready  : in  std_logic;
        m_axi_conf_bresp   : in  std_logic_vector(1 downto 0);
        m_axi_conf_bvalid  : in  std_logic;
        m_axi_conf_bready  : out std_logic;
        m_axi_conf_araddr  : out std_logic_vector(C_M_AXIL_MASTER_ADDR_WIDTH-1 downto 0);
        m_axi_conf_arprot  : out std_logic_vector(2 downto 0);
        m_axi_conf_arvalid : out std_logic;
        m_axi_conf_arready : in  std_logic;
        m_axi_conf_rdata   : in  std_logic_vector(C_M_AXIL_MASTER_DATA_WIDTH-1 downto 0);
        m_axi_conf_rresp   : in  std_logic_vector(1 downto 0);
        m_axi_conf_rvalid  : in  std_logic;
        m_axi_conf_rready  : out std_logic
    );

end entity axi_if_converter;


architecture RTL of axi_if_converter is

    -- Global signals
    signal soft_reset : std_logic;

    -- Input buffer <-> Converter AXI signals
    signal m_axis_lch_inbuf_aclk    : std_logic;
    signal m_axis_lch_inbuf_aresetn : std_logic;
    signal m_axis_lch_inbuf_tdata   : std_logic_vector(63 downto 0);
    signal m_axis_lch_inbuf_tvalid  : std_logic;
    signal m_axis_lch_inbuf_tkeep   : std_logic_vector(7 downto 0);
    signal m_axis_lch_inbuf_tlast   : std_logic;
    signal m_axis_lch_inbuf_tready  : std_logic;

    signal m_axis_rch_inbuf_aclk    : std_logic;
    signal m_axis_rch_inbuf_aresetn : std_logic;
    signal m_axis_rch_inbuf_tdata   : std_logic_vector(63 downto 0);
    signal m_axis_rch_inbuf_tvalid  : std_logic;
    signal m_axis_rch_inbuf_tkeep   : std_logic_vector(7 downto 0);
    signal m_axis_rch_inbuf_tlast   : std_logic;
    signal m_axis_rch_inbuf_tready  : std_logic;

    -- Input_buffer <-> Converters Feedback signals
    signal fb_wr_burst_start_lch : std_logic;
    signal fb_bw_counter_lch     : std_logic_vector(7 downto 0);
    signal fb_wlast_lch          : std_logic;
    signal fb_reduced_burst_lch  : std_logic;
    signal fb_awlen_lch          : std_logic_vector(7 downto 0);
    signal fb_burst_done_lch     : std_logic;
    signal fb_send_size_l        : unsigned(9 downto 0);

    signal fb_wr_burst_start_rch : std_logic;
    signal fb_bw_counter_rch     : std_logic_vector(7 downto 0);
    signal fb_wlast_rch          : std_logic;
    signal fb_reduced_burst_rch  : std_logic;
    signal fb_awlen_rch          : std_logic_vector(7 downto 0);
    signal fb_burst_done_rch     : std_logic;
    signal fb_send_size_r        : unsigned(9 downto 0);

    -- Input buffer <-> Register interface signals
    signal bram_overflow_error       : std_logic;
    signal out_reg_underflow_error_l : std_logic;
    signal out_reg_overflow_error_l  : std_logic;
    signal out_reg_underflow_error_r : std_logic;
    signal out_reg_overflow_error_r  : std_logic;

    -- AXI Lite Master <-> Register interface signals
    signal transaction_error : std_logic;

    signal write_request : std_logic;
    signal write_data    : std_logic_vector(31 downto 0);
    signal write_address : std_logic_vector(31 downto 0);
    signal write_done    : std_logic;

    signal read_request    : std_logic;
    signal read_address    : std_logic_vector(31 downto 0);
    signal read_data       : std_logic_vector (31 downto 0);
    signal read_data_valid : std_logic;

    -- Pattern counters <-> Register interface
    signal count_lch         : unsigned(31 downto 0);
    signal pattern_count_lch : unsigned(31 downto 0);
    signal count_rch         : unsigned(31 downto 0);
    signal pattern_count_rch : unsigned(31 downto 0);

    -- Registers <-> FSM signals
    signal system_enable      : std_logic;
    signal system_running     : std_logic;
    signal system_running_lch : std_logic;
    signal system_running_rch : std_logic;
    signal conv_op_lch        : std_logic;
    signal conv_op_rch        : std_logic;
    signal read_size_l        : unsigned(15 downto 0);
    signal read_size_r        : unsigned(15 downto 0);


    -- Converters <-> FSM signals
    signal conv_req_lch         : conversion_req_t;
    signal conv_rsp_lch         : conversion_rsp_t;
    signal internal_error_lch   : std_logic;
    signal pattern_req_lch      : std_logic;
    signal pattern_len_lch      : unsigned(9 downto 0);
    signal pattern_finished_lch : std_logic;
    signal pattern_tlast_lch    : std_logic;

    signal conv_req_rch         : conversion_req_t;
    signal conv_rsp_rch         : conversion_rsp_t;
    signal internal_error_rch   : std_logic;
    signal pattern_req_rch      : std_logic;
    signal pattern_len_rch      : unsigned(9 downto 0);
    signal pattern_finished_rch : std_logic;
    signal pattern_tlast_rch    : std_logic;

    -- Input Buffer <-> FSM signals
    signal buffer_size_l : unsigned(10 downto 0);
    signal bram_ptr_l    : std_logic_vector(31 downto 0);
    signal buffer_size_r : unsigned(10 downto 0);
    signal bram_ptr_r    : std_logic_vector(31 downto 0);

    -- FS clock divider
    signal clk_fs      : std_logic;
    signal clk_fs_sync : std_logic;


begin

    -- Comb logic
    fb_send_size_l <= b"00" & unsigned(fb_awlen_lch);
    fb_send_size_r <= b"00" & unsigned(fb_awlen_rch);
    system_running <= system_running_lch or system_running_rch;

    -- Instances
    I_AXI_LITE_REGS : entity xil_defaultlib.axi_lite_regs
    generic map (
        C_S_AXI_DATA_WIDTH => C_S_AXI_DATA_WIDTH,
        C_S_AXI_ADDR_WIDTH => C_S_AXI_ADDR_WIDTH
    )
    port map (
        soft_reset => soft_reset,

        -- To FSM
        system_enable  => system_enable,
        system_running => system_running,
        conv_op_lch    => conv_op_lch,
        conv_op_rch    => conv_op_rch,
        read_size_l    => read_size_l,
        read_size_r    => read_size_r,

        -- To axi-lite master
        write_request => write_request,
        write_data    => write_data,
        write_address => write_address,
        write_done    => write_done,

        read_request    => read_request,
        read_address    => read_address,
        read_data       => read_data,
        read_data_valid => read_data_valid,

        transaction_error => transaction_error,

        -- From pattern counters
        count_lch         => count_lch,
        pattern_count_lch => pattern_count_lch,
        count_rch         => count_rch,
        pattern_count_rch => pattern_count_rch,

        -- Input buffer error checking
        bram_overflow_error       => bram_overflow_error,
        out_reg_underflow_error_l => out_reg_underflow_error_l,
        out_reg_overflow_error_l  => out_reg_overflow_error_l,
        out_reg_underflow_error_r => out_reg_underflow_error_r,
        out_reg_overflow_error_r  => out_reg_overflow_error_r,

        s_axi_aclk    => s_axi_aclk,
        s_axi_aresetn => s_axi_aresetn,
        s_axi_awaddr  => s_axi_awaddr,
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
        s_axi_araddr  => s_axi_araddr,
        s_axi_arprot  => s_axi_arprot,
        s_axi_arvalid => s_axi_arvalid,
        s_axi_arready => s_axi_arready,
        s_axi_rdata   => s_axi_rdata,
        s_axi_rresp   => s_axi_rresp,
        s_axi_rvalid  => s_axi_rvalid,
        s_axi_rready  => s_axi_rready
    );


    I_INPUT_BUFFER : entity xil_defaultlib.input_buffer
    generic map (
        C_M_AXI_BURST_LEN     => C_M_AXI_BURST_LEN,
        LEFT_CH_BASE_ADDRESS  => LEFT_CH_ST_BASE_ADDRESS,
        RIGHT_CH_BASE_ADDRESS => RIGHT_CH_ST_BASE_ADDRESS
    )
    port map (
        soft_reset => soft_reset,

        -- Feedback from converters
        inputs.start_burst_master_l => fb_wr_burst_start_lch,
        inputs.bw_counter_l         => fb_bw_counter_lch,
        inputs.wlast_l              => fb_wlast_lch,
        inputs.short_burst_l        => fb_reduced_burst_lch,
        inputs.write_done_l         => fb_burst_done_lch,
        inputs.send_size_l          => fb_send_size_l,

        inputs.start_burst_master_r => fb_wr_burst_start_rch,
        inputs.bw_counter_r         => fb_bw_counter_rch,
        inputs.wlast_r              => fb_wlast_rch,
        inputs.short_burst_r        => fb_reduced_burst_rch,
        inputs.write_done_r         => fb_burst_done_rch,
        inputs.send_size_r          => fb_send_size_r,

        outputs.buffer_size_l => buffer_size_l,
        outputs.bram_ptr_l    => bram_ptr_l,
        outputs.buffer_size_r => buffer_size_r,
        outputs.bram_ptr_r    => bram_ptr_r,

        s_axis_lch_aclk    => s_axis_lch_aclk,
        s_axis_lch_aresetn => s_axis_lch_aresetn,
        s_axis_lch_tdata   => s_axis_lch_tdata,
        s_axis_lch_tvalid  => s_axis_lch_tvalid,
        s_axis_lch_tkeep   => s_axis_lch_tkeep,
        s_axis_lch_tlast   => s_axis_lch_tlast,
        s_axis_lch_tready  => s_axis_lch_tready,

        s_axis_rch_aclk    => s_axis_rch_aclk,
        s_axis_rch_aresetn => s_axis_rch_aresetn,
        s_axis_rch_tdata   => s_axis_rch_tdata,
        s_axis_rch_tvalid  => s_axis_rch_tvalid,
        s_axis_rch_tkeep   => s_axis_rch_tkeep,
        s_axis_rch_tlast   => s_axis_rch_tlast,
        s_axis_rch_tready  => s_axis_rch_tready,

        m_axis_lch_aclk    => m_axis_lch_inbuf_aclk,
        m_axis_lch_aresetn => m_axis_lch_inbuf_aresetn,
        m_axis_lch_tdata   => m_axis_lch_inbuf_tdata,
        m_axis_lch_tvalid  => m_axis_lch_inbuf_tvalid,
        m_axis_lch_tkeep   => m_axis_lch_inbuf_tkeep,
        m_axis_lch_tlast   => m_axis_lch_inbuf_tlast,
        m_axis_lch_tready  => m_axis_lch_inbuf_tready,

        m_axis_rch_aclk    => m_axis_rch_inbuf_aclk,
        m_axis_rch_aresetn => m_axis_rch_inbuf_aresetn,
        m_axis_rch_tdata   => m_axis_rch_inbuf_tdata,
        m_axis_rch_tvalid  => m_axis_rch_inbuf_tvalid,
        m_axis_rch_tkeep   => m_axis_rch_inbuf_tkeep,
        m_axis_rch_tlast   => m_axis_rch_inbuf_tlast,
        m_axis_rch_tready  => m_axis_rch_inbuf_tready,

        bram_overflow_error       => bram_overflow_error,
        out_reg_underflow_error_l => out_reg_underflow_error_l,
        out_reg_overflow_error_l  => out_reg_overflow_error_l,
        out_reg_underflow_error_r => out_reg_underflow_error_r,
        out_reg_overflow_error_r  => out_reg_overflow_error_r
    );



    I_CORE_CONVERTER_L : entity xil_defaultlib.core_converter
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

        conv_req       => conv_req_lch,
        conv_rsp       => conv_rsp_lch,
        internal_error => internal_error_lch,

        fb_wr_burst_start => fb_wr_burst_start_lch,
        fb_bw_counter     => fb_bw_counter_lch,
        fb_wlast          => fb_wlast_lch,
        fb_reduced_burst  => fb_reduced_burst_lch,
        fb_awlen          => fb_awlen_lch,
        fb_burst_done     => fb_burst_done_lch,

        pattern_req      => pattern_req_lch,
        pattern_len      => pattern_len_lch,
        pattern_finished => pattern_finished_lch,
        pattern_tlast    => pattern_tlast_lch,

        s_axis_aclk    => m_axis_lch_inbuf_aclk,
        s_axis_aresetn => m_axis_lch_inbuf_aresetn,
        s_axis_tready  => m_axis_lch_inbuf_tready,
        s_axis_tdata   => m_axis_lch_inbuf_tdata,
        s_axis_tvalid  => m_axis_lch_inbuf_tvalid,
        s_axis_tkeep   => m_axis_lch_inbuf_tkeep,
        s_axis_tlast   => m_axis_lch_inbuf_tlast,

        m_axis_aclk    => m_axis_lch_aclk,
        m_axis_aresetn => m_axis_lch_aresetn,
        m_axis_tdata   => m_axis_lch_tdata,
        m_axis_tvalid  => m_axis_lch_tvalid,
        m_axis_tkeep   => m_axis_lch_tkeep,
        m_axis_tlast   => m_axis_lch_tlast,
        m_axis_tready  => m_axis_lch_tready,
        m_axis_tdest   => m_axis_lch_tdest,

        m_axi_aclk    => m_axi_lch_aclk,
        m_axi_aresetn => m_axi_lch_aresetn,
        m_axi_awid    => m_axi_lch_awid,
        m_axi_awaddr  => m_axi_lch_awaddr,
        m_axi_awlen   => m_axi_lch_awlen,
        m_axi_awsize  => m_axi_lch_awsize,
        m_axi_awburst => m_axi_lch_awburst,
        m_axi_awlock  => m_axi_lch_awlock,
        m_axi_awcache => m_axi_lch_awcache,
        m_axi_awprot  => m_axi_lch_awprot,
        m_axi_awqos   => m_axi_lch_awqos,
        m_axi_awuser  => m_axi_lch_awuser,
        m_axi_awvalid => m_axi_lch_awvalid,
        m_axi_awready => m_axi_lch_awready,
        m_axi_wdata   => m_axi_lch_wdata,
        m_axi_wstrb   => m_axi_lch_wstrb,
        m_axi_wlast   => m_axi_lch_wlast,
        m_axi_wuser   => m_axi_lch_wuser,
        m_axi_wvalid  => m_axi_lch_wvalid,
        m_axi_wready  => m_axi_lch_wready,
        m_axi_bid     => m_axi_lch_bid,
        m_axi_bresp   => m_axi_lch_bresp,
        m_axi_buser   => m_axi_lch_buser,
        m_axi_bvalid  => m_axi_lch_bvalid,
        m_axi_bready  => m_axi_lch_bready,
        m_axi_arid    => m_axi_lch_arid,
        m_axi_araddr  => m_axi_lch_araddr,
        m_axi_arlen   => m_axi_lch_arlen,
        m_axi_arsize  => m_axi_lch_arsize,
        m_axi_arburst => m_axi_lch_arburst,
        m_axi_arlock  => m_axi_lch_arlock,
        m_axi_arcache => m_axi_lch_arcache,
        m_axi_arprot  => m_axi_lch_arprot,
        m_axi_arqos   => m_axi_lch_arqos,
        m_axi_aruser  => m_axi_lch_aruser,
        m_axi_arvalid => m_axi_lch_arvalid,
        m_axi_arready => m_axi_lch_arready,
        m_axi_rid     => m_axi_lch_rid,
        m_axi_rdata   => m_axi_lch_rdata,
        m_axi_rresp   => m_axi_lch_rresp,
        m_axi_rlast   => m_axi_lch_rlast,
        m_axi_ruser   => m_axi_lch_ruser,
        m_axi_rvalid  => m_axi_lch_rvalid,
        m_axi_rready  => m_axi_lch_rready
    );


    I_CORE_CONVERTER_R : entity xil_defaultlib.core_converter
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

        conv_req       => conv_req_rch,
        conv_rsp       => conv_rsp_rch,
        internal_error => internal_error_rch,

        fb_wr_burst_start => fb_wr_burst_start_rch,
        fb_bw_counter     => fb_bw_counter_rch,
        fb_wlast          => fb_wlast_rch,
        fb_reduced_burst  => fb_reduced_burst_rch,
        fb_awlen          => fb_awlen_rch,
        fb_burst_done     => fb_burst_done_rch,

        pattern_req      => pattern_req_rch,
        pattern_len      => pattern_len_rch,
        pattern_finished => pattern_finished_rch,
        pattern_tlast    => pattern_tlast_rch,

        s_axis_aclk    => m_axis_rch_inbuf_aclk,
        s_axis_aresetn => m_axis_rch_inbuf_aresetn,
        s_axis_tready  => m_axis_rch_inbuf_tready,
        s_axis_tdata   => m_axis_rch_inbuf_tdata,
        s_axis_tvalid  => m_axis_rch_inbuf_tvalid,
        s_axis_tkeep   => m_axis_rch_inbuf_tkeep,
        s_axis_tlast   => m_axis_rch_inbuf_tlast,

        m_axis_aclk    => m_axis_rch_aclk,
        m_axis_aresetn => m_axis_rch_aresetn,
        m_axis_tdata   => m_axis_rch_tdata,
        m_axis_tvalid  => m_axis_rch_tvalid,
        m_axis_tkeep   => m_axis_rch_tkeep,
        m_axis_tlast   => m_axis_rch_tlast,
        m_axis_tready  => m_axis_rch_tready,
        m_axis_tdest   => m_axis_rch_tdest,

        m_axi_aclk    => m_axi_rch_aclk,
        m_axi_aresetn => m_axi_rch_aresetn,
        m_axi_awid    => m_axi_rch_awid,
        m_axi_awaddr  => m_axi_rch_awaddr,
        m_axi_awlen   => m_axi_rch_awlen,
        m_axi_awsize  => m_axi_rch_awsize,
        m_axi_awburst => m_axi_rch_awburst,
        m_axi_awlock  => m_axi_rch_awlock,
        m_axi_awcache => m_axi_rch_awcache,
        m_axi_awprot  => m_axi_rch_awprot,
        m_axi_awqos   => m_axi_rch_awqos,
        m_axi_awuser  => m_axi_rch_awuser,
        m_axi_awvalid => m_axi_rch_awvalid,
        m_axi_awready => m_axi_rch_awready,
        m_axi_wdata   => m_axi_rch_wdata,
        m_axi_wstrb   => m_axi_rch_wstrb,
        m_axi_wlast   => m_axi_rch_wlast,
        m_axi_wuser   => m_axi_rch_wuser,
        m_axi_wvalid  => m_axi_rch_wvalid,
        m_axi_wready  => m_axi_rch_wready,
        m_axi_bid     => m_axi_rch_bid,
        m_axi_bresp   => m_axi_rch_bresp,
        m_axi_buser   => m_axi_rch_buser,
        m_axi_bvalid  => m_axi_rch_bvalid,
        m_axi_bready  => m_axi_rch_bready,
        m_axi_arid    => m_axi_rch_arid,
        m_axi_araddr  => m_axi_rch_araddr,
        m_axi_arlen   => m_axi_rch_arlen,
        m_axi_arsize  => m_axi_rch_arsize,
        m_axi_arburst => m_axi_rch_arburst,
        m_axi_arlock  => m_axi_rch_arlock,
        m_axi_arcache => m_axi_rch_arcache,
        m_axi_arprot  => m_axi_rch_arprot,
        m_axi_arqos   => m_axi_rch_arqos,
        m_axi_aruser  => m_axi_rch_aruser,
        m_axi_arvalid => m_axi_rch_arvalid,
        m_axi_arready => m_axi_rch_arready,
        m_axi_rid     => m_axi_rch_rid,
        m_axi_rdata   => m_axi_rch_rdata,
        m_axi_rresp   => m_axi_rch_rresp,
        m_axi_rlast   => m_axi_rch_rlast,
        m_axi_ruser   => m_axi_rch_ruser,
        m_axi_rvalid  => m_axi_rch_rvalid,
        m_axi_rready  => m_axi_rch_rready
    );



    I_AXI_LITE_MASTER : entity xil_defaultlib.axi_lite_master
    generic map (
        C_M_AXIL_MASTER_TARGET_BASE_ADDR => C_M_AXIL_MASTER_TARGET_BASE_ADDR,
        C_M_AXIL_MASTER_ADDR_WIDTH       => C_M_AXIL_MASTER_ADDR_WIDTH,
        C_M_AXIL_MASTER_DATA_WIDTH       => C_M_AXIL_MASTER_DATA_WIDTH
    )
    port map (
        soft_reset        => soft_reset,
        transaction_error => transaction_error,

        -- From register interface
        write_request => write_request,
        write_data    => write_data,
        write_address => write_address,
        write_done    => write_done,

        -- From register interface
        read_request    => read_request,
        read_address    => read_address,
        read_data       => read_data,
        read_data_valid => read_data_valid,

        m_axi_aclk    => m_axi_conf_aclk,
        m_axi_aresetn => m_axi_conf_aresetn,
        m_axi_awaddr  => m_axi_conf_awaddr,
        m_axi_awprot  => m_axi_conf_awprot,
        m_axi_awvalid => m_axi_conf_awvalid,
        m_axi_awready => m_axi_conf_awready,
        m_axi_wdata   => m_axi_conf_wdata,
        m_axi_wstrb   => m_axi_conf_wstrb,
        m_axi_wvalid  => m_axi_conf_wvalid,
        m_axi_wready  => m_axi_conf_wready,
        m_axi_bresp   => m_axi_conf_bresp,
        m_axi_bvalid  => m_axi_conf_bvalid,
        m_axi_bready  => m_axi_conf_bready,
        m_axi_araddr  => m_axi_conf_araddr,
        m_axi_arprot  => m_axi_conf_arprot,
        m_axi_arvalid => m_axi_conf_arvalid,
        m_axi_arready => m_axi_conf_arready,
        m_axi_rdata   => m_axi_conf_rdata,
        m_axi_rresp   => m_axi_conf_rresp,
        m_axi_rvalid  => m_axi_conf_rvalid,
        m_axi_rready  => m_axi_conf_rready
    );


    I_CLK_DIV : entity xil_defaultlib.clk_div
    generic map (
        DIV_FACTOR => DIV_FACTOR
    )
    port map (
        clk        => clk_fs_ext,
        resetn     => resetn,
        soft_reset => soft_reset,
        clk_out    => clk_fs
    );


    I_CLK_FS_SYNC : entity xil_defaultlib.clk_sync
    port map (
        clk         => '0',
        resetn      => '0',
        soft_reset  => '0',
        clk_fs      => '0',
        clk_fs_sync => clk_fs_sync
    );

    I_PATTERN_COUNTER_L : entity xil_defaultlib.pattern_counter
    generic map (
        DATA_WIDTH => PATTERN_COUNTER_DATA_WIDTH,
        PATTERN    => PATTERN
    )
    port map (
        soft_reset    => soft_reset,
        count         => count_lch,
        pattern_count => pattern_count_lch,

        axis_clk    => s_axis_lch_aclk,
        axis_resetn => s_axis_lch_aresetn,
        axis_tvalid => s_axis_lch_tvalid,
        axis_tdata  => s_axis_lch_tdata,
        axis_tready => s_axis_lch_tready
    );


    I_PATTERN_COUNTER_R : entity xil_defaultlib.pattern_counter
    generic map (
        DATA_WIDTH => PATTERN_COUNTER_DATA_WIDTH,
        PATTERN    => PATTERN
    )
    port map (
        soft_reset    => soft_reset,
        count         => count_rch,
        pattern_count => pattern_count_rch,

        axis_clk    => s_axis_rch_aclk,
        axis_resetn => s_axis_rch_aresetn,
        axis_tvalid => s_axis_rch_tvalid,
        axis_tdata  => s_axis_rch_tdata,
        axis_tready => s_axis_rch_tready
    );


    I_CORE_FSM_L : entity xil_defaultlib.core_fsm
    port map (
        clk           => clk,
        resetn        => resetn,
        clk_fs        => clk_fs_sync,
        soft_reset    => soft_reset,
        system_enable => system_enable,

        conv_op  => conv_op_lch,
        conv_req => conv_req_lch,
        conv_rsp => conv_rsp_lch,

        pattern_req      => pattern_req_lch,
        pattern_len      => pattern_len_lch,
        pattern_finished => pattern_finished_lch,
        pattern_tlast    => pattern_tlast_lch,

        buffer_size => buffer_size_l,
        bram_ptr    => bram_ptr_l,
        read_size   => read_size_l,

        internal_error => internal_error_lch,
        system_running => system_running_lch
    );


    I_CORE_FSM_R : entity xil_defaultlib.core_fsm
    port map (
        clk           => clk,
        resetn        => resetn,
        clk_fs        => clk_fs_sync,
        soft_reset    => soft_reset,
        system_enable => system_enable,

        conv_op          => conv_op_rch,
        conv_req         => conv_req_rch,
        conv_rsp         => conv_rsp_rch,
        pattern_req      => pattern_req_rch,
        pattern_len      => pattern_len_rch,
        pattern_finished => pattern_finished_rch,
        pattern_tlast    => pattern_tlast_rch,

        buffer_size => buffer_size_r,
        bram_ptr    => bram_ptr_r,
        read_size   => read_size_r,

        internal_error => internal_error_rch,
        system_running => system_running_rch
    );



end architecture RTL;



library IEEE;
library xil_defaultlib;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity pattern_counter is
    generic (
        DATA_WIDTH : integer                                 := 32;
        PATTERN    : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0')
    );
    port (
        axis_clk    : in std_logic;
        axis_resetn : in std_logic;
        soft_reset  : in std_logic;

        axis_tvalid : in std_logic;
        axis_tdata  : in std_logic_vector(DATA_WIDTH-1 downto 0);
        axis_tready : in std_logic;

        count         : out unsigned(31 downto 0);
        pattern_count : out unsigned(31 downto 0)
    );

end entity pattern_counter;


architecture RTL of pattern_counter is
    signal count_i         : unsigned(31 downto 0);
    signal pattern_count_i : unsigned(31 downto 0);

begin

    count         <= count_i;
    pattern_count <= pattern_count_i;

    pattern_count_proc : process(axis_clk) is
    begin
        if(rising_edge(axis_clk)) then
            if(axis_resetn = '0' or soft_reset = '1') then
                count_i         <= (others => '0');
                pattern_count_i <= (others => '0');
            elsif (axis_tvalid = '1' and axis_tready = '1') then
                count_i <= count_i + '1';
                if (axis_tdata = PATTERN) then
                    pattern_count_i <= pattern_count_i + '1';
                end if;
            end if;
        end if;
    end process pattern_count_proc;


end architecture RTL;



library UNISIM;
library xil_defaultlib;
library IEEE;
use UNISIM.vcomponents.all;
use IEEE.STD_LOGIC_1164.all;
use IEEE.numeric_std.all;

entity clk_div is
    generic (
        DIV_FACTOR : integer := 8
    );
    port (
        clk        : in  std_logic;
        resetn     : in  std_logic;
        soft_reset : in  std_logic;
        clk_out    : out std_logic
    );
end entity clk_div;


architecture RTL of clk_div is
    signal cnt     : integer   := 0;
    signal clk_div : std_logic := '1';

begin

    div_proc : process(clk)
    begin
        if (rising_edge(clk)) then
            if (resetn = '0' or soft_reset = '1') then
                cnt     <= 0;
                clk_div <= '1';
            else
                cnt <= cnt+1;
                if (cnt = (DIV_FACTOR/2)-1) then
                    clk_div <= not clk_div;
                    cnt     <= 0;
                end if;
            end if;
        end if;
    end process;


    BUFG_inst : BUFG
    port map (
        O => clk_out,
        I => clk_div
    );

end RTL;



library IEEE;
library xil_defaultlib;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;


entity clk_sync is
    port (
        clk         : in  std_logic;
        resetn      : in  std_logic;
        soft_reset  : in  std_logic;
        clk_fs      : in  std_logic;
        clk_fs_sync : out std_logic
    );
end clk_sync;


architecture RTL of clk_sync is

    signal clk_fs_ff : std_logic;

begin

    clk_fs_sync_proc : process (clk) is
    begin
        if (rising_edge(clk)) then
            if (resetn = '0' or soft_reset = '1') then
                clk_fs_sync <= '0';
                clk_fs_ff   <= '0';
            else
                clk_fs_ff   <= clk_fs;
                clk_fs_sync <= clk_fs_ff;
            end if;
        end if;
    end process clk_fs_sync_proc;


end architecture RTL;


library IEEE;
library xil_defaultlib;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use xil_defaultlib.input_buffer_types.all;
use xil_defaultlib.input_buffer_logic.all;
use xil_defaultlib.global.all;

entity input_buffer is
    generic (
        C_M_AXI_BURST_LEN     : integer                       := C_M_AXI_BURST_LEN;
        LEFT_CH_BASE_ADDRESS  : std_logic_vector(31 downto 0) := LEFT_CH_ST_BASE_ADDRESS;
        RIGHT_CH_BASE_ADDRESS : std_logic_vector(31 downto 0) := RIGHT_CH_ST_BASE_ADDRESS
    );
    port (
        inputs     : in  input_buffer_inputs_t;
        outputs    : out input_buffer_outputs_t;
        soft_reset : in  std_logic;

        -- RX Data
        s_axis_lch_aclk    : in  std_logic;
        s_axis_lch_aresetn : in  std_logic;
        s_axis_lch_tdata   : in  std_logic_vector(63 downto 0);
        s_axis_lch_tvalid  : in  std_logic;
        s_axis_lch_tkeep   : in  std_logic_vector(7 downto 0);
        s_axis_lch_tlast   : in  std_logic;
        s_axis_lch_tready  : out std_logic;

        s_axis_rch_aclk    : in  std_logic;
        s_axis_rch_aresetn : in  std_logic;
        s_axis_rch_tdata   : in  std_logic_vector(63 downto 0);
        s_axis_rch_tvalid  : in  std_logic;
        s_axis_rch_tkeep   : in  std_logic_vector(7 downto 0);
        s_axis_rch_tlast   : in  std_logic;
        s_axis_rch_tready  : out std_logic;

        -- Transmit to next stage
        m_axis_lch_aclk    : in  std_logic;
        m_axis_lch_aresetn : in  std_logic;
        m_axis_lch_tdata   : out std_logic_vector(63 downto 0);
        m_axis_lch_tvalid  : out std_logic;
        m_axis_lch_tkeep   : out std_logic_vector(7 downto 0);
        m_axis_lch_tlast   : out std_logic;
        m_axis_lch_tready  : in  std_logic;

        m_axis_rch_aclk    : in  std_logic;
        m_axis_rch_aresetn : in  std_logic;
        m_axis_rch_tdata   : out std_logic_vector(63 downto 0);
        m_axis_rch_tvalid  : out std_logic;
        m_axis_rch_tkeep   : out std_logic_vector(7 downto 0);
        m_axis_rch_tlast   : out std_logic;
        m_axis_rch_tready  : in  std_logic;

        -- Error signals
        bram_overflow_error       : out std_logic;
        out_reg_underflow_error_l : out std_logic;
        out_reg_overflow_error_l  : out std_logic;
        out_reg_underflow_error_r : out std_logic;
        out_reg_overflow_error_r  : out std_logic
    );
end input_buffer;



architecture RTL of input_buffer is

    component blk_mem_gen_0
        port (
            -- BRAM Write
            clka  : in  std_logic;
            ena   : in  std_logic;
            wea   : in  std_logic_vector(0 downto 0);
            addra : in  std_logic_vector(10 downto 0);
            dina  : in  std_logic_vector(63 downto 0);
            -- BRAM Read
            clkb  : in  std_logic;
            rstb  : in  std_logic;
            enb   : in  std_logic;
            addrb : in  std_logic_vector(10 downto 0);
            doutb : out std_logic_vector(63 downto 0)
        );
    end component;

    procedure bram_logic (
        signal s_axis_tvalid   : in    std_logic;
        signal s_axis_tdata    : in    std_logic_vector(63 downto 0);
        signal read_bram_enb   : in    std_logic;
        signal bram_pointer    : inout bram_read_pointer_t;
        signal overflow_error  : out   std_logic;
        signal address_write_d : out   std_logic_vector(10 downto 0)
    ) is
    begin
        if (read_bram_enb = '1') then
            bram_pointer.head <= bram_pointer.head + to_unsigned(1, 11);
        end if;

        stream_to_bram(s_axis_tvalid, s_axis_tdata, bram_pointer, overflow_error);
        address_write_d <= std_logic_vector(bram_pointer.tail);
    end procedure bram_logic;


    procedure bram_logic_rst (
        signal bram_pointer    : out bram_read_pointer_t;
        signal overflow_error  : out std_logic;
        signal address_write_d : out std_logic_vector(10 downto 0)
    ) is
    begin
        init_bram_logic(bram_pointer);
        overflow_error  <= '0';
        address_write_d <= (others => '0');
    end procedure bram_logic_rst;


    procedure read_to_output_reg_logic (
        signal start_burst_master    : in    std_logic;
        signal wlast                 : in    std_logic;
        signal read_size             : in    unsigned(9 downto 0);
        signal idx                   : inout unsigned(10 downto 0);
        signal idx_bram              : inout unsigned(10 downto 0);
        signal bram_to_buffer        : in    std_logic_vector(63 downto 0);
        signal read_bram_enb         : inout std_logic;
        signal last_word_out_reg     : inout std_logic;
        signal load_output_reg       : out   std_logic;
        signal output_reg_out_tvalid : out   std_logic
    ) is
    begin
        if (start_burst_master = '1') then
            read_bram_enb <= '1';
        end if;

        if (read_bram_enb = '1') then
            load_output_reg       <= '1';
            output_reg_out_tvalid <= '1';
            idx                   <= idx + 1;
            idx_bram              <= idx_bram + 1;

            if (idx = read_size-1) then
                read_bram_enb     <= '0';
                last_word_out_reg <= '1';
            end if;

        elsif (last_word_out_reg = '1') then
            idx               <= (others => '0');
            last_word_out_reg <= '0';
            load_output_reg   <= '0';

        elsif (wlast = '1') then
            output_reg_out_tvalid <= '0';
        end if;
    end procedure read_to_output_reg_logic;


    procedure read_to_output_reg_logic_rst (
        signal idx                   : out unsigned(10 downto 0);
        signal idx_bram              : out unsigned(10 downto 0);
        signal read_bram_enb         : out std_logic;
        signal output_reg_out_tvalid : out std_logic;
        signal load_output_reg       : out std_logic;
        signal last_word_out_reg     : out std_logic
    ) is
    begin
        idx                   <= (others => '0');
        idx_bram              <= (others => '0');
        output_reg_out_tvalid <= '0';
        read_bram_enb         <= '0';
        load_output_reg       <= '0';
        last_word_out_reg     <= '0';
    end procedure read_to_output_reg_logic_rst;


    procedure bram_pointer_position_calc (
        signal write_done            : in    std_logic;
        signal read_size             : in    unsigned(9 downto 0);
        signal bram_pointer_position : inout std_logic_vector(31 downto 0)
    ) is
    begin
        if (write_done = '1') then
            bram_pointer_position <= std_logic_vector(unsigned(bram_pointer_position) + (resize(read_size, 32) sll 3));
        end if;
    end procedure bram_pointer_position_calc;


    procedure bram_pointer_position_rst (
        constant CH_BASE_ADDRESS     : in  std_logic_vector(31 downto 0);
        signal bram_pointer_position : out std_logic_vector(31 downto 0)
    ) is
    begin
        bram_pointer_position <= CH_BASE_ADDRESS;
    end procedure bram_pointer_position_rst;


    procedure load_output_reg (
        constant OUTPUT_REG_DEFAULT_VALUE : in  std_logic_vector(63 downto 0);
        signal output_reg_out_tvalid      : in  std_logic;
        signal idx                        : in  unsigned(10 downto 0);
        signal bram_to_buffer             : in  std_logic_vector(63 downto 0);
        signal load_output_reg            : in  std_logic;
        signal output_reg                 : out output_reg;
        signal out_reg_underflow_error    : out std_logic;
        signal out_reg_overflow_error     : out std_logic
    ) is
    begin
        if (output_reg_out_tvalid = '1') then
            if (load_output_reg = '1') then
                if (idx <= 0) then
                    out_reg_underflow_error <= '1';
                elsif (idx > 32) then
                    out_reg_overflow_error <= '1';
                else
                    output_reg(to_integer(idx-1)) <= bram_to_buffer;
                end if;
            end if;
        else
            output_reg <= (others => OUTPUT_REG_DEFAULT_VALUE);
        end if;
    end procedure load_output_reg;


    procedure load_output_reg_rst (
        constant OUTPUT_REG_DEFAULT_VALUE : in  std_logic_vector(63 downto 0);
        signal output_reg                 : out output_reg;
        signal out_reg_underflow_error    : out std_logic;
        signal out_reg_overflow_error     : out std_logic
    ) is
    begin
        output_reg              <= (others => OUTPUT_REG_DEFAULT_VALUE);
        out_reg_underflow_error <= '0';
        out_reg_overflow_error  <= '0';
    end procedure load_output_reg_rst;


    signal reset_bram_l          : std_logic;
    signal reset_bram_r          : std_logic;
    signal bram_a_addrb          : std_logic_vector(10 downto 0);
    signal bram_b_addrb          : std_logic_vector(10 downto 0);
    signal read_bram_enb_l       : std_logic;
    signal read_bram_enb_r       : std_logic;
    signal bram_pointer_l        : bram_read_pointer_t;
    signal bram_pointer_r        : bram_read_pointer_t;
    signal bram_to_buffer_l      : std_logic_vector(63 downto 0);
    signal bram_to_buffer_r      : std_logic_vector(63 downto 0);
    signal bram_overflow_error_l : std_logic;
    signal bram_overflow_error_r : std_logic;

    signal output_reg_l              : output_reg;
    signal output_reg_r              : output_reg;
    signal output_reg_out_tvalid_l   : std_logic;
    signal output_reg_out_tvalid_l_d : std_logic;
    signal output_reg_out_tvalid_r   : std_logic;
    signal output_reg_out_tvalid_r_d : std_logic;
    signal last_word_out_reg_l       : std_logic;
    signal last_word_out_reg_r       : std_logic;
    signal load_output_reg_l         : std_logic;
    signal load_output_reg_r         : std_logic;

    signal bram_ptr_pos_l    : std_logic_vector(31 downto 0);
    signal bram_ptr_pos_r    : std_logic_vector(31 downto 0);
    signal address_write_d_l : std_logic_vector(10 downto 0);
    signal address_write_d_r : std_logic_vector(10 downto 0);

    signal idx_l      : unsigned(10 downto 0);
    signal idx_l_bram : unsigned(10 downto 0);
    signal idx_r      : unsigned(10 downto 0);
    signal idx_r_bram : unsigned(10 downto 0);

    attribute keep                      : string;
    attribute keep of output_reg_l      : signal is "true";
    attribute keep of output_reg_r      : signal is "true";
    attribute ram_style                 : string;
    attribute ram_style of output_reg_l : signal is "distributed";
    attribute ram_style of output_reg_r : signal is "distributed";

begin

    reset_bram_l <= not s_axis_lch_aresetn;
    reset_bram_r <= not s_axis_rch_aresetn;

    input_buffer_l : blk_mem_gen_0
    port map (
        clka  => s_axis_lch_aclk,
        ena   => bram_pointer_l.ena,
        wea   => bram_pointer_l.wea,
        addra => address_write_d_l,
        dina  => bram_pointer_l.dina,

        clkb  => s_axis_lch_aclk,
        rstb  => reset_bram_l,
        enb   => read_bram_enb_l,
        addrb => bram_a_addrb,
        doutb => bram_to_buffer_l
    );


    input_buffer_r : blk_mem_gen_0
    port map (
        clka  => s_axis_rch_aclk,
        ena   => bram_pointer_r.ena,
        wea   => bram_pointer_r.wea,
        addra => address_write_d_r,
        dina  => bram_pointer_r.dina,

        clkb  => s_axis_rch_aclk,
        rstb  => reset_bram_r,
        enb   => read_bram_enb_r,
        addrb => bram_b_addrb,
        doutb => bram_to_buffer_r
    );


    ----------------
    -- COMB LOGIC --
    ----------------
    outputs.buffer_size_l <= bram_pointer_l.tail - bram_pointer_l.head;
    outputs.bram_ptr_l    <= bram_ptr_pos_l;
    m_axis_lch_tdata      <= output_reg_l(to_integer(unsigned(inputs.bw_counter_l)));
    m_axis_lch_tvalid     <= output_reg_out_tvalid_l and output_reg_out_tvalid_l_d;

    outputs.buffer_size_r <= bram_pointer_r.tail - bram_pointer_r.head;
    outputs.bram_ptr_r    <= bram_ptr_pos_r;
    m_axis_rch_tdata      <= output_reg_r(to_integer(unsigned(inputs.bw_counter_r)));
    m_axis_rch_tvalid     <= output_reg_out_tvalid_r and output_reg_out_tvalid_r_d;

    -- BRAMs Address read/write management
    bram_a_addrb <= std_logic_vector(idx_l_bram);
    bram_b_addrb <= std_logic_vector(idx_r_bram);

    -- Overflow management
    bram_overflow_error <= bram_overflow_error_l or bram_overflow_error_r;


    --------------------
    -- Undriven Signals --
    --------------------
    s_axis_lch_tready <= '1';
    s_axis_rch_tready <= '1';
    m_axis_lch_tkeep  <= (others => '0');
    m_axis_lch_tlast  <= '0';
    m_axis_rch_tkeep  <= (others => '0');
    m_axis_rch_tlast  <= '0';

    ---------------
    -- SEQ LOGIC --
    ---------------
    axi_bram_logic_l : process(s_axis_lch_aclk)
    begin
        if (rising_edge(s_axis_lch_aclk)) then
            if (s_axis_lch_aresetn = '0' or soft_reset = '1') then
                bram_logic_rst(
                    bram_pointer    => bram_pointer_l,
                    overflow_error  => bram_overflow_error_l,
                    address_write_d => address_write_d_l
                );
            else
                bram_logic(
                    s_axis_tvalid   => s_axis_lch_tvalid,
                    s_axis_tdata    => s_axis_lch_tdata,
                    read_bram_enb   => read_bram_enb_l,
                    bram_pointer    => bram_pointer_l,
                    overflow_error  => bram_overflow_error_l,
                    address_write_d => address_write_d_l
                );
            end if;
        end if;
    end process axi_bram_logic_l;


    axi_bram_logic_r : process(s_axis_rch_aclk)
    begin
        if (rising_edge(s_axis_rch_aclk)) then
            if (s_axis_rch_aresetn = '0' or soft_reset = '1') then
                bram_logic_rst(
                    bram_pointer    => bram_pointer_r,
                    overflow_error  => bram_overflow_error_r,
                    address_write_d => address_write_d_r
                );
            else
                bram_logic(
                    s_axis_tvalid   => s_axis_rch_tvalid,
                    s_axis_tdata    => s_axis_rch_tdata,
                    read_bram_enb   => read_bram_enb_r,
                    bram_pointer    => bram_pointer_r,
                    overflow_error  => bram_overflow_error_r,
                    address_write_d => address_write_d_r
                );
            end if;
        end if;
    end process axi_bram_logic_r;


    read_to_output_reg_l : process (s_axis_lch_aclk) is
    begin
        if (rising_edge(s_axis_lch_aclk)) then
            if (s_axis_lch_aresetn = '0' or soft_reset = '1') then
                read_to_output_reg_logic_rst(
                    idx                   => idx_l,
                    idx_bram              => idx_l_bram,
                    read_bram_enb         => read_bram_enb_l,
                    output_reg_out_tvalid => output_reg_out_tvalid_l,
                    load_output_reg       => load_output_reg_l,
                    last_word_out_reg     => last_word_out_reg_l
                );
            else
                read_to_output_reg_logic(
                    start_burst_master    => inputs.start_burst_master_l,
                    wlast                 => inputs.wlast_l,
                    read_size             => inputs.send_size_l,
                    idx                   => idx_l,
                    idx_bram              => idx_l_bram,
                    bram_to_buffer        => bram_to_buffer_l,
                    read_bram_enb         => read_bram_enb_l,
                    last_word_out_reg     => last_word_out_reg_l,
                    load_output_reg       => load_output_reg_l,
                    output_reg_out_tvalid => output_reg_out_tvalid_l
                );
            end if;
        end if;
    end process read_to_output_reg_l;


    read_to_output_reg_r : process (s_axis_rch_aclk) is
    begin
        if (rising_edge(s_axis_rch_aclk)) then
            if (s_axis_rch_aresetn = '0' or soft_reset = '1') then
                read_to_output_reg_logic_rst(
                    idx                   => idx_r,
                    idx_bram              => idx_r_bram,
                    read_bram_enb         => read_bram_enb_r,
                    output_reg_out_tvalid => output_reg_out_tvalid_r,
                    load_output_reg       => load_output_reg_r,
                    last_word_out_reg     => last_word_out_reg_r
                );
            else
                read_to_output_reg_logic(
                    start_burst_master    => inputs.start_burst_master_r,
                    wlast                 => inputs.wlast_r,
                    read_size             => inputs.send_size_r,
                    idx                   => idx_r,
                    idx_bram              => idx_r_bram,
                    bram_to_buffer        => bram_to_buffer_r,
                    read_bram_enb         => read_bram_enb_r,
                    last_word_out_reg     => last_word_out_reg_r,
                    load_output_reg       => load_output_reg_r,
                    output_reg_out_tvalid => output_reg_out_tvalid_r
                );
            end if;
        end if;
    end process read_to_output_reg_r;


    bram_ptr_pos_l_proc : process (s_axis_lch_aclk) is
    begin
        if (rising_edge(s_axis_lch_aclk)) then
            if (s_axis_lch_aresetn = '0' or soft_reset = '1') then
                bram_pointer_position_rst(
                    CH_BASE_ADDRESS       => LEFT_CH_BASE_ADDRESS,
                    bram_pointer_position => bram_ptr_pos_l
                );
            else
                bram_pointer_position_calc(
                    write_done            => inputs.write_done_l,
                    read_size             => inputs.send_size_l,
                    bram_pointer_position => bram_ptr_pos_l
                );
            end if;
        end if;
    end process bram_ptr_pos_l_proc;


    bram_ptr_pos_r_proc : process (s_axis_rch_aclk) is
    begin
        if (rising_edge(s_axis_rch_aclk)) then
            if (s_axis_rch_aresetn = '0' or soft_reset = '1') then
                bram_pointer_position_rst(
                    CH_BASE_ADDRESS       => RIGHT_CH_BASE_ADDRESS,
                    bram_pointer_position => bram_ptr_pos_r
                );
            else
                bram_pointer_position_calc(
                    write_done            => inputs.write_done_r,
                    read_size             => inputs.send_size_r,
                    bram_pointer_position => bram_ptr_pos_r
                );
            end if;
        end if;
    end process bram_ptr_pos_r_proc;


    output_reg_loading_l : process (s_axis_lch_aclk) is
    begin
        if (rising_edge(s_axis_lch_aclk)) then
            if (s_axis_lch_aresetn = '0' or soft_reset = '1') then
                load_output_reg_rst(
                    OUTPUT_REG_DEFAULT_VALUE => OUTPUT_REG_DEFAULT_VALUE,
                    output_reg               => output_reg_l,
                    out_reg_underflow_error  => out_reg_underflow_error_l,
                    out_reg_overflow_error   => out_reg_overflow_error_l
                );
            else
                load_output_reg(
                    OUTPUT_REG_DEFAULT_VALUE => OUTPUT_REG_DEFAULT_VALUE,
                    output_reg_out_tvalid    => output_reg_out_tvalid_l,
                    idx                      => idx_l,
                    bram_to_buffer           => bram_to_buffer_l,
                    load_output_reg          => load_output_reg_l,
                    output_reg               => output_reg_l,
                    out_reg_underflow_error  => out_reg_underflow_error_l,
                    out_reg_overflow_error   => out_reg_overflow_error_l
                );
            end if;
        end if;
    end process output_reg_loading_l;



    output_reg_loading_r : process (s_axis_rch_aclk) is
    begin
        if (rising_edge(s_axis_rch_aclk)) then
            if (s_axis_rch_aresetn = '0' or soft_reset = '1') then
                load_output_reg_rst(
                    OUTPUT_REG_DEFAULT_VALUE => OUTPUT_REG_DEFAULT_VALUE,
                    output_reg               => output_reg_r,
                    out_reg_underflow_error  => out_reg_underflow_error_r,
                    out_reg_overflow_error   => out_reg_overflow_error_r
                );
            else
                load_output_reg(
                    OUTPUT_REG_DEFAULT_VALUE => OUTPUT_REG_DEFAULT_VALUE,
                    output_reg_out_tvalid    => output_reg_out_tvalid_r,
                    idx                      => idx_r,
                    bram_to_buffer           => bram_to_buffer_r,
                    load_output_reg          => load_output_reg_r,
                    output_reg               => output_reg_r,
                    out_reg_underflow_error  => out_reg_underflow_error_r,
                    out_reg_overflow_error   => out_reg_overflow_error_r
                );
            end if;
        end if;
    end process output_reg_loading_r;


    delay_output_reg_valid_l : process (s_axis_lch_aclk) is
    begin
        if (rising_edge(s_axis_lch_aclk)) then
            if (s_axis_lch_aresetn = '0' or soft_reset = '1') then
                output_reg_out_tvalid_l_d <= '0';
            else
                output_reg_out_tvalid_l_d <= output_reg_out_tvalid_l;
            end if;
        end if;
    end process delay_output_reg_valid_l;


    delay_output_reg_valid_r : process (s_axis_rch_aclk) is
    begin
        if (rising_edge(s_axis_rch_aclk)) then
            if (s_axis_rch_aresetn = '0' or soft_reset = '1') then
                output_reg_out_tvalid_r_d <= '0';
            else
                output_reg_out_tvalid_r_d <= output_reg_out_tvalid_r;
            end if;
        end if;
    end process delay_output_reg_valid_r;


end RTL;


library xil_defaultlib;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use xil_defaultlib.global.all;

entity core_fsm is
    port (
        clk            : in  std_logic;
        resetn         : in  std_logic;
        clk_fs         : in  std_logic;
        soft_reset     : in  std_logic;
        system_enable  : in  std_logic;
        system_running : out std_logic;

        conv_op        : in  std_logic;
        conv_req       : out conversion_req_t;
        conv_rsp       : in  conversion_rsp_t;
        internal_error : in  std_logic;

        pattern_req      : out std_logic;
        pattern_len      : out unsigned(9 downto 0);
        pattern_finished : in  std_logic;
        pattern_tlast    : in  std_logic;

        buffer_size : in unsigned(10 downto 0);
        bram_ptr    : in std_logic_vector(31 downto 0);
        read_size   : in unsigned(15 downto 0)
    );

end core_fsm;

architecture RTL of core_fsm is

    type fsm_states is (
    IDLE,
    REQ_S2MM,
    WAIT_S2MM,
    REQ_MM2S,
    WAIT_MM2S
    );
    signal state : fsm_states;

    signal s2mm_write_ptr      : std_logic_vector(31 downto 0);
    signal mm2s_read_ptr       : std_logic_vector(31 downto 0);
    signal s2mm_write_req_size : unsigned(9 downto 0);
    signal mm2s_read_req_size  : unsigned(9 downto 0);

    constant S2MM_WRITE_SIZE : natural := 32;
    constant MM2S_READ_SIZE  : natural := 32;

begin


    fsm_axi_full : process (clk)
    begin
        if (rising_edge(clk)) then
            if (resetn = '0' or soft_reset = '1') then
                state          <= IDLE;
                system_running <= '0';

                conv_req            <= (request => '0', op_type => S2MM, size => (others => '0'), address => (others => '0'));
                s2mm_write_ptr      <= (others  => '0');
                mm2s_read_ptr       <= (others  => '0');
                s2mm_write_req_size <= (others  => '0');
                mm2s_read_req_size  <= (others  => '0');

            else
                -- Default outputs
                conv_req.request <= '0';
                conv_req.size    <= (others => '0');
                conv_req.address <= (others => '0');
                system_running   <= '1';

                -- FSM
                case state is
                    when IDLE =>
                        system_running <= '0';
                        if (system_enable) then
                            if (conv_op = '0' and buffer_size >= S2MM_WRITE_SIZE) then
                                state <= REQ_S2MM;
                            elsif (conv_op = '1' and mm2s_read_ptr(15 downto 0) < std_logic_vector(read_size)) then
                                state <= REQ_MM2S;
                            end if;
                        end if;


                    when REQ_S2MM =>
                        if (buffer_size >= S2MM_WRITE_SIZE) then
                            conv_req.op_type <= S2MM;
                            conv_req.request <= '1';
                            conv_req.size    <= to_unsigned(S2MM_WRITE_SIZE, 10);
                            conv_req.address <= s2mm_write_ptr;

                            s2mm_write_req_size <= to_unsigned(S2MM_WRITE_SIZE, 10);
                            state               <= WAIT_S2MM;
                        end if;


                    when WAIT_S2MM =>
                        if (conv_rsp.s2mm_done) then
                            s2mm_write_ptr <= std_logic_vector(unsigned(s2mm_write_ptr) + s2mm_write_req_size);
                            state          <= IDLE;
                        end if;


                    when REQ_MM2S =>
                        conv_req.op_type <= MM2S;
                        conv_req.request <= '1';
                        conv_req.size    <= read_size(9 downto 0);
                        conv_req.address <= mm2s_read_ptr;

                        mm2s_read_req_size <= read_size(9 downto 0);
                        state              <= WAIT_MM2S;


                    when WAIT_MM2S =>
                        if (conv_rsp.mm2s_done) then
                            mm2s_read_ptr <= std_logic_vector(unsigned(mm2s_read_ptr) + mm2s_read_req_size);
                            state         <= IDLE;
                        end if;

                end case;

                if (not system_enable) then
                    state <= IDLE;
                end if;

            end if;
        end if;
    end process fsm_axi_full;


end RTL;


library ieee;
library xil_defaultlib;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use xil_defaultlib.global.all;

entity core_converter is
    generic (
        C_M_AXI_BURST_LEN    : integer := 256;
        C_M_AXI_ID_WIDTH     : integer := 1;
        C_M_AXI_ADDR_WIDTH   : integer := 32;
        C_M_AXI_DATA_WIDTH   : integer := 64;
        C_M_AXI_AWUSER_WIDTH : integer := 0;
        C_M_AXI_ARUSER_WIDTH : integer := 0;
        C_M_AXI_WUSER_WIDTH  : integer := 0;
        C_M_AXI_RUSER_WIDTH  : integer := 0;
        C_M_AXI_BUSER_WIDTH  : integer := 0
    );
    port (
        soft_reset : in  std_logic;
        conv_req   : in  conversion_req_t;
        conv_rsp   : out conversion_rsp_t;

        fb_wr_burst_start : out std_logic;
        fb_bw_counter     : out std_logic_vector(7 downto 0);
        fb_wlast          : out std_logic;
        fb_reduced_burst  : out std_logic;
        fb_awlen          : out std_logic_vector(7 downto 0);
        fb_burst_done     : out std_logic;

        pattern_req      : in  std_logic            := '0';
        pattern_len      : in  unsigned(9 downto 0) := (others => '0');
        pattern_finished : out std_logic;
        pattern_tlast    : in  std_logic            := '0';

        s_axis_aclk    : in  std_logic;
        s_axis_aresetn : in  std_logic;
        s_axis_tready  : out std_logic;
        s_axis_tdata   : in  std_logic_vector(63 downto 0) := (others => '0');
        s_axis_tvalid  : in  std_logic                     := '0';
        s_axis_tkeep   : in  std_logic_vector(7 downto 0)  := (others => '0');
        s_axis_tlast   : in  std_logic                     := '0';

        m_axis_aclk    : in  std_logic := '0';
        m_axis_aresetn : in  std_logic := '0';
        m_axis_tdata   : out std_logic_vector(63 downto 0);
        m_axis_tvalid  : out std_logic;
        m_axis_tkeep   : out std_logic_vector(7 downto 0);
        m_axis_tlast   : out std_logic;
        m_axis_tready  : in  std_logic := '0';
        m_axis_tdest   : out std_logic;

        m_axi_aclk    : in  std_logic;
        m_axi_aresetn : in  std_logic;
        m_axi_awid    : out std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
        m_axi_awaddr  : out std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
        m_axi_awlen   : out std_logic_vector(7 downto 0);
        m_axi_awsize  : out std_logic_vector(2 downto 0);
        m_axi_awburst : out std_logic_vector(1 downto 0);
        m_axi_awlock  : out std_logic;
        m_axi_awcache : out std_logic_vector(3 downto 0);
        m_axi_awprot  : out std_logic_vector(2 downto 0);
        m_axi_awqos   : out std_logic_vector(3 downto 0);
        m_axi_awuser  : out std_logic_vector(C_M_AXI_AWUSER_WIDTH-1 downto 0);
        m_axi_awvalid : out std_logic;
        m_axi_awready : in  std_logic;
        m_axi_wdata   : out std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
        m_axi_wstrb   : out std_logic_vector(C_M_AXI_DATA_WIDTH/8-1 downto 0);
        m_axi_wlast   : out std_logic;
        m_axi_wuser   : out std_logic_vector(C_M_AXI_WUSER_WIDTH-1 downto 0);
        m_axi_wvalid  : out std_logic;
        m_axi_wready  : in  std_logic;
        m_axi_bid     : in  std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
        m_axi_bresp   : in  std_logic_vector(1 downto 0);
        m_axi_buser   : in  std_logic_vector(C_M_AXI_BUSER_WIDTH-1 downto 0);
        m_axi_bvalid  : in  std_logic;
        m_axi_bready  : out std_logic;
        m_axi_arid    : out std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
        m_axi_araddr  : out std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
        m_axi_arlen   : out std_logic_vector(7 downto 0);
        m_axi_arsize  : out std_logic_vector(2 downto 0);
        m_axi_arburst : out std_logic_vector(1 downto 0);
        m_axi_arlock  : out std_logic;
        m_axi_arcache : out std_logic_vector(3 downto 0);
        m_axi_arprot  : out std_logic_vector(2 downto 0);
        m_axi_arqos   : out std_logic_vector(3 downto 0);
        m_axi_aruser  : out std_logic_vector(C_M_AXI_ARUSER_WIDTH-1 downto 0);
        m_axi_arvalid : out std_logic;
        m_axi_arready : in  std_logic;
        m_axi_rid     : in  std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
        m_axi_rdata   : in  std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
        m_axi_rresp   : in  std_logic_vector(1 downto 0);
        m_axi_rlast   : in  std_logic;
        m_axi_ruser   : in  std_logic_vector(C_M_AXI_RUSER_WIDTH-1 downto 0);
        m_axi_rvalid  : in  std_logic;
        m_axi_rready  : out std_logic;

        internal_error : out std_logic
    );

end core_converter;



architecture RTL of core_converter is

    function clogb2 (bit_depth : integer) return integer is
        variable depth : integer := bit_depth;
        variable count : integer := 1;
    begin
        for clogb2 in 1 to bit_depth loop
            if (bit_depth <= 2) then
                count := 1;
            else
                if (depth <= 1) then
                    count := count;
                else
                    depth := depth / 2;
                    count := count + 1;
                end if;
            end if;
        end loop;
        return (count);
    end;


    type fsm_state is (
    IDLE,
    WRITE_BURST_SIZE_CALC,
    WRITE_INITIATE,
    WRITING_TO_MEM,
    READ_BURST_SIZE_CALC,
    READ_INITIATE,
    READING_FROM_MEM
    );

    signal state : fsm_state;

    signal axi_awlen   : std_logic_vector(7 downto 0);
    signal axi_awvalid : std_logic;
    signal axi_wdata   : std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
    signal axi_wlast   : std_logic;
    signal axi_wlast_i : std_logic;
    signal axi_wvalid  : std_logic;
    signal axi_bready  : std_logic;
    signal axi_arlen   : std_logic_vector(7 downto 0);
    signal axi_arvalid : std_logic;
    signal axi_rready  : std_logic;

    signal req_d     : std_logic;
    signal req_dd    : std_logic;
    signal req_pulse : std_logic;

    signal rd_burst_size_calc_start  : std_logic;
    signal rd_burst_size_calc_active : std_logic;
    signal rd_burst_size_calc_done   : std_logic;
    signal rd_burst_start            : std_logic;
    signal rd_burst_start_active     : std_logic;
    signal rd_burst_start_done       : std_logic;
    signal base_wr_addr              : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    signal burst_wr_addr             : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    signal burst_wr_addr_end         : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    signal transaction_wr_size       : std_logic_vector(9 downto 0);
    signal read_start                : std_logic;
    signal read_done                 : std_logic;
    signal burst_write_counter       : std_logic_vector(7 downto 0);
    signal transaction_wr_counter    : std_logic_vector(11 downto 0);
    signal wr_burst_size             : std_logic_vector(7 downto 0);
    signal wr_short_burst_4kb        : std_logic;
    signal strobe_burst              : std_logic;
    signal strobe_len                : std_logic_vector(7 downto 0);

    signal wr_burst_size_calc_start  : std_logic;
    signal wr_burst_size_calc_active : std_logic;
    signal wr_burst_size_calc_done   : std_logic;
    signal wr_burst_start            : std_logic;
    signal wr_burst_start_active     : std_logic;
    signal wr_burst_start_done       : std_logic;
    signal base_rd_addr              : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    signal burst_rd_addr             : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    signal burst_rd_addr_end         : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    signal transaction_rd_size       : std_logic_vector(9 downto 0);
    signal write_start               : std_logic;
    signal write_done                : std_logic;
    signal burst_read_counter        : std_logic_vector(7 downto 0);
    signal transaction_rd_counter    : std_logic_vector(11 downto 0);
    signal rd_burst_size             : std_logic_vector(7 downto 0);

    signal pattern_finished_i : std_logic;
    signal pattern_cnt        : unsigned(9 downto 0);


begin

    -- Write fixed signals
    m_axi_awid    <= (others => '0');
    m_axi_awburst <= "01";
    m_axi_awlock  <= '0';
    m_axi_awcache <= "0010";
    m_axi_awprot  <= "000";
    m_axi_awqos   <= x"0";
    m_axi_wuser   <= (others => '0');
    -- Read fixed signals
    m_axi_arid    <= (others => '0');
    m_axi_aruser  <= (others => '1');
    m_axi_arburst <= "01";
    m_axi_arlock  <= '0';
    m_axi_arcache <= "0010";
    m_axi_arprot  <= "000";
    m_axi_arqos   <= x"0";
    -- Read/write logic
    m_axi_awaddr  <= burst_wr_addr;
    m_axi_awlen   <= std_logic_vector(unsigned(axi_awlen) -1);
    m_axi_awsize  <= std_logic_vector(to_unsigned(clogb2((C_M_AXI_DATA_WIDTH/8)-1), 3));
    m_axi_awuser  <= (others => '1');
    m_axi_awvalid <= axi_awvalid;
    m_axi_wdata   <= axi_wdata;
    m_axi_wlast   <= axi_wlast;
    m_axi_wvalid  <= axi_wvalid;
    m_axi_bready  <= axi_bready;
    m_axi_araddr  <= burst_rd_addr;
    m_axi_arlen   <= std_logic_vector(unsigned(axi_arlen) -1);
    m_axi_arsize  <= std_logic_vector(to_unsigned(clogb2((C_M_AXI_DATA_WIDTH/8)-1), 3));
    m_axi_arvalid <= axi_arvalid;
    m_axi_rready  <= axi_rready;

    ----------------------------
    -- Master stream interface -
    ----------------------------
    m_axis_tdata <= (others => '0') when pattern_req = '1' else
                                                           m_axi_rdata when (axi_rready = '1' and m_axi_rvalid = '1' and (unsigned(transaction_rd_size)-1) >= unsigned(transaction_rd_counter)) else
                                                                                                                                                                                                (others => '0');

    m_axis_tvalid <= '1' when (pattern_req = '1' and pattern_finished_i = '0') else
                                                                               m_axi_rvalid when (axi_rready = '1' and (unsigned(transaction_rd_size)-1) >= unsigned(transaction_rd_counter)) else
                                                                                                                                                                                              '0';

    m_axis_tlast <= '1' when (pattern_tlast = '1' and m_axi_rvalid = '1' and axi_rready = '1' and (unsigned(transaction_rd_size)-1) = unsigned(transaction_rd_counter)) else
                                                                                                                                                                        '0';

    axi_rready <=
    m_axis_tready when (read_start = '1' and (unsigned(transaction_rd_size)-1) >= unsigned(transaction_rd_counter)) else
                                                                                                                    '1'           when (read_start = '1' and (unsigned(burst_read_counter) <= C_M_AXI_BURST_LEN-1)) else
                                                                                                                                                                                                                    '0';

    m_axis_tdest <= '0';
    m_axis_tkeep <= x"FF";


    ----------------------------
    -- Slave stream interface --
    ----------------------------
    axi_wdata <= s_axis_tdata when ((axi_wvalid = '1') and ((unsigned(transaction_wr_size)-1) >= unsigned(transaction_wr_counter))) else
                                                                                                                                    (others => '0');

    axi_wvalid <=
    s_axis_tvalid when ((m_axi_wready = '1') and ((unsigned(transaction_wr_size)-1) >= unsigned(transaction_wr_counter))) else
                                                                                                                          '1'           when ((write_start = '1') and (unsigned(burst_write_counter) <= C_M_AXI_BURST_LEN-1)) else
                                                                                                                                                                                                                              '0';

    s_axis_tready <= m_axi_wready when ((m_axi_wready = '1') and ((unsigned(transaction_wr_size)-1) >= unsigned(transaction_wr_counter))) else
                                                                                                                                          '0';


    -- Feedback to input buffer
    fb_wr_burst_start <= wr_burst_start;
    fb_bw_counter     <= burst_write_counter;
    fb_wlast          <= axi_wlast;
    fb_reduced_burst  <= (wr_short_burst_4kb or strobe_burst);
    fb_awlen          <= strobe_len when (strobe_burst = '1')     else axi_awlen;
    fb_burst_done     <= write_done when (state = WRITING_TO_MEM) else '0';

    -- Other signals
    pattern_finished <= pattern_finished_i;
    internal_error   <= (axi_rready and m_axi_rvalid and m_axi_rresp(1)) or
                        (axi_bready and m_axi_bvalid and m_axi_bresp(1));

    -- Internal signals comb logic
    axi_wlast <= (axi_wlast_i) and (m_axi_wready);
    req_pulse <= '1' when req_dd = '0' and req_d = '1' else '0';


    ---------
    -- FSM --
    ---------
    fsm_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                state                    <= IDLE;
                conv_rsp.s2mm_done       <= '0';
                conv_rsp.mm2s_done       <= '0';
                wr_burst_size_calc_start <= '0';
                rd_burst_size_calc_start <= '0';
                base_wr_addr             <= (others => '0');
                transaction_wr_size      <= (others => '0');
                transaction_rd_size      <= (others => '0');
                wr_burst_start           <= '0';
                write_start              <= '0';
                base_rd_addr             <= (others => '0');
                rd_burst_start           <= '0';
                read_start               <= '0';
            else
                case (state) is
                    when IDLE =>
                        write_start        <= '0';
                        wr_burst_start     <= '0';
                        conv_rsp.s2mm_done <= '0';
                        conv_rsp.mm2s_done <= '0';

                        if (req_pulse = '1') then
                            if (conv_req.op_type = S2MM) then
                                state               <= WRITE_BURST_SIZE_CALC;
                                base_wr_addr        <= conv_req.address;
                                transaction_wr_size <= std_logic_vector(conv_req.size);
                            else
                                state               <= READ_BURST_SIZE_CALC;
                                base_rd_addr        <= conv_req.address;
                                transaction_rd_size <= std_logic_vector(conv_req.size);
                            end if;
                        end if;


                    when WRITE_BURST_SIZE_CALC =>
                        if (wr_burst_size_calc_done = '0' and wr_burst_size_calc_active = '0' and wr_burst_size_calc_start = '0') then
                            wr_burst_size_calc_start <= '1';
                        else
                            wr_burst_size_calc_start <= '0';
                        end if;
                        if (wr_burst_size_calc_done = '1')then
                            state <= WRITE_INITIATE;
                        end if;


                    when WRITE_INITIATE =>
                        if (wr_burst_start_done = '0' and wr_burst_start_active = '0' and wr_burst_start = '0') then
                            wr_burst_start <= '1';
                        else
                            wr_burst_start <= '0';
                        end if;

                        if (wr_burst_start_done = '1') then
                            state <= WRITING_TO_MEM;
                        end if;


                    when WRITING_TO_MEM =>
                        if (write_done = '0') then
                            write_start <= '1';
                        else
                            write_start <= '0';
                        end if;

                        if (write_done = '1') then
                            if (unsigned(transaction_wr_counter) >= unsigned(transaction_wr_size)) then
                                conv_rsp.s2mm_done <= '1';
                                state              <= IDLE;
                            else
                                state              <= WRITE_BURST_SIZE_CALC;
                                conv_rsp.s2mm_done <= '0';
                            end if;
                        else
                            conv_rsp.s2mm_done <= '0';
                        end if;


                    when READ_BURST_SIZE_CALC =>
                        if (rd_burst_size_calc_done = '0' and rd_burst_size_calc_active = '0' and rd_burst_size_calc_start = '0') then
                            rd_burst_size_calc_start <= '1';
                        else
                            rd_burst_size_calc_start <= '0';
                        end if;

                        if (rd_burst_size_calc_done = '1')then
                            state <= READ_INITIATE;
                        end if;


                    when READ_INITIATE =>
                        if (rd_burst_start_done = '0' and rd_burst_start_active = '0' and rd_burst_start = '0') then
                            rd_burst_start <= '1';
                        else
                            rd_burst_start <= '0';
                        end if;

                        if (rd_burst_start_done = '1') then
                            state <= READING_FROM_MEM;
                        end if;


                    when READING_FROM_MEM =>
                        if (read_done = '0') then
                            read_start <= '1';
                        else
                            read_start <= '0';
                        end if;

                        if (read_done = '1') then
                            if (unsigned(transaction_rd_counter) >= unsigned(transaction_rd_size)) then
                                conv_rsp.mm2s_done <= '1';
                                state              <= IDLE;
                            else
                                state <= READ_BURST_SIZE_CALC;
                            end if;
                        else
                            conv_rsp.mm2s_done <= '0';
                        end if;

                end case;
            end if;
        end if;
    end process fsm_proc;


    read_burst_size_calc_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                rd_burst_size_calc_active <= '0';
                rd_burst_size_calc_done   <= '0';
                rd_burst_size             <= (others => '0');
            elsif (state /= READ_BURST_SIZE_CALC) then
                rd_burst_size_calc_active <= '0';
                rd_burst_size_calc_done   <= '0';
            else
                if (rd_burst_size_calc_start = '1') then
                    rd_burst_size_calc_active <= '1';
                    burst_rd_addr_end         <= std_logic_vector((C_M_AXI_DATA_WIDTH/8)*to_unsigned(C_M_AXI_BURST_LEN, 8) + unsigned(burst_rd_addr));
                    rd_burst_size             <= std_logic_vector(to_unsigned(C_M_AXI_BURST_LEN, 8));
                    rd_burst_size_calc_done   <= '0';
                elsif (rd_burst_size_calc_active = '1') then
                    if ((burst_rd_addr_end(12) /= burst_rd_addr(12)) and
                        (burst_rd_addr_end(11 downto 0) /= std_logic_vector(to_unsigned(0, 12)))) then
                        rd_burst_size           <= (rd_burst_size srl 1);
                        burst_rd_addr_end       <= std_logic_vector((C_M_AXI_DATA_WIDTH/8)*unsigned(rd_burst_size srl 1) + unsigned(burst_rd_addr));
                        rd_burst_size_calc_done <= '0';
                    else
                        rd_burst_size_calc_active <= '0';
                        rd_burst_size_calc_done   <= '1';
                    end if;
                else
                    rd_burst_size_calc_active <= '0';
                    rd_burst_size_calc_done   <= '0';
                end if;
            end if;
        end if;
    end process read_burst_size_calc_proc;



    araddr_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                burst_rd_addr <= (others => '0');
            elsif (state = IDLE and req_pulse = '1' and conv_req.op_type = MM2S) then
                burst_rd_addr <= conv_req.address;
            elsif (read_done = '1' and state = READING_FROM_MEM) then
                burst_rd_addr <= std_logic_vector(unsigned(burst_rd_addr) + unsigned(rd_burst_size)*(C_M_AXI_DATA_WIDTH/8));
            end if;
        end if;
    end process araddr_proc;


    arvalid_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or state /= READ_INITIATE or soft_reset = '1') then
                rd_burst_start_done   <= '0';
                rd_burst_start_active <= '0';
                axi_arvalid           <= '0';
            else
                if (rd_burst_start = '1') then
                    rd_burst_start_active <= '1';
                end if;
                if (rd_burst_start_active = '1') then
                    axi_arvalid <= '1';
                    if (m_axi_arready = '1') then
                        rd_burst_start_active <= '0';
                        rd_burst_start_done   <= '1';
                    end if;
                end if;
                if (rd_burst_start_done = '1') then
                    axi_arvalid <= '0';
                end if;
            end if;
        end if;
    end process arvalid_proc;


    arlen_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                axi_arlen <= (others => '0');
            elsif (rd_burst_start = '1') then
                axi_arlen <= rd_burst_size;
            end if;
        end if;
    end process arlen_proc;


    rdata_counter_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or state /= READING_FROM_MEM or soft_reset = '1') then
                burst_read_counter <= (others => '0');
            elsif (axi_rready = '1' and m_axi_rvalid = '1') then
                burst_read_counter <= std_logic_vector(unsigned(burst_read_counter) + 1);
            end if;
        end if;
    end process rdata_counter_proc;


    rdata_total_counter_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                transaction_rd_counter <= (others => '0');
            else
                if (m_axi_rvalid = '1' and axi_rready = '1') then
                    transaction_rd_counter <= std_logic_vector(unsigned(transaction_rd_counter) + 1);
                end if;

                if (state = IDLE) then
                    transaction_rd_counter <= (others => '0');
                end if;
            end if;
        end if;
    end process rdata_total_counter_proc;


    r_done_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or state /= READING_FROM_MEM or soft_reset = '1') then
                read_done <= '0';
            elsif (burst_read_counter = std_logic_vector(unsigned(rd_burst_size)-1) and m_axi_rvalid = '1' and axi_rready = '1') then
                read_done <= '1';
            end if;
        end if;
    end process r_done_proc;


    write_burst_size_calc_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                wr_burst_size_calc_active <= '0';
                wr_burst_size_calc_done   <= '0';
                wr_burst_size             <= (others => '0');
                wr_short_burst_4kb        <= '0';
            elsif (state = IDLE) then
                wr_short_burst_4kb <= '0';
            elsif (state /= WRITE_BURST_SIZE_CALC) then
                wr_burst_size_calc_active <= '0';
                wr_burst_size_calc_done   <= '0';
            else
                if (wr_burst_size_calc_start = '1') then
                    wr_burst_size_calc_active <= '1';
                    wr_burst_size_calc_done   <= '0';
                    burst_wr_addr_end         <= std_logic_vector((C_M_AXI_DATA_WIDTH/8)*to_unsigned(C_M_AXI_BURST_LEN, 8) + unsigned(burst_wr_addr));
                    wr_burst_size             <= std_logic_vector (to_unsigned(C_M_AXI_BURST_LEN, 8));
                    wr_short_burst_4kb        <= '0';
                elsif (wr_burst_size_calc_active = '1') then
                    if (burst_wr_addr_end(12) /= burst_wr_addr(12) and
                        (burst_wr_addr_end(11 downto 0) /= std_logic_vector(to_unsigned(0, 12)))) then
                        wr_short_burst_4kb      <= '1';
                        wr_burst_size           <= (wr_burst_size srl 1);
                        burst_wr_addr_end       <= std_logic_vector((C_M_AXI_DATA_WIDTH/8)*unsigned(wr_burst_size srl 1) + unsigned(burst_wr_addr));
                        wr_burst_size_calc_done <= '0';
                    else
                        wr_burst_size_calc_active <= '0';
                        wr_burst_size_calc_done   <= '1';
                    end if;
                end if;
            end if;
        end if;
    end process write_burst_size_calc_proc;


    awaddr_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                burst_wr_addr <= (others => '0');
            elsif (state = IDLE and req_pulse = '1' and conv_req.op_type = S2MM) then
                burst_wr_addr <= conv_req.address;
            elsif (write_done = '1' and state = WRITING_TO_MEM) then
                burst_wr_addr <= std_logic_vector(unsigned(burst_wr_addr) + unsigned(wr_burst_size)*(C_M_AXI_DATA_WIDTH/8));
            end if;
        end if;
    end process awaddr_proc;


    strobe_burst_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                strobe_burst <= '0';
                strobe_len   <= (others => '0');
            elsif (state = IDLE) then
                strobe_burst <= '0';
                strobe_len   <= (others => '0');
            elsif (state = WRITE_BURST_SIZE_CALC) then
                if (wr_burst_size_calc_start = '1') then
                    strobe_burst <= '0';
                    strobe_len   <= (others => '0');
                else
                    if ((unsigned(transaction_wr_counter) + unsigned(wr_burst_size)) > unsigned(transaction_wr_size)) then
                        strobe_burst <= '1';
                        strobe_len   <= std_logic_vector(resize((resize(unsigned(transaction_wr_size), 11)-unsigned(transaction_wr_counter)), 8));
                    else
                        strobe_burst <= '0';
                        strobe_len   <= (others => '0');
                    end if;
                end if;
            end if;
        end if;
    end process strobe_burst_proc;


    awlen_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                axi_awlen <= (others => '0');
            elsif (wr_burst_start = '1') then
                axi_awlen <= wr_burst_size;
            end if;
        end if;
    end process awlen_proc;


    awvalid_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1' or state /= WRITE_INITIATE) then
                wr_burst_start_done   <= '0';
                wr_burst_start_active <= '0';
                axi_awvalid           <= '0';
            else
                if (wr_burst_start = '1') then
                    wr_burst_start_active <= '1';
                end if;

                if (wr_burst_start_active = '1') then
                    axi_awvalid <= '1';
                    if (m_axi_awready = '1') then
                        wr_burst_start_active <= '0';
                        wr_burst_start_done   <= '1';
                    end if;
                end if;

                if (wr_burst_start_done = '1') then
                    axi_awvalid <= '0';
                end if;
            end if;
        end if;
    end process awvalid_proc;


    wdata_counter_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or state /= WRITING_TO_MEM or soft_reset = '1') then
                burst_write_counter <= (others => '0');
            else
                if (m_axi_wready = '1' and axi_wvalid = '1') then
                    burst_write_counter <= std_logic_vector(unsigned(burst_write_counter) + 1);
                end if;
            end if;
        end if;
    end process wdata_counter_proc;


    wdata_total_counter_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                transaction_wr_counter <= (others => '0');
                m_axi_wstrb            <= (others => '0');
            elsif (state = IDLE) then
                transaction_wr_counter <= (others => '0');
            elsif (state /= WRITING_TO_MEM) then
                m_axi_wstrb <= (others => '0');
            else
                if (unsigned(transaction_wr_counter) < unsigned(transaction_wr_size)) then
                    m_axi_wstrb <= (others => '1');
                else
                    m_axi_wstrb <= (others => '0');
                end if;

                if (m_axi_wready = '1' and axi_wvalid = '1') then
                    transaction_wr_counter <= std_logic_vector(unsigned(transaction_wr_counter) + 1);
                    if ((unsigned(transaction_wr_counter) + 1) >= unsigned(transaction_wr_size)) then
                        m_axi_wstrb <= (others => '0');
                    end if;
                end if;

            end if;
        end if;
    end process wdata_total_counter_proc;


    wlast_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or state /= WRITING_TO_MEM or soft_reset = '1') then
                axi_wlast_i <= '0';
            else
                if (unsigned(wr_burst_size) > 2) then
                    if ((unsigned(burst_write_counter) = unsigned(wr_burst_size)-2) and (axi_wvalid = '1') and (m_axi_wready = '1')) then
                        axi_wlast_i <= '1';
                    else
                        axi_wlast_i <= '0';
                    end if;
                elsif (unsigned(wr_burst_size) = 2) then
                    if ((unsigned(burst_write_counter) = unsigned(wr_burst_size)-2) and (axi_wvalid = '1') and (m_axi_wready = '1')) then
                        axi_wlast_i <= '1';
                    else
                        axi_wlast_i <= '0';
                    end if;
                else
                    if ((unsigned(burst_write_counter) = unsigned(wr_burst_size)-1)) then
                        axi_wlast_i <= '1';
                    else
                        axi_wlast_i <= '0';
                    end if;
                end if;
            end if;
        end if;
    end process wlast_proc;


    bready_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or state /= WRITING_TO_MEM or soft_reset = '1') then
                axi_bready <= '0';
                write_done <= '0';
            else
                if (m_axi_bvalid = '1' and axi_bready = '0') then
                    axi_bready <= '1';
                else
                    axi_bready <= axi_bready;
                    if (axi_bready = '1') then
                        axi_bready <= '0';
                        if (write_done = '1') then
                            write_done <= '0';
                        else
                            write_done <= '1';
                        end if;
                    end if;
                end if;
            end if;
        end if;
    end process bready_proc;


    request_edge_detection_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                req_dd <= '0';
                req_d  <= '0';
            else
                req_d  <= conv_req.request;
                req_dd <= req_d;
            end if;
        end if;
    end process request_edge_detection_proc;


    pattern_cnt_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                pattern_finished_i <= '0';
                pattern_cnt        <= (others => '0');
            else
                if (m_axis_tready = '1' and pattern_req = '1' and pattern_finished_i = '0') then
                    if (pattern_cnt = pattern_len-1) then
                        pattern_finished_i <= '1';
                    else
                        pattern_cnt <= pattern_cnt + 1;
                    end if;
                end if;

                if (pattern_req = '0') then
                    pattern_finished_i <= '0';
                    pattern_cnt        <= (others => '0');
                end if;

            end if;
        end if;
    end process pattern_cnt_proc;


end RTL;


-------------------------------------------------------------------------------
-- Title      : AXI Lite Registers
-- Project    :
-------------------------------------------------------------------------------
-- File       : axi_lite_regs.vhd
-- Author     : Gonzalo Martinez Larumbe  <gonzalomlarumbe@gmail.com>
-- Company    :
-- Created    : 2020-02-12
-- Last update: 2023-06-09
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
library xil_defaultlib;

entity axi_lite_regs is
    generic (
        C_S_AXI_DATA_WIDTH : integer := 32;
        C_S_AXI_ADDR_WIDTH : integer := 7
    );
    port (
        soft_reset     : out std_logic;
        system_enable  : out std_logic;
        system_running : in  std_logic;

        conv_op_lch : out std_logic;
        conv_op_rch : out std_logic;
        read_size_l : out unsigned(15 downto 0);
        read_size_r : out unsigned(15 downto 0);

        -- Axi Lite Master R/W interface
        write_request : out std_logic;
        write_data    : out std_logic_vector(31 downto 0);
        write_address : out std_logic_vector(31 downto 0);
        write_done    : in  std_logic;

        read_request    : out std_logic;
        read_address    : out std_logic_vector(31 downto 0);
        read_data       : in  std_logic_vector (31 downto 0);
        read_data_valid : in  std_logic;

        transaction_error : in std_logic;

        -- Pattern counters
        count_lch         : in unsigned(31 downto 0);
        pattern_count_lch : in unsigned(31 downto 0);
        count_rch         : in unsigned(31 downto 0);
        pattern_count_rch : in unsigned(31 downto 0);

        bram_overflow_error       : in std_logic;
        out_reg_underflow_error_l : in std_logic;
        out_reg_overflow_error_l  : in std_logic;
        out_reg_underflow_error_r : in std_logic;
        out_reg_overflow_error_r  : in std_logic;

        -- Slave AXI interface
        s_axi_aclk    : in  std_logic;
        s_axi_aresetn : in  std_logic;
        s_axi_awaddr  : in  std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
        s_axi_awprot  : in  std_logic_vector(2 downto 0);
        s_axi_awvalid : in  std_logic;
        s_axi_awready : out std_logic;
        s_axi_wdata   : in  std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
        s_axi_wstrb   : in  std_logic_vector((C_S_AXI_DATA_WIDTH/8)-1 downto 0);
        s_axi_wvalid  : in  std_logic;
        s_axi_wready  : out std_logic;
        s_axi_bresp   : out std_logic_vector(1 downto 0);
        s_axi_bvalid  : out std_logic;
        s_axi_bready  : in  std_logic;
        s_axi_araddr  : in  std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
        s_axi_arprot  : in  std_logic_vector(2 downto 0);
        s_axi_arvalid : in  std_logic;
        s_axi_arready : out std_logic;
        s_axi_rdata   : out std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
        s_axi_rresp   : out std_logic_vector(1 downto 0);
        s_axi_rvalid  : out std_logic;
        s_axi_rready  : in  std_logic
    );

end axi_lite_regs;


architecture RTL of axi_lite_regs is

    signal axi_awaddr  : std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
    signal axi_awready : std_logic;
    signal axi_wready  : std_logic;
    signal axi_bresp   : std_logic_vector(1 downto 0);
    signal axi_bvalid  : std_logic;
    signal axi_araddr  : std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
    signal axi_arready : std_logic;
    signal axi_rdata   : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal axi_rresp   : std_logic_vector(1 downto 0);
    signal axi_rvalid  : std_logic;

    constant ADDR_LSB          : integer := (C_S_AXI_DATA_WIDTH/32)+ 1;
    constant OPT_MEM_ADDR_BITS : integer := 4;

    signal slv_reg_rden : std_logic;
    signal slv_reg_wren : std_logic;
    signal reg_data_out : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal byte_index   : integer;

    signal soft_reset_i        : std_logic;
    signal soft_reset_cnt      : integer;
    constant SOFT_RESET_CYCLES : integer := 50;

    -- Actual registers
    signal control_reg              : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal status_reg               : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal version_reg              : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal converter_setup_reg      : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal mm2s_size_reg            : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal master_lite_wr_setup_reg : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal master_lite_wr_add_reg   : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal master_lite_wr_data_reg  : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal master_lite_rd_setup_reg : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal master_lite_rd_add_reg   : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal master_lite_rd_data_reg  : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal count_lch_reg            : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal pattern_count_lch_reg    : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal count_rch_reg            : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal pattern_count_rch_reg    : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);

    attribute keep                             : boolean;
    attribute keep of control_reg              : signal is true;
    attribute keep of status_reg               : signal is true;
    attribute keep of version_reg              : signal is true;
    attribute keep of converter_setup_reg      : signal is true;
    attribute keep of mm2s_size_reg            : signal is true;
    attribute keep of master_lite_wr_setup_reg : signal is true;
    attribute keep of master_lite_wr_add_reg   : signal is true;
    attribute keep of master_lite_wr_data_reg  : signal is true;
    attribute keep of master_lite_rd_setup_reg : signal is true;
    attribute keep of master_lite_rd_add_reg   : signal is true;
    attribute keep of master_lite_rd_data_reg  : signal is true;
    attribute keep of count_lch_reg            : signal is true;
    attribute keep of pattern_count_lch_reg    : signal is true;
    attribute keep of count_rch_reg            : signal is true;
    attribute keep of pattern_count_rch_reg    : signal is true;

    -- Bit aliases
    alias BIT_ENABLE     : std_logic is control_reg(0);
    alias BIT_SOFT_RESET : std_logic is control_reg(31);

    alias BIT_RUNNING                 : std_logic is status_reg(0);
    alias BIT_AXI_LITE_MASTER_ERR     : std_logic is status_reg(26);
    alias BIT_BRAM_OVERFLOW_ERR       : std_logic is status_reg(27);
    alias BIT_OUT_REG_UNDERFLOW_ERR_L : std_logic is status_reg(28);
    alias BIT_OUT_REG_OVERFLOW_ERR_L  : std_logic is status_reg(29);
    alias BIT_OUT_REG_UNDERFLOW_ERR_R : std_logic is status_reg(30);
    alias BIT_OUT_REG_OVERFLOW_ERR_R  : std_logic is status_reg(31);

    constant IP_VERSION : std_logic_vector(31 downto 0) := x"DEAD_BEEF";

    alias BIT_CONV_OP_L : std_logic is converter_setup_reg(0);
    alias BIT_CONV_OP_R : std_logic is converter_setup_reg(1);

    alias BIT_WRITE_REQUEST   : std_logic is master_lite_wr_setup_reg(0);
    alias BIT_WRITE_DONE      : std_logic is master_lite_wr_setup_reg(31);
    alias BIT_READ_REQUEST    : std_logic is master_lite_rd_setup_reg(0);
    alias BIT_READ_DATA_VALID : std_logic is master_lite_rd_setup_reg(31);


    -- Procedures
    procedure add_bit (signal sigH : in std_logic; signal bitpos : out std_logic) is
    begin
        if (sigH) then bitpos <= '1';
        else bitpos           <= '0';
        end if;
    end procedure add_bit;


begin

    s_axi_awready <= axi_awready;
    s_axi_wready  <= axi_wready;
    s_axi_bresp   <= axi_bresp;
    s_axi_bvalid  <= axi_bvalid;
    s_axi_arready <= axi_arready;
    s_axi_rdata   <= axi_rdata;
    s_axi_rresp   <= axi_rresp;
    s_axi_rvalid  <= axi_rvalid;

    slv_reg_wren <= axi_wready and s_axi_wvalid and axi_awready and s_axi_awvalid;
    slv_reg_rden <= axi_arready and s_axi_arvalid and (not axi_rvalid);


    -- Implement axi_awready generation
    -- axi_awready is asserted for one S_AXI_ACLK clock cycle when both
    -- S_AXI_AWVALID and S_AXI_WVALID are asserted. axi_awready is
    -- de-asserted when reset is low.
    axi_awready_proc : process (s_axi_aclk)
    begin
        if rising_edge(s_axi_aclk) then
            if (s_axi_aresetn = '0' or soft_reset_i = '1') then
                axi_awready <= '0';
            else
                if (axi_awready = '0' and s_axi_awvalid = '1' and s_axi_wvalid = '1') then
                    axi_awready <= '1';
                else
                    axi_awready <= '0';
                end if;
            end if;
        end if;
    end process axi_awready_proc;


    -- Implement axi_awaddr latching
    -- This process is used to latch the address when both
    -- S_AXI_AWVALID and S_AXI_WVALID are valid.
    axi_awaddr_proc : process (s_axi_aclk)
    begin
        if rising_edge(s_axi_aclk) then
            if (s_axi_aresetn = '0' or soft_reset_i = '1') then
                axi_awaddr <= (others => '0');
            elsif (axi_awready = '0' and s_axi_awvalid = '1' and s_axi_wvalid = '1') then
                axi_awaddr <= s_axi_awaddr;
            end if;
        end if;
    end process axi_awaddr_proc;


    -- Implement axi_wready generation
    -- axi_wready is asserted for one S_AXI_ACLK clock cycle when both
    -- S_AXI_AWVALID and S_AXI_WVALID are asserted. axi_wready is
    -- de-asserted when reset is low.
    axi_wready_proc : process (s_axi_aclk)
    begin
        if rising_edge(s_axi_aclk) then
            if (s_axi_aresetn = '0' or soft_reset_i = '1') then
                axi_wready <= '0';
            else
                if (axi_wready = '0' and s_axi_wvalid = '1' and s_axi_awvalid = '1') then
                    axi_wready <= '1';
                else
                    axi_wready <= '0';
                end if;
            end if;
        end if;
    end process axi_wready_proc;


    -- Implement write response logic generation
    -- The write response and response valid signals are asserted by the slave
    -- when axi_wready, S_AXI_WVALID, axi_wready and S_AXI_WVALID are asserted.
    -- This marks the acceptance of address and indicates the status of
    -- write transaction.
    axi_bvalid_proc : process (s_axi_aclk)
    begin
        if rising_edge(s_axi_aclk) then
            if (s_axi_aresetn = '0' or soft_reset_i = '1') then
                axi_bvalid <= '0';
                axi_bresp  <= "00";
            else
                if (axi_awready = '1' and s_axi_awvalid = '1' and axi_wready = '1' and s_axi_wvalid = '1' and axi_bvalid = '0') then
                    axi_bvalid <= '1';
                    axi_bresp  <= "00";
                elsif (s_axi_bready = '1' and axi_bvalid = '1') then
                    axi_bvalid <= '0';
                end if;
            end if;
        end if;
    end process axi_bvalid_proc;


    -- Implement axi_arready generation
    -- axi_arready is asserted for one S_AXI_ACLK clock cycle when
    -- S_AXI_ARVALID is asserted. axi_awready is
    -- de-asserted when reset (active low) is asserted.
    -- The read address is also latched when S_AXI_ARVALID is
    -- asserted. axi_araddr is reset to zero on reset assertion.
    axi_arready_proc : process (s_axi_aclk)
    begin
        if rising_edge(s_axi_aclk) then
            if (s_axi_aresetn = '0' or soft_reset_i = '1') then
                axi_arready <= '0';
                axi_araddr  <= (others => '1');
            else
                if (axi_arready = '0' and s_axi_arvalid = '1') then
                    axi_arready <= '1';
                    axi_araddr  <= s_axi_araddr;
                else
                    axi_arready <= '0';
                end if;
            end if;
        end if;
    end process axi_arready_proc;


    -- Implement axi_arvalid generation
    -- axi_rvalid is asserted for one S_AXI_ACLK clock cycle when both
    -- S_AXI_ARVALID and axi_arready are asserted. The slave registers
    -- data are available on the axi_rdata bus at this instance. The
    -- assertion of axi_rvalid marks the validity of read data on the
    -- bus and axi_rresp indicates the status of read transaction.axi_rvalid
    -- is deasserted on reset (active low). axi_rresp and axi_rdata are
    -- cleared to zero on reset (active low).
    axi_rvalid_proc : process (s_axi_aclk)
    begin
        if rising_edge(s_axi_aclk) then
            if (s_axi_aresetn = '0' or soft_reset_i = '1') then
                axi_rvalid <= '0';
                axi_rresp  <= "00";
            else
                if (axi_arready = '1' and s_axi_arvalid = '1' and axi_rvalid = '0') then
                    axi_rvalid <= '1';
                    axi_rresp  <= "00";
                elsif (axi_rvalid = '1' and s_axi_rready = '1') then
                    axi_rvalid <= '0';
                end if;
            end if;
        end if;
    end process axi_rvalid_proc;


    -- Output register or memory read data
    axi_rdata_proc : process(s_axi_aclk) is
    begin
        if (rising_edge (s_axi_aclk)) then
            if (s_axi_aresetn = '0' or soft_reset_i = '1') then
                axi_rdata <= (others => '0');
            else
                if (slv_reg_rden = '1') then
                    axi_rdata <= reg_data_out;
                end if;
            end if;
        end if;
    end process axi_rdata_proc;


    -- Read address decoding
    address_decoding_proc : process (all)
        variable loc_addr : std_logic_vector(OPT_MEM_ADDR_BITS downto 0);
    begin
        loc_addr := axi_araddr(ADDR_LSB + OPT_MEM_ADDR_BITS downto ADDR_LSB);
        case loc_addr is
            when b"00000" =>
                reg_data_out <= control_reg;
            when b"00001" =>
                reg_data_out <= status_reg;
            when b"00010" =>
                reg_data_out <= version_reg;
            when b"00011" =>
                reg_data_out <= converter_setup_reg;
            when b"00100" =>
                reg_data_out <= mm2s_size_reg;
            when b"00101" =>
                reg_data_out <= master_lite_wr_setup_reg;
            when b"00110" =>
                reg_data_out <= master_lite_wr_add_reg;
            when b"00111" =>
                reg_data_out <= master_lite_wr_data_reg;
            when b"01000" =>
                reg_data_out <= master_lite_rd_setup_reg;
            when b"01001" =>
                reg_data_out <= master_lite_rd_add_reg;
            when b"01010" =>
                reg_data_out <= master_lite_rd_data_reg;
            when b"01011" =>
                reg_data_out <= count_lch_reg;
            when b"01100" =>
                reg_data_out <= pattern_count_lch_reg;
            when b"01101" =>
                reg_data_out <= count_rch_reg;
            when b"01110" =>
                reg_data_out <= pattern_count_rch_reg;
            when others =>
                reg_data_out <= (others => '0');
        end case;
    end process address_decoding_proc;


    -- Implement memory mapped register select and write logic generation
    -- The write data is accepted and written to memory mapped registers when
    -- axi_awready, S_AXI_WVALID, axi_wready and S_AXI_WVALID are asserted. Write strobes are used to
    -- select byte enables of slave registers while writing.
    -- These registers are cleared when reset (active low) is applied.
    -- Slave register write enable is asserted when valid address and data are available
    -- and the slave is ready to accept the write address and write data.
    read_write_regs_proc : process (s_axi_aclk)
        variable loc_addr : std_logic_vector(OPT_MEM_ADDR_BITS downto 0);
    begin
        if rising_edge(s_axi_aclk) then
            if (s_axi_aresetn = '0') then
                control_reg              <= (others => '0');
                converter_setup_reg      <= (others => '0');
                mm2s_size_reg            <= (others => '0');
                master_lite_wr_setup_reg <= (others => '0');
                master_lite_wr_add_reg   <= (others => '0');
                master_lite_wr_data_reg  <= (others => '0');
                master_lite_rd_setup_reg <= (others => '0');
                master_lite_rd_add_reg   <= (others => '0');
            else
                loc_addr := axi_awaddr(ADDR_LSB + OPT_MEM_ADDR_BITS downto ADDR_LSB);
                if (slv_reg_wren = '1') then
                    case loc_addr is
                        when b"00000" =>
                            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
                                if (s_axi_wstrb(byte_index) = '1') then
                                    control_reg(byte_index*8+7 downto byte_index*8) <= s_axi_wdata(byte_index*8+7 downto byte_index*8);
                                end if;
                            end loop;

                        when b"00011" =>
                            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
                                if (s_axi_wstrb(byte_index) = '1') then
                                    converter_setup_reg(byte_index*8+7 downto byte_index*8) <= s_axi_wdata(byte_index*8+7 downto byte_index*8);
                                end if;
                            end loop;

                        when b"00100" =>
                            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
                                if (s_axi_wstrb(byte_index) = '1') then
                                    mm2s_size_reg(byte_index*8+7 downto byte_index*8) <= s_axi_wdata(byte_index*8+7 downto byte_index*8);
                                end if;
                            end loop;

                        when b"00101" =>
                            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
                                if (s_axi_wstrb(byte_index) = '1') then
                                    master_lite_wr_setup_reg(byte_index*8+7 downto byte_index*8) <= s_axi_wdata(byte_index*8+7 downto byte_index*8);
                                end if;
                            end loop;

                        when b"00110" =>
                            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
                                if (s_axi_wstrb(byte_index) = '1') then
                                    master_lite_wr_add_reg(byte_index*8+7 downto byte_index*8) <= s_axi_wdata(byte_index*8+7 downto byte_index*8);
                                end if;
                            end loop;

                        when b"00111" =>
                            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
                                if (s_axi_wstrb(byte_index) = '1') then
                                    master_lite_wr_data_reg(byte_index*8+7 downto byte_index*8) <= s_axi_wdata(byte_index*8+7 downto byte_index*8);
                                end if;
                            end loop;

                        when b"01000" =>
                            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
                                if (s_axi_wstrb(byte_index) = '1') then
                                    master_lite_rd_setup_reg(byte_index*8+7 downto byte_index*8) <= s_axi_wdata(byte_index*8+7 downto byte_index*8);
                                end if;
                            end loop;

                        when b"01001" =>
                            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
                                if (s_axi_wstrb(byte_index) = '1') then
                                    master_lite_rd_add_reg(byte_index*8+7 downto byte_index*8) <= s_axi_wdata(byte_index*8+7 downto byte_index*8);
                                end if;
                            end loop;

                        when others => null;

                    end case;
                end if;

                -- Soft reset bit auto-clear after specified amount of cycles
                if (soft_reset_cnt = SOFT_RESET_CYCLES - 1) then
                    BIT_SOFT_RESET <= '0';
                end if;

                -- Axi lite master read/write requests should last one cycle
                if (read_request) then
                    BIT_READ_REQUEST <= '0';
                end if;

                if (read_data_valid) then
                    BIT_READ_DATA_VALID <= '1';
                end if;

                if (write_request) then
                    BIT_WRITE_REQUEST <= '0';
                end if;

                if (write_done) then
                    BIT_WRITE_DONE <= '1';
                end if;



            end if;
        end if;
    end process read_write_regs_proc;



    read_only_regs_proc : process (s_axi_aclk) is
    begin
        if rising_edge(s_axi_aclk) then
            if (s_axi_aresetn = '0' or soft_reset_i = '1') then
                status_reg              <= (others => '0');
                version_reg             <= IP_VERSION;
                count_lch_reg           <= (others => '0');
                pattern_count_lch_reg   <= (others => '0');
                count_rch_reg           <= (others => '0');
                pattern_count_rch_reg   <= (others => '0');
                master_lite_rd_data_reg <= (others => '0');
            else
                -- Status REG
                add_bit(system_running, BIT_RUNNING);
                add_bit(transaction_error, BIT_AXI_LITE_MASTER_ERR);
                add_bit(bram_overflow_error, BIT_BRAM_OVERFLOW_ERR);
                add_bit(out_reg_underflow_error_l, BIT_OUT_REG_UNDERFLOW_ERR_L);
                add_bit(out_reg_overflow_error_l, BIT_OUT_REG_OVERFLOW_ERR_L);
                add_bit(out_reg_underflow_error_r, BIT_OUT_REG_UNDERFLOW_ERR_R);
                add_bit(out_reg_overflow_error_r, BIT_OUT_REG_OVERFLOW_ERR_R);
                -- Version REG
                version_reg             <= IP_VERSION;
                -- Pattern counters
                count_lch_reg           <= std_logic_vector(count_lch);
                pattern_count_lch_reg   <= std_logic_vector(pattern_count_lch);
                count_rch_reg           <= std_logic_vector(count_rch);
                pattern_count_rch_reg   <= std_logic_vector(pattern_count_rch);
                -- Axi lite master read data
                master_lite_rd_data_reg <= read_data;
            end if;
        end if;
    end process read_only_regs_proc;



    control_reg_signals_proc : process (s_axi_aclk) is
        procedure add_out_sigH (signal sigH : out std_logic; signal bitpos : in std_logic) is
        begin
            if (bitpos) then sigH <= '1';
            else sigH             <= '0';
            end if;
        end procedure add_out_sigH;

    begin
        if rising_edge(s_axi_aclk) then
            if (s_axi_aresetn = '0' or soft_reset_i = '1') then
                system_enable <= '0';
            else
                add_out_sigH(system_enable, BIT_ENABLE);
            end if;
        end if;
    end process control_reg_signals_proc;



    soft_reset_out_proc : process (s_axi_aclk) is
    begin
        if (rising_edge(s_axi_aclk)) then
            if (s_axi_aresetn = '0') then
                soft_reset     <= '0';
                soft_reset_i   <= '0';
                soft_reset_cnt <= 0;
            else
                if (BIT_SOFT_RESET = '1') then
                    soft_reset_i   <= '1';
                    soft_reset_cnt <= soft_reset_cnt + 1;
                else
                    soft_reset_i   <= '0';
                    soft_reset_cnt <= 0;
                end if;

                soft_reset <= soft_reset_i;
            end if;
        end if;
    end process soft_reset_out_proc;


    mm2s_proc : process (s_axi_aclk) is
    begin
        if (rising_edge(s_axi_aclk)) then
            if (s_axi_aresetn = '0' or soft_reset_i = '1') then
                conv_op_lch <= '0';
                conv_op_rch <= '0';
                read_size_l <= (others => '0');
                read_size_r <= (others => '0');
            else
                add_bit(BIT_CONV_OP_L, conv_op_lch);
                add_bit(BIT_CONV_OP_R, conv_op_rch);
                read_size_l <= unsigned(mm2s_size_reg(15 downto 0));
                read_size_r <= unsigned(mm2s_size_reg(31 downto 16));
            end if;
        end if;
    end process mm2s_proc;


    master_lite_proc : process (s_axi_aclk) is
    begin
        if (rising_edge(s_axi_aclk)) then
            if (s_axi_aresetn = '0' or soft_reset_i = '1') then
                write_address <= (others => '0');
                write_data    <= (others => '0');
                write_request <= '0';
                read_address  <= (others => '0');
                read_request  <= '0';
            else
                write_address <= master_lite_wr_add_reg;
                write_data    <= master_lite_wr_data_reg;
                write_request <= BIT_WRITE_REQUEST;
                read_address  <= master_lite_rd_add_reg;
                read_request  <= BIT_READ_REQUEST;
            end if;
        end if;
    end process master_lite_proc;



end RTL;



library ieee;
library xil_defaultlib;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity axi_lite_master is
    generic (
        C_M_AXIL_MASTER_TARGET_BASE_ADDR : std_logic_vector := x"0000_0000";
        C_M_AXIL_MASTER_ADDR_WIDTH       : integer          := 32;
        C_M_AXIL_MASTER_DATA_WIDTH       : integer          := 32
    );
    port (
        soft_reset        : in  std_logic;
        transaction_error : out std_logic;

        write_request : in  std_logic                     := '0';
        write_data    : in  std_logic_vector(31 downto 0) := x"0000_0000";
        write_address : in  std_logic_vector(31 downto 0) := x"0000_0000";
        write_done    : out std_logic;

        read_request    : in  std_logic                     := '0';
        read_address    : in  std_logic_vector(31 downto 0) := x"0000_0000";
        read_data       : out std_logic_vector (31 downto 0);
        read_data_valid : out std_logic;

        m_axi_aclk    : in  std_logic;
        m_axi_aresetn : in  std_logic;
        m_axi_awaddr  : out std_logic_vector(C_M_AXIL_MASTER_ADDR_WIDTH-1 downto 0);
        m_axi_awprot  : out std_logic_vector(2 downto 0);
        m_axi_awvalid : out std_logic;
        m_axi_awready : in  std_logic;
        m_axi_wdata   : out std_logic_vector(C_M_AXIL_MASTER_DATA_WIDTH-1 downto 0);
        m_axi_wstrb   : out std_logic_vector(C_M_AXIL_MASTER_DATA_WIDTH/8-1 downto 0);
        m_axi_wvalid  : out std_logic;
        m_axi_wready  : in  std_logic;
        m_axi_bresp   : in  std_logic_vector(1 downto 0);
        m_axi_bvalid  : in  std_logic;
        m_axi_bready  : out std_logic;
        m_axi_araddr  : out std_logic_vector(C_M_AXIL_MASTER_ADDR_WIDTH-1 downto 0);
        m_axi_arprot  : out std_logic_vector(2 downto 0);
        m_axi_arvalid : out std_logic;
        m_axi_arready : in  std_logic;
        m_axi_rdata   : in  std_logic_vector(C_M_AXIL_MASTER_DATA_WIDTH-1 downto 0);
        m_axi_rresp   : in  std_logic_vector(1 downto 0);
        m_axi_rvalid  : in  std_logic;
        m_axi_rready  : out std_logic
    );
end axi_lite_master;


architecture RTL of axi_lite_master is

    signal axi_awvalid : std_logic;
    signal axi_wvalid  : std_logic;
    signal axi_arvalid : std_logic;
    signal axi_rready  : std_logic;
    signal axi_bready  : std_logic;
    signal axi_awaddr  : std_logic_vector(C_M_AXIL_MASTER_ADDR_WIDTH-1 downto 0);
    signal axi_wdata   : std_logic_vector(C_M_AXIL_MASTER_DATA_WIDTH-1 downto 0);
    signal axi_araddr  : std_logic_vector(C_M_AXIL_MASTER_ADDR_WIDTH-1 downto 0);

    signal write_resp_error : std_logic;
    signal read_resp_error  : std_logic;

    signal start_single_read : std_logic := '0';
    signal reading           : std_logic;


begin

    m_axi_awaddr  <= std_logic_vector (unsigned(C_M_AXIL_MASTER_TARGET_BASE_ADDR) + unsigned(axi_awaddr));
    m_axi_wdata   <= axi_wdata;
    m_axi_awvalid <= axi_awvalid;
    m_axi_wvalid  <= axi_wvalid;
    m_axi_bready  <= axi_bready;
    m_axi_araddr  <= std_logic_vector(unsigned(C_M_AXIL_MASTER_TARGET_BASE_ADDR) + unsigned(axi_araddr));
    m_axi_rready  <= axi_rready;
    m_axi_arvalid <= axi_arvalid;
    m_axi_awprot  <= "000";
    m_axi_wstrb   <= "1111";
    m_axi_arprot  <= "001";

    read_data <= m_axi_rdata when (m_axi_rvalid = '1' and axi_rready = '1') else
                                                                            (others => '0');
    read_data_valid <= axi_rready and m_axi_rvalid;
    write_done      <= axi_bready and m_axi_bvalid;

    write_resp_error <= (axi_bready and m_axi_bvalid and m_axi_bresp(1));
    read_resp_error  <= (axi_rready and m_axi_rvalid and m_axi_rresp(1));


    awvalid_proc : process(m_axi_aclk)
    begin
        if (rising_edge (m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                axi_awvalid <= '0';
            else
                if (write_request = '1') then
                    axi_awvalid <= '1';
                elsif (m_axi_awready = '1' and axi_awvalid = '1') then
                    axi_awvalid <= '0';
                end if;
            end if;
        end if;
    end process awvalid_proc;


    wvalid_proc : process(m_axi_aclk)
    begin
        if (rising_edge (m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                axi_wvalid <= '0';
            else
                if (write_request = '1') then
                    axi_wvalid <= '1';
                elsif (m_axi_wready = '1' and axi_wvalid = '1') then
                    axi_wvalid <= '0';
                end if;
            end if;
        end if;
    end process wvalid_proc;


    awaddr_proc : process(m_axi_aclk)
    begin
        if (rising_edge (m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                axi_awaddr <= (others => '0');
            elsif (write_request = '1') then
                axi_awaddr <= write_address;
            end if;
        end if;
    end process awaddr_proc;


    wdata_proc : process(m_axi_aclk)
    begin
        if (rising_edge (m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                axi_wdata <= (others => '0');
            else
                if (write_request = '1') then
                    axi_wdata <= write_data;
                elsif (m_axi_bvalid = '1' and axi_bready = '1') then
                    axi_wdata <= (others => '0');
                end if;
            end if;
        end if;
    end process wdata_proc;


    bready_proc : process(m_axi_aclk)
    begin
        if (rising_edge (m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                axi_bready <= '0';
            else
                if (m_axi_bvalid = '1' and axi_bready = '0') then
                    axi_bready <= '1';
                elsif (axi_bready = '1') then
                    axi_bready <= '0';
                end if;
            end if;
        end if;
    end process bready_proc;


    arvalid_proc : process(m_axi_aclk)
    begin
        if (rising_edge (m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                axi_arvalid <= '0';
            else
                if (start_single_read = '1') then
                    axi_arvalid <= '1';
                elsif (m_axi_arready = '1' and axi_arvalid = '1') then
                    axi_arvalid <= '0';
                end if;
            end if;
        end if;
    end process arvalid_proc;


    rready_proc : process(m_axi_aclk)
    begin
        if (rising_edge (m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                axi_rready <= '0';
            else
                if (m_axi_rvalid = '1' and axi_rready = '0') then
                    axi_rready <= '1';
                elsif (axi_rready = '1') then
                    axi_rready <= '0';
                end if;
            end if;
        end if;
    end process rready_proc;


    araddr_proc : process(m_axi_aclk)
    begin
        if (rising_edge (m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                axi_araddr <= (others => '0');
            elsif (read_request = '1') then
                axi_araddr <= read_address;
            end if;
        end if;
    end process araddr_proc;


    do_read : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                start_single_read <= '0';
                reading           <= '0';
            else
                if (read_request = '1' or reading = '1') then
                    if (start_single_read = '1') then
                        start_single_read <= '0';

                    elsif (reading = '0') then
                        start_single_read <= '1';
                        reading           <= '1';
                    end if;
                end if;

                if (read_data_valid = '1') then
                    reading <= '0';
                end if;
            end if;
        end if;

    end process do_read;


    error_proc : process (all) is
    begin
        transaction_error <= read_resp_error or write_resp_error;
    end process error_proc;



end RTL;

