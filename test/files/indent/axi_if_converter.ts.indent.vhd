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
        m_axi_conf_awaddr  : out std_logic_vector(C_M_MEM_AXI_ADDR_WIDTH-1 downto 0);
        m_axi_conf_awprot  : out std_logic_vector(2 downto 0);
        m_axi_conf_awvalid : out std_logic;
        m_axi_conf_awready : in  std_logic;
        m_axi_conf_wdata   : out std_logic_vector(C_M_MEM_AXI_DATA_WIDTH-1 downto 0);
        m_axi_conf_wstrb   : out std_logic_vector(C_M_MEM_AXI_DATA_WIDTH/8-1 downto 0);
        m_axi_conf_wvalid  : out std_logic;
        m_axi_conf_wready  : in  std_logic;
        m_axi_conf_bresp   : in  std_logic_vector(1 downto 0);
        m_axi_conf_bvalid  : in  std_logic;
        m_axi_conf_bready  : out std_logic;
        m_axi_conf_araddr  : out std_logic_vector(C_M_MEM_AXI_ADDR_WIDTH-1 downto 0);
        m_axi_conf_arprot  : out std_logic_vector(2 downto 0);
        m_axi_conf_arvalid : out std_logic;
        m_axi_conf_arready : in  std_logic;
        m_axi_conf_rdata   : in  std_logic_vector(C_M_MEM_AXI_DATA_WIDTH-1 downto 0);
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
        C_M_MEM_AXI_TARGET_SLAVE_BASE_ADDR => C_M_MEM_AXI_TARGET_SLAVE_BASE_ADDR,
        C_M_MEM_AXI_ADDR_WIDTH             => C_M_MEM_AXI_ADDR_WIDTH,
        C_M_MEM_AXI_DATA_WIDTH             => C_M_MEM_AXI_DATA_WIDTH
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
        clk         => clk,
        resetn      => resetn,
        soft_reset  => soft_reset,
        clk_fs      => clk_fs,
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
