library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
library xil_defaultlib;
use xil_defaultlib.global.all;
use xil_defaultlib.global_sim.all;
use xil_defaultlib.axil_slave_bfm.all;
use xil_defaultlib.axil_master_bfm.all;
use xil_defaultlib.axif_master_bfm.all;

------------------------------------------------------------------------------------------------------------------------

entity tb_axi_if_converter is
end entity tb_axi_if_converter;

------------------------------------------------------------------------------------------------------------------------

architecture TB of tb_axi_if_converter is

    constant C_S_AXI_DATA_WIDTH : integer := 32;
    constant C_S_AXI_ADDR_WIDTH : integer := 7;

-- component ports
    signal clk        : std_logic := '1';
    signal resetn     : std_logic := '0';
    signal clk_fs_ext : std_logic := '1';

    signal s_axi_aclk    : std_logic := '1';
    signal s_axi_aresetn : std_logic := '0';
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

    signal s_axis_lch_aclk    : std_logic;
    signal s_axis_lch_aresetn : std_logic;
    signal s_axis_lch_tdata   : std_logic_vector(63 downto 0) := (others => '0');
    signal s_axis_lch_tvalid  : std_logic;
    signal s_axis_lch_tkeep   : std_logic_vector(7 downto 0);
    signal s_axis_lch_tlast   : std_logic;
    signal s_axis_lch_tready  : std_logic;

    signal s_axis_rch_aclk    : std_logic;
    signal s_axis_rch_aresetn : std_logic;
    signal s_axis_rch_tdata   : std_logic_vector(63 downto 0) := (others => '1');
    signal s_axis_rch_tvalid  : std_logic;
    signal s_axis_rch_tkeep   : std_logic_vector(7 downto 0);
    signal s_axis_rch_tlast   : std_logic;
    signal s_axis_rch_tready  : std_logic;

    signal m_axis_lch_aclk    : std_logic;
    signal m_axis_lch_aresetn : std_logic;
    signal m_axis_lch_tdata   : std_logic_vector(63 downto 0);
    signal m_axis_lch_tvalid  : std_logic;
    signal m_axis_lch_tkeep   : std_logic_vector(7 downto 0);
    signal m_axis_lch_tlast   : std_logic;
    signal m_axis_lch_tready  : std_logic := '1';
    signal m_axis_lch_tdest   : std_logic;

    signal m_axis_rch_aclk    : std_logic;
    signal m_axis_rch_aresetn : std_logic;
    signal m_axis_rch_tdata   : std_logic_vector(63 downto 0);
    signal m_axis_rch_tvalid  : std_logic;
    signal m_axis_rch_tkeep   : std_logic_vector(7 downto 0);
    signal m_axis_rch_tlast   : std_logic;
    signal m_axis_rch_tready  : std_logic := '1';
    signal m_axis_rch_tdest   : std_logic;

    signal m_axi_lch_aclk    : std_logic;
    signal m_axi_lch_aresetn : std_logic;
    signal m_axi_lch_awid    : std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
    signal m_axi_lch_awaddr  : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    signal m_axi_lch_awlen   : std_logic_vector(7 downto 0);
    signal m_axi_lch_awsize  : std_logic_vector(2 downto 0);
    signal m_axi_lch_awburst : std_logic_vector(1 downto 0);
    signal m_axi_lch_awlock  : std_logic;
    signal m_axi_lch_awcache : std_logic_vector(3 downto 0);
    signal m_axi_lch_awprot  : std_logic_vector(2 downto 0);
    signal m_axi_lch_awqos   : std_logic_vector(3 downto 0);
    signal m_axi_lch_awuser  : std_logic_vector(C_M_AXI_AWUSER_WIDTH-1 downto 0);
    signal m_axi_lch_awvalid : std_logic;
    signal m_axi_lch_awready : std_logic;
    signal m_axi_lch_wdata   : std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
    signal m_axi_lch_wstrb   : std_logic_vector(C_M_AXI_DATA_WIDTH/8-1 downto 0);
    signal m_axi_lch_wlast   : std_logic;
    signal m_axi_lch_wuser   : std_logic_vector(C_M_AXI_WUSER_WIDTH-1 downto 0);
    signal m_axi_lch_wvalid  : std_logic;
    signal m_axi_lch_wready  : std_logic;
    signal m_axi_lch_bid     : std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
    signal m_axi_lch_bresp   : std_logic_vector(1 downto 0);
    signal m_axi_lch_buser   : std_logic_vector(C_M_AXI_BUSER_WIDTH-1 downto 0);
    signal m_axi_lch_bvalid  : std_logic;
    signal m_axi_lch_bready  : std_logic;
    signal m_axi_lch_arid    : std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
    signal m_axi_lch_araddr  : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    signal m_axi_lch_arlen   : std_logic_vector(7 downto 0);
    signal m_axi_lch_arsize  : std_logic_vector(2 downto 0);
    signal m_axi_lch_arburst : std_logic_vector(1 downto 0);
    signal m_axi_lch_arlock  : std_logic;
    signal m_axi_lch_arcache : std_logic_vector(3 downto 0);
    signal m_axi_lch_arprot  : std_logic_vector(2 downto 0);
    signal m_axi_lch_arqos   : std_logic_vector(3 downto 0);
    signal m_axi_lch_aruser  : std_logic_vector(C_M_AXI_ARUSER_WIDTH-1 downto 0);
    signal m_axi_lch_arvalid : std_logic;
    signal m_axi_lch_arready : std_logic;
    signal m_axi_lch_rid     : std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
    signal m_axi_lch_rdata   : std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
    signal m_axi_lch_rresp   : std_logic_vector(1 downto 0);
    signal m_axi_lch_rlast   : std_logic;
    signal m_axi_lch_ruser   : std_logic_vector(C_M_AXI_RUSER_WIDTH-1 downto 0);
    signal m_axi_lch_rvalid  : std_logic;
    signal m_axi_lch_rready  : std_logic;

    signal m_axi_rch_aclk    : std_logic;
    signal m_axi_rch_aresetn : std_logic;
    signal m_axi_rch_awid    : std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
    signal m_axi_rch_awaddr  : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    signal m_axi_rch_awlen   : std_logic_vector(7 downto 0);
    signal m_axi_rch_awsize  : std_logic_vector(2 downto 0);
    signal m_axi_rch_awburst : std_logic_vector(1 downto 0);
    signal m_axi_rch_awlock  : std_logic;
    signal m_axi_rch_awcache : std_logic_vector(3 downto 0);
    signal m_axi_rch_awprot  : std_logic_vector(2 downto 0);
    signal m_axi_rch_awqos   : std_logic_vector(3 downto 0);
    signal m_axi_rch_awuser  : std_logic_vector(C_M_AXI_AWUSER_WIDTH-1 downto 0);
    signal m_axi_rch_awvalid : std_logic;
    signal m_axi_rch_awready : std_logic;
    signal m_axi_rch_wdata   : std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
    signal m_axi_rch_wstrb   : std_logic_vector(C_M_AXI_DATA_WIDTH/8-1 downto 0);
    signal m_axi_rch_wlast   : std_logic;
    signal m_axi_rch_wuser   : std_logic_vector(C_M_AXI_WUSER_WIDTH-1 downto 0);
    signal m_axi_rch_wvalid  : std_logic;
    signal m_axi_rch_wready  : std_logic;
    signal m_axi_rch_bid     : std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
    signal m_axi_rch_bresp   : std_logic_vector(1 downto 0);
    signal m_axi_rch_buser   : std_logic_vector(C_M_AXI_BUSER_WIDTH-1 downto 0);
    signal m_axi_rch_bvalid  : std_logic;
    signal m_axi_rch_bready  : std_logic;
    signal m_axi_rch_arid    : std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
    signal m_axi_rch_araddr  : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    signal m_axi_rch_arlen   : std_logic_vector(7 downto 0);
    signal m_axi_rch_arsize  : std_logic_vector(2 downto 0);
    signal m_axi_rch_arburst : std_logic_vector(1 downto 0);
    signal m_axi_rch_arlock  : std_logic;
    signal m_axi_rch_arcache : std_logic_vector(3 downto 0);
    signal m_axi_rch_arprot  : std_logic_vector(2 downto 0);
    signal m_axi_rch_arqos   : std_logic_vector(3 downto 0);
    signal m_axi_rch_aruser  : std_logic_vector(C_M_AXI_ARUSER_WIDTH-1 downto 0);
    signal m_axi_rch_arvalid : std_logic;
    signal m_axi_rch_arready : std_logic;
    signal m_axi_rch_rid     : std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
    signal m_axi_rch_rdata   : std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
    signal m_axi_rch_rresp   : std_logic_vector(1 downto 0);
    signal m_axi_rch_rlast   : std_logic;
    signal m_axi_rch_ruser   : std_logic_vector(C_M_AXI_RUSER_WIDTH-1 downto 0);
    signal m_axi_rch_rvalid  : std_logic;
    signal m_axi_rch_rready  : std_logic;

    signal m_axi_conf_aclk    : std_logic;
    signal m_axi_conf_aresetn : std_logic;
    signal m_axi_conf_awaddr  : std_logic_vector(C_M_MEM_AXI_ADDR_WIDTH-1 downto 0);
    signal m_axi_conf_awprot  : std_logic_vector(2 downto 0);
    signal m_axi_conf_awvalid : std_logic;
    signal m_axi_conf_awready : std_logic;
    signal m_axi_conf_wdata   : std_logic_vector(C_M_MEM_AXI_DATA_WIDTH-1 downto 0);
    signal m_axi_conf_wstrb   : std_logic_vector(C_M_MEM_AXI_DATA_WIDTH/8-1 downto 0);
    signal m_axi_conf_wvalid  : std_logic;
    signal m_axi_conf_wready  : std_logic;
    signal m_axi_conf_bresp   : std_logic_vector(1 downto 0);
    signal m_axi_conf_bvalid  : std_logic;
    signal m_axi_conf_bready  : std_logic;
    signal m_axi_conf_araddr  : std_logic_vector(C_M_MEM_AXI_ADDR_WIDTH-1 downto 0);
    signal m_axi_conf_arprot  : std_logic_vector(2 downto 0);
    signal m_axi_conf_arvalid : std_logic;
    signal m_axi_conf_arready : std_logic;
    signal m_axi_conf_rdata   : std_logic_vector(C_M_MEM_AXI_DATA_WIDTH-1 downto 0);
    signal m_axi_conf_rresp   : std_logic_vector(1 downto 0);
    signal m_axi_conf_rvalid  : std_logic;
    signal m_axi_conf_rready  : std_logic;

-- BFMs
-- Axi lite slave
    signal s_bfm_in_r  : s_common_response_r_in;
    signal s_bfm_out_r : s_common_response_r_out;
    signal s_bfm_in_w  : s_common_response_w_in;
    signal s_bfm_out_w : s_common_response_w_out;

-- Axi lite master
    signal m_bfm_in_r  : m_common_response_r_type_in;
    signal m_bfm_out_r : m_common_response_r_type_out;
    signal m_bfm_in_w  : m_common_response_w_type_in;
    signal m_bfm_out_w : m_common_response_w_type_out;

-- TB aux signals
    signal stop_clock : std_logic := '0';

begin

-- component instantiation
    DUT : entity xil_defaultlib.axi_if_converter
        port map (
            clk        => clk,
            resetn     => resetn,
            clk_fs_ext => clk_fs_ext,

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
            s_axi_rready  => s_axi_rready,

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

            m_axis_lch_aclk    => m_axis_lch_aclk,
            m_axis_lch_aresetn => m_axis_lch_aresetn,
            m_axis_lch_tdata   => m_axis_lch_tdata,
            m_axis_lch_tvalid  => m_axis_lch_tvalid,
            m_axis_lch_tkeep   => m_axis_lch_tkeep,
            m_axis_lch_tlast   => m_axis_lch_tlast,
            m_axis_lch_tready  => m_axis_lch_tready,
            m_axis_lch_tdest   => m_axis_lch_tdest,

            m_axis_rch_aclk    => m_axis_rch_aclk,
            m_axis_rch_aresetn => m_axis_rch_aresetn,
            m_axis_rch_tdata   => m_axis_rch_tdata,
            m_axis_rch_tvalid  => m_axis_rch_tvalid,
            m_axis_rch_tkeep   => m_axis_rch_tkeep,
            m_axis_rch_tlast   => m_axis_rch_tlast,
            m_axis_rch_tready  => m_axis_rch_tready,
            m_axis_rch_tdest   => m_axis_rch_tdest,

            m_axi_lch_aclk    => m_axi_lch_aclk,
            m_axi_lch_aresetn => m_axi_lch_aresetn,
            m_axi_lch_awid    => m_axi_lch_awid,
            m_axi_lch_awaddr  => m_axi_lch_awaddr,
            m_axi_lch_awlen   => m_axi_lch_awlen,
            m_axi_lch_awsize  => m_axi_lch_awsize,
            m_axi_lch_awburst => m_axi_lch_awburst,
            m_axi_lch_awlock  => m_axi_lch_awlock,
            m_axi_lch_awcache => m_axi_lch_awcache,
            m_axi_lch_awprot  => m_axi_lch_awprot,
            m_axi_lch_awqos   => m_axi_lch_awqos,
            m_axi_lch_awuser  => m_axi_lch_awuser,
            m_axi_lch_awvalid => m_axi_lch_awvalid,
            m_axi_lch_awready => m_axi_lch_awready,
            m_axi_lch_wdata   => m_axi_lch_wdata,
            m_axi_lch_wstrb   => m_axi_lch_wstrb,
            m_axi_lch_wlast   => m_axi_lch_wlast,
            m_axi_lch_wuser   => m_axi_lch_wuser,
            m_axi_lch_wvalid  => m_axi_lch_wvalid,
            m_axi_lch_wready  => m_axi_lch_wready,
            m_axi_lch_bid     => m_axi_lch_bid,
            m_axi_lch_bresp   => m_axi_lch_bresp,
            m_axi_lch_buser   => m_axi_lch_buser,
            m_axi_lch_bvalid  => m_axi_lch_bvalid,
            m_axi_lch_bready  => m_axi_lch_bready,
            m_axi_lch_arid    => m_axi_lch_arid,
            m_axi_lch_araddr  => m_axi_lch_araddr,
            m_axi_lch_arlen   => m_axi_lch_arlen,
            m_axi_lch_arsize  => m_axi_lch_arsize,
            m_axi_lch_arburst => m_axi_lch_arburst,
            m_axi_lch_arlock  => m_axi_lch_arlock,
            m_axi_lch_arcache => m_axi_lch_arcache,
            m_axi_lch_arprot  => m_axi_lch_arprot,
            m_axi_lch_arqos   => m_axi_lch_arqos,
            m_axi_lch_aruser  => m_axi_lch_aruser,
            m_axi_lch_arvalid => m_axi_lch_arvalid,
            m_axi_lch_arready => m_axi_lch_arready,
            m_axi_lch_rid     => m_axi_lch_rid,
            m_axi_lch_rdata   => m_axi_lch_rdata,
            m_axi_lch_rresp   => m_axi_lch_rresp,
            m_axi_lch_rlast   => m_axi_lch_rlast,
            m_axi_lch_ruser   => m_axi_lch_ruser,
            m_axi_lch_rvalid  => m_axi_lch_rvalid,
            m_axi_lch_rready  => m_axi_lch_rready,

            m_axi_rch_aclk    => m_axi_rch_aclk,
            m_axi_rch_aresetn => m_axi_rch_aresetn,
            m_axi_rch_awid    => m_axi_rch_awid,
            m_axi_rch_awaddr  => m_axi_rch_awaddr,
            m_axi_rch_awlen   => m_axi_rch_awlen,
            m_axi_rch_awsize  => m_axi_rch_awsize,
            m_axi_rch_awburst => m_axi_rch_awburst,
            m_axi_rch_awlock  => m_axi_rch_awlock,
            m_axi_rch_awcache => m_axi_rch_awcache,
            m_axi_rch_awprot  => m_axi_rch_awprot,
            m_axi_rch_awqos   => m_axi_rch_awqos,
            m_axi_rch_awuser  => m_axi_rch_awuser,
            m_axi_rch_awvalid => m_axi_rch_awvalid,
            m_axi_rch_awready => m_axi_rch_awready,
            m_axi_rch_wdata   => m_axi_rch_wdata,
            m_axi_rch_wstrb   => m_axi_rch_wstrb,
            m_axi_rch_wlast   => m_axi_rch_wlast,
            m_axi_rch_wuser   => m_axi_rch_wuser,
            m_axi_rch_wvalid  => m_axi_rch_wvalid,
            m_axi_rch_wready  => m_axi_rch_wready,
            m_axi_rch_bid     => m_axi_rch_bid,
            m_axi_rch_bresp   => m_axi_rch_bresp,
            m_axi_rch_buser   => m_axi_rch_buser,
            m_axi_rch_bvalid  => m_axi_rch_bvalid,
            m_axi_rch_bready  => m_axi_rch_bready,
            m_axi_rch_arid    => m_axi_rch_arid,
            m_axi_rch_araddr  => m_axi_rch_araddr,
            m_axi_rch_arlen   => m_axi_rch_arlen,
            m_axi_rch_arsize  => m_axi_rch_arsize,
            m_axi_rch_arburst => m_axi_rch_arburst,
            m_axi_rch_arlock  => m_axi_rch_arlock,
            m_axi_rch_arcache => m_axi_rch_arcache,
            m_axi_rch_arprot  => m_axi_rch_arprot,
            m_axi_rch_arqos   => m_axi_rch_arqos,
            m_axi_rch_aruser  => m_axi_rch_aruser,
            m_axi_rch_arvalid => m_axi_rch_arvalid,
            m_axi_rch_arready => m_axi_rch_arready,
            m_axi_rch_rid     => m_axi_rch_rid,
            m_axi_rch_rdata   => m_axi_rch_rdata,
            m_axi_rch_rresp   => m_axi_rch_rresp,
            m_axi_rch_rlast   => m_axi_rch_rlast,
            m_axi_rch_ruser   => m_axi_rch_ruser,
            m_axi_rch_rvalid  => m_axi_rch_rvalid,
            m_axi_rch_rready  => m_axi_rch_rready,

            m_axi_conf_aclk    => m_axi_conf_aclk,
            m_axi_conf_aresetn => m_axi_conf_aresetn,
            m_axi_conf_awaddr  => m_axi_conf_awaddr,
            m_axi_conf_awprot  => m_axi_conf_awprot,
            m_axi_conf_awvalid => m_axi_conf_awvalid,
            m_axi_conf_awready => m_axi_conf_awready,
            m_axi_conf_wdata   => m_axi_conf_wdata,
            m_axi_conf_wstrb   => m_axi_conf_wstrb,
            m_axi_conf_wvalid  => m_axi_conf_wvalid,
            m_axi_conf_wready  => m_axi_conf_wready,
            m_axi_conf_bresp   => m_axi_conf_bresp,
            m_axi_conf_bvalid  => m_axi_conf_bvalid,
            m_axi_conf_bready  => m_axi_conf_bready,
            m_axi_conf_araddr  => m_axi_conf_araddr,
            m_axi_conf_arprot  => m_axi_conf_arprot,
            m_axi_conf_arvalid => m_axi_conf_arvalid,
            m_axi_conf_arready => m_axi_conf_arready,
            m_axi_conf_rdata   => m_axi_conf_rdata,
            m_axi_conf_rresp   => m_axi_conf_rresp,
            m_axi_conf_rvalid  => m_axi_conf_rvalid,
            m_axi_conf_rready  => m_axi_conf_rready
            );



    I_SLAVEMODEL_L : entity xil_defaultlib.s_axi_model
        port map (
            s_axi_aclk     => m_axi_lch_aclk,
            s_axi_aresetn  => m_axi_lch_aresetn,
            s_axi_arid     => m_axi_lch_arid,
            s_axi_araddr   => m_axi_lch_araddr(9 downto 0),
            s_axi_arlen    => m_axi_lch_arlen,
            s_axi_arsize   => m_axi_lch_arsize,
            s_axi_arburst  => m_axi_lch_arburst,
            s_axi_arlock   => m_axi_lch_arlock,
            s_axi_arcache  => m_axi_lch_arcache,
            s_axi_arprot   => m_axi_lch_arprot,
            s_axi_arqos    => m_axi_lch_arqos,
            s_axi_arregion => (others => '0'),
            s_axi_aruser   => m_axi_lch_aruser,
            s_axi_arvalid  => m_axi_lch_arvalid,
            s_axi_arready  => m_axi_lch_arready,
            s_axi_rid      => m_axi_lch_rid,
            s_axi_rdata    => m_axi_lch_rdata,
            s_axi_rresp    => m_axi_lch_rresp,
            s_axi_rlast    => m_axi_lch_rlast,
            s_axi_ruser    => m_axi_lch_ruser,
            s_axi_rvalid   => m_axi_lch_rvalid,
            s_axi_rready   => m_axi_lch_rready,
            s_axi_awid     => m_axi_lch_awid,
            s_axi_awaddr   => m_axi_lch_awaddr(9 downto 0),
            s_axi_awlen    => m_axi_lch_awlen,
            s_axi_awsize   => m_axi_lch_awsize,
            s_axi_awburst  => m_axi_lch_awburst,
            s_axi_awlock   => m_axi_lch_awlock,
            s_axi_awcache  => m_axi_lch_awcache,
            s_axi_awprot   => m_axi_lch_awprot,
            s_axi_awqos    => m_axi_lch_awqos,
            s_axi_awregion => (others => '0'),
            s_axi_awuser   => m_axi_lch_awuser,
            s_axi_awvalid  => m_axi_lch_awvalid,
            s_axi_awready  => m_axi_lch_awready,
            s_axi_wdata    => m_axi_lch_wdata,
            s_axi_wstrb    => m_axi_lch_wstrb,
            s_axi_wlast    => m_axi_lch_wlast,
            s_axi_wuser    => m_axi_lch_wuser,
            s_axi_wvalid   => m_axi_lch_wvalid,
            s_axi_wready   => m_axi_lch_wready,
            s_axi_bid      => m_axi_lch_bid,
            s_axi_bresp    => m_axi_lch_bresp,
            s_axi_buser    => m_axi_lch_buser,
            s_axi_bvalid   => m_axi_lch_bvalid,
            s_axi_bready   => m_axi_lch_bready
            );


    I_SLAVEMODEL_R : entity xil_defaultlib.s_axi_model
        port map (
            s_axi_aclk     => m_axi_rch_aclk,
            s_axi_aresetn  => m_axi_rch_aresetn,
            s_axi_arid     => m_axi_rch_arid,
            s_axi_araddr   => m_axi_rch_araddr(9 downto 0),
            s_axi_arlen    => m_axi_rch_arlen,
            s_axi_arsize   => m_axi_rch_arsize,
            s_axi_arburst  => m_axi_rch_arburst,
            s_axi_arlock   => m_axi_rch_arlock,
            s_axi_arcache  => m_axi_rch_arcache,
            s_axi_arprot   => m_axi_rch_arprot,
            s_axi_arqos    => m_axi_rch_arqos,
            s_axi_arregion => (others => '0'),
            s_axi_aruser   => m_axi_rch_aruser,
            s_axi_arvalid  => m_axi_rch_arvalid,
            s_axi_arready  => m_axi_rch_arready,
            s_axi_rid      => m_axi_rch_rid,
            s_axi_rdata    => m_axi_rch_rdata,
            s_axi_rresp    => m_axi_rch_rresp,
            s_axi_rlast    => m_axi_rch_rlast,
            s_axi_ruser    => m_axi_rch_ruser,
            s_axi_rvalid   => m_axi_rch_rvalid,
            s_axi_rready   => m_axi_rch_rready,
            s_axi_awid     => m_axi_rch_awid,
            s_axi_awaddr   => m_axi_rch_awaddr(9 downto 0),
            s_axi_awlen    => m_axi_rch_awlen,
            s_axi_awsize   => m_axi_rch_awsize,
            s_axi_awburst  => m_axi_rch_awburst,
            s_axi_awlock   => m_axi_rch_awlock,
            s_axi_awcache  => m_axi_rch_awcache,
            s_axi_awprot   => m_axi_rch_awprot,
            s_axi_awqos    => m_axi_rch_awqos,
            s_axi_awregion => (others => '0'),
            s_axi_awuser   => m_axi_rch_awuser,
            s_axi_awvalid  => m_axi_rch_awvalid,
            s_axi_awready  => m_axi_rch_awready,
            s_axi_wdata    => m_axi_rch_wdata,
            s_axi_wstrb    => m_axi_rch_wstrb,
            s_axi_wlast    => m_axi_rch_wlast,
            s_axi_wuser    => m_axi_rch_wuser,
            s_axi_wvalid   => m_axi_rch_wvalid,
            s_axi_wready   => m_axi_rch_wready,
            s_axi_bid      => m_axi_rch_bid,
            s_axi_bresp    => m_axi_rch_bresp,
            s_axi_buser    => m_axi_rch_buser,
            s_axi_bvalid   => m_axi_rch_bvalid,
            s_axi_bready   => m_axi_rch_bready
            );



-- clock generation
    clk <= (not clk and not stop_clock) after AXI_CLK_T/2;

-- Clock & Reset connections
    s_axi_aclk      <= clk;
    m_axi_conf_aclk <= clk;
    s_axis_lch_aclk <= clk;
    s_axis_rch_aclk <= clk;
    m_axi_lch_aclk  <= clk;
    m_axi_rch_aclk  <= clk;
    m_axis_lch_aclk <= clk;
    m_axis_rch_aclk <= clk;

    s_axi_aresetn      <= resetn;
    m_axi_conf_aresetn <= resetn;
    s_axis_lch_aresetn <= resetn;
    s_axis_rch_aresetn <= resetn;
    m_axi_lch_aresetn  <= resetn;
    m_axi_rch_aresetn  <= resetn;
    m_axis_lch_aresetn <= resetn;
    m_axis_rch_aresetn <= resetn;

-- BFM signal connections
    s_bfm_in_r                                                                                        <= (s_axi_aclk, s_axi_arready, s_axi_rready, s_axi_rvalid);
    (s_axi_araddr, s_axi_arvalid, s_axi_rready)                                                       <= s_bfm_out_r;
    s_bfm_in_w                                                                                        <= (s_axi_aclk, s_axi_awready, s_axi_wready, s_axi_bvalid);
    (s_axi_awaddr, s_axi_wdata, s_axi_wstrb, s_axi_awprot, s_axi_awvalid, s_axi_wvalid, s_axi_bready) <= s_bfm_out_w;

    m_bfm_in_r                                                 <= (m_axi_conf_aclk, m_axi_conf_arvalid, m_axi_conf_rready);
    (m_axi_conf_arready, m_axi_conf_rvalid, m_axi_conf_rdata)  <= m_bfm_out_r;
    m_bfm_in_w                                                 <= (m_axi_conf_aclk, m_axi_conf_awvalid, m_axi_conf_wvalid, m_axi_conf_wdata, m_axi_conf_bready);
    (m_axi_conf_awready, m_axi_conf_wready, m_axi_conf_bvalid) <= m_bfm_out_w;


-- Stimuli
    main : process
        procedure init_values is
        begin
            resetn <= '0';
            wait for (5*AXI_CLK_T);
            resetn <= '1';
            wait for (5*AXI_CLK_T);
        end init_values;


        procedure rx_data (signal tvalid : out std_logic; signal tdata : inout std_logic_vector(63 downto 0); constant RX_DATA_AMOUNT : natural := 128) is
begin
wait until (rising_edge(s_axis_lch_aclk));
tvalid <= '1';
wait for AXI_CLK_T;
for i in 1 to RX_DATA_AMOUNT loop
tdata <= std_logic_vector(unsigned(tdata) + to_unsigned(1, 64));
wait for AXI_CLK_T;
end loop;
tvalid <= '0';
wait for AXI_CLK_T;
end procedure rx_data;



begin
init_values;
read_control_reg(s_bfm_in_r, s_bfm_out_r);
read_status_reg(s_bfm_in_r, s_bfm_out_r);
read_version_reg(s_bfm_in_r, s_bfm_out_r);
read_counters(s_bfm_in_r, s_bfm_out_r);
write_control_reg_system_enable(s_bfm_in_w, s_bfm_out_w);

write_converter_setup_reg(s_bfm_in_w, s_bfm_out_w, x"0000_0000");  -- Setup as S2MM
rx_data(s_axis_lch_tvalid, s_axis_lch_tdata, 32);
rx_data(s_axis_rch_tvalid, s_axis_rch_tdata, 128);
read_counters(s_bfm_in_r, s_bfm_out_r);

wait for (50*AXI_CLK_T);
write_converter_setup_reg(s_bfm_in_w, s_bfm_out_w, x"0000_0003");  -- Setup as MM2S
write_mm2s_size_reg(s_bfm_in_w, s_bfm_out_w, x"0000_0020");

wait for (50*AXI_CLK_T);
write_master_lite_write_request(s_bfm_in_w, s_bfm_out_w, x"0000_ADD0", x"BEBA_CAFE");
master_write_sim(m_bfm_in_w, m_bfm_out_w);
write_master_lite_read_request(s_bfm_in_w, s_bfm_out_w, x"0000_ADD0");
master_read_sim(m_bfm_in_r, m_bfm_out_r, x"BEBA_CAFE");

write_control_reg_system_stop(s_bfm_in_w, s_bfm_out_w);
write_control_reg_soft_reset(s_bfm_in_w, s_bfm_out_w);
wait for (500*AXI_CLK_T);
end_test_and_stop_clock(stop_clock);
end process main;



end architecture TB;
