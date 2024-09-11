-------------------------------------------------------------------------------
-- Title      : AXI Lite Master
-- Project    :
-------------------------------------------------------------------------------
-- File       : axi_lite_master.vhd
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
