library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
library xil_defaultlib;
use xil_defaultlib.global.all;
use xil_defaultlib.axil_slave_bfm.all;
use xil_defaultlib.axil_master_bfm.all;

package global_sim is

    -- Clock constants
    constant AXI_CLK_T : time := 6.4 ns;
    constant FS_CLK_T  : time := 22675 ns;  -- 44,1 Khz

    -- AXI-Lite reg offsets
    constant CONTROL_REG_ADDR              : std_logic_vector(31 downto 0) := x"0000_0000";
    constant STATUS_REG_ADDR               : std_logic_vector(31 downto 0) := x"0000_0004";
    constant VERSION_REG_ADDR              : std_logic_vector(31 downto 0) := x"0000_0008";
    constant CONVERTER_SETUP_REG_ADDR      : std_logic_vector(31 downto 0) := x"0000_000C";
    constant MM2S_SIZE_REG_ADDR            : std_logic_vector(31 downto 0) := x"0000_0010";
    constant MASTER_LITE_WR_SETUP_REG_ADDR : std_logic_vector(31 downto 0) := x"0000_0014";
    constant MASTER_LITE_WR_ADDR_REG_ADDR  : std_logic_vector(31 downto 0) := x"0000_0018";
    constant MASTER_LITE_WR_DATA_REG_ADDR  : std_logic_vector(31 downto 0) := x"0000_001C";
    constant MASTER_LITE_RD_SETUP_REG_ADDR : std_logic_vector(31 downto 0) := x"0000_0020";
    constant MASTER_LITE_RD_ADD_REG_ADDR   : std_logic_vector(31 downto 0) := x"0000_0024";
    constant MASTER_LITE_RD_DATA_REG_ADDR  : std_logic_vector(31 downto 0) := x"0000_0028";
    constant COUNT_LCH_REG_ADDR            : std_logic_vector(31 downto 0) := x"0000_002C";
    constant PATTERN_COUNT_LCH_REG_ADDR    : std_logic_vector(31 downto 0) := x"0000_0030";
    constant COUNT_RCH_REG_ADDR            : std_logic_vector(31 downto 0) := x"0000_0034";
    constant PATTERN_COUNT_RCH_REG_ADDR    : std_logic_vector(31 downto 0) := x"0000_0038";

    -----------------------
    -- Procedure headers --
    -----------------------
    -- Read
    procedure read_control_reg (signal bfm_in_r            : in s_common_response_r_in; signal bfm_out_r : out s_common_response_r_out);
    procedure read_status_reg (signal bfm_in_r             : in s_common_response_r_in; signal bfm_out_r : out s_common_response_r_out);
    procedure read_version_reg (signal bfm_in_r            : in s_common_response_r_in; signal bfm_out_r : out s_common_response_r_out);
    procedure read_counters (signal bfm_in_r               : in s_common_response_r_in; signal bfm_out_r : out s_common_response_r_out);
    procedure read_master_lite_rd_data_reg(signal bfm_in_r : in s_common_response_r_in; signal bfm_out_r : out s_common_response_r_out);

    -- Write
    procedure write_control_reg(signal bfm_in_w              : in s_common_response_w_in; signal bfm_out_w : out s_common_response_w_out; constant DATA : in std_logic_vector(31 downto 0));
    procedure write_converter_setup_reg(signal bfm_in_w      : in s_common_response_w_in; signal bfm_out_w : out s_common_response_w_out; constant DATA : in std_logic_vector(31 downto 0));
    procedure write_mm2s_size_reg(signal bfm_in_w            : in s_common_response_w_in; signal bfm_out_w : out s_common_response_w_out; constant DATA : in std_logic_vector(31 downto 0));
    procedure write_master_lite_wr_setup_reg(signal bfm_in_w : in s_common_response_w_in; signal bfm_out_w : out s_common_response_w_out; constant DATA : in std_logic_vector(31 downto 0));
    procedure write_master_lite_wr_add_reg(signal bfm_in_w   : in s_common_response_w_in; signal bfm_out_w : out s_common_response_w_out; constant DATA : in std_logic_vector(31 downto 0));
    procedure write_master_lite_wr_data_reg(signal bfm_in_w  : in s_common_response_w_in; signal bfm_out_w : out s_common_response_w_out; constant DATA : in std_logic_vector(31 downto 0));
    procedure write_master_lite_rd_setup_reg(signal bfm_in_w : in s_common_response_w_in; signal bfm_out_w : out s_common_response_w_out; constant DATA : in std_logic_vector(31 downto 0));
    procedure write_master_lite_rd_add_reg(signal bfm_in_w   : in s_common_response_w_in; signal bfm_out_w : out s_common_response_w_out; constant DATA : in std_logic_vector(31 downto 0));

    -- Custom procedures
    procedure write_control_reg_system_enable (signal bfm_in_w : in s_common_response_w_in; signal bfm_out_w : out s_common_response_w_out);
    procedure write_control_reg_system_stop (signal bfm_in_w   : in s_common_response_w_in; signal bfm_out_w : out s_common_response_w_out);
    procedure write_control_reg_soft_reset (signal bfm_in_w    : in s_common_response_w_in; signal bfm_out_w : out s_common_response_w_out);
    procedure write_master_lite_write_request (signal bfm_in_w : in s_common_response_w_in; signal bfm_out_w : out s_common_response_w_out; constant ADDR : in std_logic_vector(31 downto 0); constant DATA : in std_logic_vector(31 downto 0));
    procedure write_master_lite_read_request (signal bfm_in_w  : in s_common_response_w_in; signal bfm_out_w : out s_common_response_w_out; constant ADDR : in std_logic_vector(31 downto 0));

    -- Others
    procedure end_test_and_stop_clock(signal stop_clock : out std_logic);

end package global_sim;



package body global_sim is

    -- Others
    procedure end_test_and_stop_clock (signal stop_clock : out std_logic) is
    begin
        assert false report "Test finished" severity note;
        stop_clock <= '1';
        wait;
    end end_test_and_stop_clock;


    -- Read procedures
    procedure read_control_reg (signal bfm_in_r : in s_common_response_r_in; signal bfm_out_r : out s_common_response_r_out) is
    begin
        slave_read_sim(bfm_in_r, bfm_out_r, CONTROL_REG_ADDR);
        wait for (5*AXI_CLK_T);
    end procedure read_control_reg;


    procedure read_status_reg (signal bfm_in_r : in s_common_response_r_in; signal bfm_out_r : out s_common_response_r_out) is
    begin
        slave_read_sim(bfm_in_r, bfm_out_r, STATUS_REG_ADDR);
        wait for (5*AXI_CLK_T);
    end procedure read_status_reg;


    procedure read_version_reg (signal bfm_in_r : in s_common_response_r_in; signal bfm_out_r : out s_common_response_r_out) is
    begin
        slave_read_sim(bfm_in_r, bfm_out_r, VERSION_REG_ADDR);
        wait for (5*AXI_CLK_T);
    end procedure read_version_reg;


    procedure read_counters (signal bfm_in_r : in s_common_response_r_in; signal bfm_out_r : out s_common_response_r_out) is
    begin
        slave_read_sim(bfm_in_r, bfm_out_r, COUNT_LCH_REG_ADDR);
        slave_read_sim(bfm_in_r, bfm_out_r, PATTERN_COUNT_LCH_REG_ADDR);
        slave_read_sim(bfm_in_r, bfm_out_r, COUNT_RCH_REG_ADDR);
        slave_read_sim(bfm_in_r, bfm_out_r, PATTERN_COUNT_RCH_REG_ADDR);
        wait for (5*AXI_CLK_T);
    end procedure read_counters;


    procedure read_master_lite_rd_data_reg(signal bfm_in_r : in s_common_response_r_in; signal bfm_out_r : out s_common_response_r_out) is
    begin
        slave_read_sim(bfm_in_r, bfm_out_r, MASTER_LITE_RD_DATA_REG_ADDR);
        wait for (5*AXI_CLK_T);
    end procedure read_master_lite_rd_data_reg;


    -- Write procedures
    procedure write_control_reg (
        signal bfm_in_w  : in  s_common_response_w_in;
        signal bfm_out_w : out s_common_response_w_out;
        constant DATA    : in  std_logic_vector(31 downto 0)
        ) is
    begin
        slave_write_sim(bfm_in_w, bfm_out_w, CONTROL_REG_ADDR, DATA);
        wait for (5*AXI_CLK_T);
    end procedure write_control_reg;


    procedure write_converter_setup_reg(
        signal bfm_in_w  : in  s_common_response_w_in;
        signal bfm_out_w : out s_common_response_w_out;
        constant DATA    : in  std_logic_vector(31 downto 0)
        ) is
    begin
        slave_write_sim(bfm_in_w, bfm_out_w, CONVERTER_SETUP_REG_ADDR, DATA);
        wait for (5*AXI_CLK_T);
    end procedure write_converter_setup_reg;


    procedure write_mm2s_size_reg(
        signal bfm_in_w  : in  s_common_response_w_in;
        signal bfm_out_w : out s_common_response_w_out;
        constant DATA    : in  std_logic_vector(31 downto 0)
        ) is
    begin
        slave_write_sim(bfm_in_w, bfm_out_w, MM2S_SIZE_REG_ADDR, DATA);
        wait for (5*AXI_CLK_T);
    end procedure write_mm2s_size_reg;


    procedure write_master_lite_wr_setup_reg(
        signal bfm_in_w  : in  s_common_response_w_in;
        signal bfm_out_w : out s_common_response_w_out;
        constant DATA    : in  std_logic_vector(31 downto 0)
        ) is
    begin
        slave_write_sim(bfm_in_w, bfm_out_w, MASTER_LITE_WR_SETUP_REG_ADDR, DATA);
        wait for (5*AXI_CLK_T);
    end procedure write_master_lite_wr_setup_reg;


    procedure write_master_lite_wr_add_reg(
        signal bfm_in_w  : in  s_common_response_w_in;
        signal bfm_out_w : out s_common_response_w_out;
        constant DATA    : in  std_logic_vector(31 downto 0)
        ) is
    begin
        slave_write_sim(bfm_in_w, bfm_out_w, MASTER_LITE_WR_ADDR_REG_ADDR, DATA);
        wait for (5*AXI_CLK_T);
    end procedure write_master_lite_wr_add_reg;


    procedure write_master_lite_wr_data_reg(
        signal bfm_in_w  : in  s_common_response_w_in;
        signal bfm_out_w : out s_common_response_w_out;
        constant DATA    : in  std_logic_vector(31 downto 0)
        ) is
    begin
        slave_write_sim(bfm_in_w, bfm_out_w, MASTER_LITE_WR_DATA_REG_ADDR, DATA);
        wait for (5*AXI_CLK_T);
    end procedure write_master_lite_wr_data_reg;


    procedure write_master_lite_rd_setup_reg(
        signal bfm_in_w  : in  s_common_response_w_in;
        signal bfm_out_w : out s_common_response_w_out;
        constant DATA    : in  std_logic_vector(31 downto 0)
        ) is
    begin
        slave_write_sim(bfm_in_w, bfm_out_w, MASTER_LITE_RD_SETUP_REG_ADDR, DATA);
        wait for (5*AXI_CLK_T);
    end procedure write_master_lite_rd_setup_reg;


    procedure write_master_lite_rd_add_reg(
        signal bfm_in_w  : in  s_common_response_w_in;
        signal bfm_out_w : out s_common_response_w_out;
        constant DATA    : in  std_logic_vector(31 downto 0)
        ) is
    begin
        slave_write_sim(bfm_in_w, bfm_out_w, MASTER_LITE_RD_ADD_REG_ADDR, DATA);
        wait for (5*AXI_CLK_T);
    end procedure write_master_lite_rd_add_reg;


    -- Custom function procedures
    procedure write_control_reg_system_enable (
        signal bfm_in_w  : in  s_common_response_w_in;
        signal bfm_out_w : out s_common_response_w_out
        ) is
    begin
        write_control_reg(bfm_in_w, bfm_out_w, x"0000_0001");
        wait for (5*AXI_CLK_T);
    end procedure write_control_reg_system_enable;


    procedure write_control_reg_system_stop (
        signal bfm_in_w  : in  s_common_response_w_in;
        signal bfm_out_w : out s_common_response_w_out
        ) is
    begin
        write_control_reg(bfm_in_w, bfm_out_w, x"0000_0000");
        wait for (5*AXI_CLK_T);
    end procedure write_control_reg_system_stop;


    procedure write_control_reg_soft_reset (
        signal bfm_in_w  : in  s_common_response_w_in;
        signal bfm_out_w : out s_common_response_w_out
        ) is
    begin
        write_control_reg(bfm_in_w, bfm_out_w, x"8000_0000");
        wait for (5*AXI_CLK_T);
    end procedure write_control_reg_soft_reset;


    procedure write_master_lite_write_request (
        signal bfm_in_w  : in  s_common_response_w_in;
        signal bfm_out_w : out s_common_response_w_out;
        constant ADDR    : in  std_logic_vector(31 downto 0);
        constant DATA    : in  std_logic_vector(31 downto 0)
        ) is
    begin
        write_master_lite_wr_add_reg(bfm_in_w, bfm_out_w, ADDR);
        write_master_lite_wr_data_reg(bfm_in_w, bfm_out_w, DATA);
        write_master_lite_wr_setup_reg(bfm_in_w, bfm_out_w, x"0000_0001");
        wait for (5*AXI_CLK_T);
    end write_master_lite_write_request;


    procedure write_master_lite_read_request (
        signal bfm_in_w  : in  s_common_response_w_in;
        signal bfm_out_w : out s_common_response_w_out;
        constant ADDR    : in  std_logic_vector(31 downto 0)
        ) is
    begin
        write_master_lite_rd_add_reg(bfm_in_w, bfm_out_w, ADDR);
        write_master_lite_rd_setup_reg(bfm_in_w, bfm_out_w, x"0000_0001");
        wait for (5*AXI_CLK_T);
    end write_master_lite_read_request;

    


end package body global_sim;

