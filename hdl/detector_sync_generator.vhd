-- EAV and SAV detector, that generates sync signals
library ieee;
use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;
entity detector_sync_generator is
    port (
        parallel_data_in : in std_logic_vector(15 downto 0);
        lvds_clk_in      : in std_logic;
        reset_i          : in std_logic;

        lv_reg_o       : out std_logic;
        fv_reg_o       : out std_logic;
        delayed_data_o : out std_logic_vector(15 downto 0)
    );
end detector_sync_generator;

architecture behav of detector_sync_generator is
    constant SAV_AD : std_logic_vector(7 downto 0) := x"80";
    constant EAV_AD : std_logic_vector(7 downto 0) := x"9D";

    constant SAV_NAD : std_logic_vector(7 downto 0) := x"AB";
    constant EAV_NAD : std_logic_vector(7 downto 0) := x"B6";

    type syncs_del is array (natural range 3 downto 0) of
    std_logic_vector(1 downto 0);
    type delayed_data is array (natural range 4 downto 0) of
    std_logic_vector(15 downto 0);

    signal syncs_del_reg  : syncs_del;
    signal syncs_del_next : syncs_del;
    signal data_reg       : delayed_data;
    signal data_next      : delayed_data;

    signal lv_next : std_logic;
    signal fv_next : std_logic;
    signal lv_reg  : std_logic;
    signal fv_reg  : std_logic;
begin

    process (lvds_clk_in, reset_i)
    begin
        if reset_i = '1' then
            lv_reg           <= '0';
            fv_reg           <= '0';
        elsif rising_edge(lvds_clk_in) then
            lv_reg           <= lv_next;
            fv_reg           <= fv_next;
        end if;
    end process;

    syncs_del_reg(3) <= lv_reg & fv_reg;
    syncs_Delay : for i in 2 downto 0 generate
        process (lvds_clk_in, reset_i)
        begin
            if reset_i = '1' then
                syncs_del_reg(i) <= (others => '0');
            elsif rising_edge(lvds_clk_in) then
                syncs_del_reg(i) <= syncs_del_next(i);
            end if;
        end process;
        syncs_del_next(i) <= syncs_del_reg(i + 1);
    end generate syncs_Delay;
    lv_reg_o <= syncs_del_reg(0)(1);
    fv_reg_o <= syncs_del_reg(0)(0);

    data_reg(4)    <= parallel_data_in;
    delayed_data_o <= data_reg(0);
    data_delay : for i in 3 downto 0 generate
        process (lvds_clk_in, reset_i)
        begin
            if reset_i = '1' then
                data_reg(i) <= (others => '0');
            elsif rising_edge(lvds_clk_in) then
                data_reg(i) <= data_next(i);
            end if;
        end process;
        data_next(i) <= data_reg(i + 1);
    end generate data_delay;

    process (all)
    begin
        fv_next <= fv_reg;
        lv_next <= lv_reg; -- lets try to change sequence to opposite
        if data_reg(0)(15 downto 8) = x"FF" or data_reg(1)(15 downto 8) = x"00" or data_reg(2)(15 downto 8) = x"00" then
            case data_reg(3)(15 downto 8) is
                when SAV_AD =>
                    fv_next <= '1';
                    lv_next <= '1';
                when EAV_AD =>
                    fv_next <= '1'; 
                    lv_next <= '0';
                when SAV_NAD =>
                    fv_next <= '0';
                    lv_next <= '0';
                when EAV_NAD =>
                    fv_next <= '0';
                    lv_next <= '0';
                when others =>
            end case;
        end if;
    end process;
end architecture;