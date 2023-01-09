library ieee;
use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;
--lvds_to_parallel specially for tamron
--for data lines pair 
-- --> 0_p
-- --> 1_p
-- --> clk_p

entity lvds_double_buffer is
    port (
        lvds_in0_p : in std_logic;
        lvds_in1_p : in std_logic;
        high_clk_i : in std_logic;
        reset_i    : in std_logic;
        cmos_o     : out std_logic_vector(7 downto 0)
    );
end lvds_double_buffer;

architecture behav of lvds_double_buffer is
    signal cnt_in_reg  : std_logic_vector(7 downto 0);
    signal cnt_in_next : std_logic_vector(7 downto 0);

    type lvds_reg is array(1 downto 0) of std_logic_vector(6 downto 0);
    signal main_memory_reg : lvds_reg;
    signal aux_memory_reg  : lvds_reg;
    type lvds_next is array(1 downto 0) of std_logic_vector(6 downto 0);
    signal main_memory_next : lvds_next;
    signal aux_memory_next  : lvds_next;

    signal out_reg  : std_logic_vector(7 downto 0);
    signal out_next : std_logic_vector(7 downto 0);

    type state_type is (idle, out_reg1, out_reg0, reg1_prefill);
    signal state_reg  : state_type;
    signal state_next : state_type;
begin

    process (high_clk_i, reset_i)
    begin
        if reset_i = '1' then
            for i in 1 downto 0 loop
                main_memory_reg(i) <= (others => '0');
                aux_memory_reg(i)  <= (others => '0');
            end loop;

            out_reg    <= (others => '0');
            cnt_in_reg <= (others => '0');
            state_reg  <= idle;
        elsif rising_edge(high_clk_i) then
            for i in 1 downto 0 loop
                main_memory_reg(i) <= main_memory_next(i);
                aux_memory_reg(i)  <= aux_memory_next(i);
            end loop;
            out_reg    <= out_next;
            cnt_in_reg <= cnt_in_next;
            state_reg  <= state_next;
        end if;
    end process;

    incnt_nextsstatelogic : process (all)
    begin
        if (to_integer(unsigned(cnt_in_reg)) = 6) then
            cnt_in_next <= std_logic_vector(to_unsigned(0, 8));
        else
            cnt_in_next <= std_logic_vector(unsigned(cnt_in_reg) + 1);
        end if;
    end process incnt_nextsstatelogic;

    aux_reg_add : process (all)
    begin
        for i in 1 downto 0 loop
            for j in 5 downto 0 loop
                main_memory_next(i)(j) <= main_memory_reg(i)(j + 1);
                aux_memory_next(i)(j)  <= aux_memory_reg(i)(j + 1);
            end loop;
        end loop;
    end process aux_reg_add;

    control : process (all)
    begin
        state_next             <= state_reg;
        out_next               <= out_reg;
        main_memory_next(1)(6) <= '0';
        aux_memory_next(1)(6)  <= '0';
        main_memory_next(0)(6) <= '0';
        aux_memory_next(0)(6)  <= '0';
        case state_reg is

            when idle =>
                if (to_integer(unsigned(cnt_in_reg)) = 6) then
                    state_next <= reg1_prefill;
                else
                    state_next <= state_reg;
                end if;
            when reg1_prefill => --depends on a counter starting point.
                -- so visiting this state can becontrol by the valid signal that controls counter
                main_memory_next(1)(6) <= lvds_in0_p;
                aux_memory_next(1)(6)  <= lvds_in1_p;
                if (to_integer(unsigned(cnt_in_reg)) = 6) then
                    state_next <= out_reg1;
                else
                    state_next <= state_reg;
                end if;
            when out_reg1 =>
                main_memory_next(0)(6) <= lvds_in0_p;
                aux_memory_next(0)(6)  <= lvds_in1_p;

                if (to_integer(unsigned(cnt_in_reg)) = 0) then
                    out_next <= aux_memory_reg(1)(0) & main_memory_reg(1);
                else
                    out_next <= out_reg;
                end if;

                if (to_integer(unsigned(cnt_in_reg)) = 6) then
                    state_next <= out_reg0;
                else
                    state_next <= state_reg;
                end if;
            when out_reg0 =>
                main_memory_next(1)(6) <= lvds_in0_p;
                aux_memory_next(1)(6)  <= lvds_in1_p;
                if (to_integer(unsigned(cnt_in_reg)) = 0) then
                    out_next <= aux_memory_reg(0)(0) & main_memory_reg(0);
                else
                    out_next <= out_reg;
                end if;
                if (to_integer(unsigned(cnt_in_reg)) = 6) then
                    state_next <= out_reg1;
                else
                    state_next <= state_reg;
                end if;

            when others =>
        end case;
    end process control;

    cmos_o <= out_reg;

end architecture;