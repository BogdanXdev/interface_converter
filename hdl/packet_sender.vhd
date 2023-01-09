library ieee;
use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;

entity packet_sender is
    port (
        reset_i      : in std_logic;
        px_data      : in std_logic_vector(15 downto 0);
        px_clk       : in std_logic;
        fv_i         : in std_logic;
        lv_i         : in std_logic;
        d_hs_ready_i : in std_logic;
        byte_clk     : in std_logic;
        c2d_ready_i  : in std_logic;
        phdr_i       : in std_logic;
        fv_e_r_i     : in std_logic;
        fv_s_r_i     : in std_logic;

        d_hs_en_o   : out std_logic;
        packet_en_o : out std_logic;
        packets_o   : out std_logic_vector (31 downto 0);
        lv_start_o  : out std_logic;
        lv_end_o    : out std_logic;
        fv_start_o  : out std_logic;
        fv_end_o    : out std_logic;
        sp_en_o     : out std_logic

    );
end packet_sender;

architecture behav of packet_sender is

    signal lv_reg      : std_logic;
    signal lv_next     : std_logic;
    signal fv_reg      : std_logic;
    signal fv_next     : std_logic;
    signal lv_reg1     : std_logic;
    signal lv_next1    : std_logic;
    signal fv_reg1     : std_logic;
    signal fv_next1    : std_logic;
    signal lv_reg2     : std_logic;
    signal lv_next2    : std_logic;
    signal fv_reg2     : std_logic;
    signal fv_next2    : std_logic;
    signal wr_en_fifo  : std_logic;
    signal rd_en_fifo  : std_logic;
    signal data_packet : std_logic;

    component row_fifo is
        port (
            wr_clk_i       : in std_logic;
            rd_clk_i       : in std_logic;
            rst_i          : in std_logic;
            rp_rst_i       : in std_logic;
            wr_en_i        : in std_logic;
            rd_en_i        : in std_logic;
            wr_data_i      : in std_logic_vector(15 downto 0);
            full_o         : out std_logic;
            empty_o        : out std_logic;
            almost_full_o  : out std_logic;
            almost_empty_o : out std_logic;
            rd_data_o      : out std_logic_vector(31 downto 0)
        );
    end component;

    type state_type is (wait_fs, d_hs_sp_ready_wait, d_hs_sp_fve_ready_wait, sp_en, sp_en_fve,
        phdr_fve_wait, phdr_wait, wait_lv_end, d_hs_en_assertion, d_hs_ready_wait);
    signal state_reg         : state_type := wait_fs;
    signal state_next        : state_type;
    signal almost_e          : std_logic;
    signal c2d_ready_reg     : std_logic;
    signal c2d_ready_next    : std_logic;
    signal d_hs_ready_reg    : std_logic;
    signal d_hs_ready_next   : std_logic;
    signal fv_mem            : std_logic;
    signal word_counter_next : std_logic_vector(12 downto 0);
    signal word_counter_reg  : std_logic_vector(12 downto 0);
    signal fifo_full         : std_logic;
    signal fifo_reset        : std_logic;
    signal px_data_reg       : std_logic_vector(15 downto 0);
    signal px_data_next      : std_logic_vector(15 downto 0);
    signal line_cnt_next     : std_logic_vector(15 downto 0);
    signal line_cnt_reg      : std_logic_vector(15 downto 0);
    signal rd_en_reg         : std_logic_vector(5 downto 0);
    signal rd_en_next        : std_logic_vector(5 downto 0);
begin
    process (px_clk, reset_i)
    begin
        if reset_i = '1' then
            lv_reg      <= '0';
            fv_reg      <= '0';
            lv_reg1     <= '0';
            fv_reg1     <= '0';
            lv_reg2     <= '0';
            fv_reg2     <= '0';
            px_data_reg <= (others => '0');

        elsif rising_edge(px_clk) then
            lv_reg      <= lv_next;
            fv_reg      <= fv_next;
            lv_reg1     <= lv_next1;
            fv_reg1     <= fv_next1;
            lv_reg2     <= lv_next2;
            fv_reg2     <= fv_next2;
            px_data_reg <= px_data_next;

        end if;
    end process;

    process (byte_clk, reset_i)
    begin
        if reset_i = '1' then
            c2d_ready_reg    <= '0';
            state_reg        <= wait_fs;
            d_hs_ready_reg   <= '0';
            word_counter_reg <= (others => '0');
            -- line_cnt_reg     <= (others => '0');
        elsif rising_edge(byte_clk) then
            c2d_ready_reg    <= c2d_ready_next;
            state_reg        <= state_next;
            d_hs_ready_reg   <= d_hs_ready_next;
            word_counter_reg <= word_counter_next;
            -- line_cnt_reg     <= line_cnt_next;
        end if;
    end process;

    px_data_next    <= px_data;
    c2d_ready_next  <= c2d_ready_i;
    d_hs_ready_next <= d_hs_ready_i;

    lv_next  <= lv_i;
    fv_next  <= fv_i;
    lv_next1 <= lv_reg;
    fv_next1 <= fv_reg;
    lv_next2 <= lv_reg1;
    fv_next2 <= fv_reg1;

    lv_start_o <= '1' when lv_reg > lv_reg2 else
        '0';
    lv_end_o <= '1' when lv_reg < lv_reg2 else
        '0';
    fv_start_o <= '1' when fv_reg > fv_reg2 else
        '0';
    fv_end_o <= '1' when fv_reg < fv_reg2 else
        '0';

    process (all)
    begin
        wr_en_fifo <= '0';
        if fv_reg = '1' and lv_reg = '1' then
            if fifo_full = '0' then
                wr_en_fifo <= '1';
            else
                wr_en_fifo <= '0';
            end if;
        else
            wr_en_fifo <= '0';
        end if;
    end process;

    mipi_ip_internal_protocol : process (all)
    begin
        sp_en_o           <= '0';
        d_hs_en_o         <= '0';
        packet_en_o       <= '0';
        state_next        <= state_reg;
        word_counter_next <= word_counter_reg;
        rd_en_fifo        <= '0';
        fifo_reset        <= '0';
        -- line_cnt_next     <= line_cnt_reg;
        case state_reg is
            when wait_fs =>
                if fv_s_r_i = '1' and fv_reg = '1' and lv_reg = '1' and c2d_ready_reg = '1' then
                    d_hs_en_o  <= '1';
                    state_next <= d_hs_sp_ready_wait;
                else
                end if;
                -- fifo_reset        <= '1';

            when d_hs_sp_ready_wait =>
                if d_hs_ready_i = '1' then
                    state_next <= sp_en;
                else
                end if;
            when d_hs_sp_fve_ready_wait =>
                fifo_reset <= '1';

                if d_hs_ready_i = '1' then
                    state_next <= sp_en_fve;
                else
                end if;
            when sp_en_fve =>
                sp_en_o    <= '1';
                state_next <= phdr_fve_wait;
                -- line_cnt_next <= (others => '0');
            when phdr_fve_wait =>
                if phdr_i = '1' then
                    state_next <= wait_fs;
                else
                end if;
            when sp_en =>
                sp_en_o    <= '1';
                state_next <= phdr_wait;
            when phdr_wait =>
                if phdr_i = '1' then
                    state_next <= wait_lv_end;
                else
                end if;
            when wait_lv_end =>
                if lv_end_o = '1' then
                    state_next <= d_hs_en_assertion;
                else
                end if;

            when d_hs_en_assertion =>
                if fv_s_r_i = '1' and fv_end_o = '0' and fv_reg = '1' and lv_reg = '1'
                    and c2d_ready_reg = '1' then
                    d_hs_en_o  <= '1';
                    state_next <= d_hs_sp_ready_wait;
                elsif fv_s_r_i = '0' and fv_end_o = '0' and fv_reg = '1' and lv_reg = '1'
                    and c2d_ready_reg = '1' then
                    d_hs_en_o  <= '1';
                    state_next <= d_hs_ready_wait;

                elsif fv_e_r_i = '1' and fv_reg = '0' and lv_reg = '0' and c2d_ready_reg = '1' then
                    state_next <= d_hs_sp_fve_ready_wait;
                    d_hs_en_o  <= '1';
                else
                end if;
            when d_hs_ready_wait =>
                if d_hs_ready_i = '1' then
                    packet_en_o <= '1';
                    if almost_e = '0' then
                        rd_en_fifo <= '1';
                    else
                        rd_en_fifo <= '0';
                    end if;
                    if rd_en_reg(4) = '1' then
                        if (to_integer(unsigned(word_counter_reg)) < 961) then
                            word_counter_next <= std_logic_vector(unsigned(word_counter_reg) + 1);
                        else
                            state_next        <= d_hs_en_assertion;
                            word_counter_next <= (others => '0');
                        end if;
                    else
                    end if;
                else
                    packet_en_o       <= '0';
                    word_counter_next <= (others => '0');
                end if;
            when others =>
        end case;

    end process mipi_ip_internal_protocol;

    rd_en_reg(4) <= rd_en_fifo;
    rd_en_del : for i in 3 downto 0 generate
        process (byte_clk, reset_i)
        begin
            if reset_i = '1' then
                rd_en_reg(i) <= '0';
            elsif rising_edge(byte_clk) then
                rd_en_reg(i) <= rd_en_next(i);
            end if;
        end process;
        rd_en_next(i) <= rd_en_reg(i + 1);
    end generate rd_en_del;

    packets_fifo : row_fifo port map(
        wr_clk_i       => px_clk,
        rd_clk_i       => byte_clk,
        rst_i          => reset_i or fifo_reset,
        rp_rst_i       => open,
        wr_en_i        => wr_en_fifo,
        rd_en_i        => rd_en_reg(0),
        wr_data_i      => px_data_reg,
        full_o         => fifo_full,
        empty_o        => open,
        almost_full_o  => open,
        almost_empty_o => almost_e,
        rd_data_o      => packets_o
    );

end architecture behav;