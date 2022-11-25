
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
-- use work.bus_pkg1.all;

entity ChaCha20Poly1305Decrypt is
Port ( 
        clk					: in  STD_LOGIC;
        rst					: in  STD_LOGIC;
-----------------------------
--  axi_st_in_data
		axi_tvalid_in_msg    : in  STD_LOGIC;
		axi_tlast_in_msg     : in  STD_LOGIC;
		axi_tdata_in_msg     : in  UNSIGNED(127 downto 0);
		axi_tready_in_msg    : out STD_LOGIC:='1';

		axi_key_in_msg     : in  UNSIGNED(127 downto 0);

--  axi_st_out
		axi_tvalid_out        : out  STD_LOGIC;
		axi_tlast_out         : out  STD_LOGIC;
		axi_tdata_out         : out  UNSIGNED(127 downto 0);
		axi_tready_out        : in STD_LOGIC;
------------------------------
-- additional ports		
        tag_valid             : out STD_LOGIC
);
end ChaCha20Poly1305Decrypt;

architecture arch of ChaCha20Poly1305Decrypt is

begin

axi_tvalid_out <= axi_tvalid_in_msg;
axi_tlast_out <= axi_tlast_in_msg;
axi_tdata_out <= axi_tdata_in_msg;
axi_tready_in_msg <= axi_tready_out;

end arch;

