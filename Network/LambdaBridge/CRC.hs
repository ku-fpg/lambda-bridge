
-- Provide Packet Checking by appending a 16-bit CRCs
crc16Protocol :: (Bridge Packet integrity) -> IO (Bridge Packet Checked)
crc16Protocol bridge = do
