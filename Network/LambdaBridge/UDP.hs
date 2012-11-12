

-- Provide Packet Checking by using the UDP protocol with CRCs
udpProtocol :: (Bridge Packet integrity) -> IO (Bridge Packet Checked)
