import HasKAL.FrameUtils.FrameUtils

main :: IO()
main = do
    channelList <- getChannelList "ligo/L-L1_RDS_R_L1-959299648-64.gwf"
    print channelList
    gpstime <- getGPSTime "ligo/L-L1_RDS_R_L1-959299648-64.gwf"
    print $ fst gpstime
    fs <- getSamplingFrequency "H-H2_RDS_C03_L2-877260319-128.gwf" "H2:LSC-STRAIN"
    print fs

