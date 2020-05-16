namespace StardewValleyStonks
{
    public class SettingsState
    {
        /*
        List<Crop> crops
        List<Crop> regrowCrops
        List<Fertilizer> ferts
        */

        public bool SpecialCharm { get; set; }
        public int LuckBuff
        {
            get => LuckBuff;
            set => value.WithMin(0);
        }
        public double DoubleCropChance => 0.0000999999974737875 + LuckBuff / 1500.0 + (SpecialCharm ? 0.0234375005122273718774982435775 / 1200 : 0);
        
        public double SeedsFromSeedMaker
        {
            get => SeedsFromSeedMaker;
            set
            {
                SeedsFromSeedMaker = value;
                foreach(Reference<double> reference in QualitySeedsFromSeedMaker)
                {
                    reference.Value = value;
                }
            }
        }
        public bool QualitySeedMaker { get; set; }
        public Reference<double>[] QualitySeedsFromSeedMaker;

        public double GiantCropChecksPerTile
        {
            get => GiantCropChecksPerTile;
            set => value.InRange(0, 9);
        }
        public bool GreenhouseMode { get; set; }
        //public bool TrelisPenalty;
        public Fertilizer StaringFert { get; set; }
        
        public SettingsState(bool test)
        {
            StaringFert = null;
            SpecialCharm = false;
            LuckBuff = 0;
            GiantCropChecksPerTile = 9;
            GreenhouseMode = false;
            SeedsFromSeedMaker = 2;
            QualitySeedsFromSeedMaker = new Reference<double>[4];
            for(int quality = 0; quality < 4; quality++)
            {
                QualitySeedsFromSeedMaker[quality] = new Reference<double>(SeedsFromSeedMaker);
            }
        }
    }
}
