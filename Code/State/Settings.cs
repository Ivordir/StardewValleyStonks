using ExtentionsLibrary.Limits;
using static StardewValleyStonks.Quality;

namespace StardewValleyStonks
{
    public class Settings
    {
        public bool SpecialCharm { get; set; }
        public int LuckBuff
        {
            get => _LuckBuff;
            set => _LuckBuff = value.WithMin(0);
        }
        //not sure if the "0.0001" is intended by Concerned Ape, or just something that the (de)compiler threw in there
        public double DoubleCropProb => 0.0001 + _LuckBuff / 1500 + (SpecialCharm ? 0.025 : 0);

        public bool QualitySeedMaker { get; set; }
        public int SeedsByQuality(int quality)
            => SeedAmounts[quality];
        public void SetSeedsByQuality(int quality, int value)
        {
            SeedAmounts[quality] = value.WithMin(0);
            SeedsDIO[quality] = SeedAmounts[quality] * SeedProb;
        }
        public double[] Seeds { get; }
        public double[] SeedsDIO { get; }

        public double GiantCropChecksPerTile
        {
            get => _GiantCropChecksPerTile;
            set
            {
                _GiantCropChecksPerTile = value.InRange(0, 9);
            }
        }
        public double NoGiantCropProb => System.Math.Pow(1 - GiantCropChance, _GiantCropChecksPerTile);

        public bool GreenhouseMode { get; set; }
        public FertilizerDIO StaringFert { get; set; }

        int _LuckBuff;
        double _GiantCropChecksPerTile;

        readonly double GiantCropChance;
        readonly double SeedProb;
        readonly int[] SeedAmounts;

        public void Save()
        {
            for (int quality = 0; quality < Qualities.Count; quality++)
            {
                Seeds[quality] = SeedsDIO[quality];
            }
        }

        public Settings()
        {
            GiantCropChance = 0.01;
            _GiantCropChecksPerTile = 8;

            SeedAmounts = new int[Qualities.Count];
            for (int quality = 0; quality < Qualities.Count; quality++)
            {
                SeedAmounts[quality] = 2;
            }
            SeedProb = 0.975;
            SeedsDIO = new double[Qualities.Count];
            for (int quality = 0; quality < Qualities.Count; quality++)
            {
                SeedsDIO[quality] = SeedAmounts[quality] * SeedProb;
            }
            Seeds = new double[Qualities.Count];
            Save();
            StaringFert = null;
        }
    }
}
