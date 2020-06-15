using ExtentionsLibrary.Limits;
using ExtentionsLibrary.Collections;
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

        public double[] Seeds { get; }
        //heccing ancient fruit exception
        public double[] AncientFruitSeeds { get; }
        public bool QualitySeedMaker { get; set; }
        public int SeedsByQuality(int quality)
            => QualitySeeds[quality];
        public void SetSeedsByQuality(int quality, int value)
            => QualitySeeds[quality] = value.WithMin(0);

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
        readonly int[] QualitySeeds;
        readonly double AncientFruitBonusSeeds;

        public void Save()
        {
            if (QualitySeedMaker)
            {
                for (int quality = 0; quality < Qualities.Count; quality++)
                {
                    Seeds[quality] = SeedProb * QualitySeeds[quality];
                    AncientFruitSeeds[quality] = SeedProb * QualitySeeds[quality] + AncientFruitBonusSeeds;
                }
            }
            else
            {
                Seeds.SetAll(2 * SeedProb);
                AncientFruitSeeds.SetAll(2 * SeedProb + AncientFruitBonusSeeds);
            }
        }

        public Settings()
        {
            GiantCropChance = 0.01;
            _GiantCropChecksPerTile = 8;

            QualitySeeds = new int[Qualities.Count];
            for (int quality = 0; quality < Qualities.Count; quality++)
            {
                QualitySeeds[quality] = 2 + quality;
            }
            SeedProb = 0.975;
            Seeds = new double[Qualities.Count];
            AncientFruitSeeds = new double[Qualities.Count];
            AncientFruitBonusSeeds = 0.005;

            StaringFert = null;

            Save();
        }
    }
}
