using System.Collections.Generic;
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
        public double DoubleCropProb => 0.0000999999974737875 + _LuckBuff / 1500 + (SpecialCharm ? 0.025000000372529 : 0);

        public bool QualitySeedMaker { get; set; }
        public int SeedsByQuality(int quality)
            => (int)_Seeds[quality].Value;
        public void SetSeedsByQuality(int quality, int value)
            => _Seeds[quality].Value = value.WithMin(0);
        public IValue[] SeedAmounts { get; }

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
        readonly RefValue SeedProb;
        readonly RefValue[] _Seeds;

        public Settings()
        {
            GiantCropChance = 0.01;

            _Seeds = new RefValue[Qualities.Count];
            for (int quality = 0; quality < Qualities.Count; quality++)
            {
                _Seeds[quality] = (RefValue)2;
            }
            SeedProb = new RefValue(0.975);
            SeedAmounts = new IValue[Qualities.Count];
            for (int quality = 0; quality < Qualities.Count; quality++)
            {
                SeedAmounts[quality] = new Term(SeedProb, _Seeds[quality]);
            }

            StaringFert = null;
        }
    }
}
