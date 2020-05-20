namespace StardewValleyStonks
{
    public class CropDistribution : ICropDistribution
    {
		public void SetAmounts(int fertQuality = 0)
		{
			double[] dist = CreateDist(fertQuality);
			for (int quality = 0; quality < dist.Length; quality++)
			{
				Distribution[quality].Value = dist[quality] * AvgCrops;
			}
			Distribution[0].Value += AvgExtraCrops;
		}

		private readonly Amount[] Distribution;
		private readonly Skill Farming;
		private readonly SettingsState Settings;
		private readonly double ExtraCrops;
		private readonly bool DoubleChance, Giant;
		private double AvgCrops => 1 - GiantCropProbability;
		private double AvgExtraCrops => (DoubleChance ? (ExtraCrops + AvgCrops) * Settings.DoubleCropChance : 0) + ExtraCrops + GiantCrops;
		private double GiantCrops => Giant ? 2 * GiantCropProbability : 0;
		private double GiantCropProbability => 1 - System.Math.Pow(0.99, Settings.GiantCropChecksPerTile);

		private double[] CreateDist(int fertQuality)
		{
			double[] dist = new double[3];
			dist[2] = 0.01 + 0.2 * (Farming.BuffedLevel / 10.0 + fertQuality * (Farming.BuffedLevel + 2) / 12.0);
			dist[1] = System.Math.Min(2 * dist[2], 0.75) * (1 - dist[2]);
			dist[0] = 1 - dist[1] - dist[2];
			return dist;
		}
	}
}
