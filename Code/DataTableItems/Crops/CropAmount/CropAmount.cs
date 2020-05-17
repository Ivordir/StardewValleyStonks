namespace StardewValleyStonks
{
    public class CropAmount : BaseCropAmount
    {
		private readonly SkillsState Skills;

		private readonly ItemAmount[] Amounts;

		public override void SetAmounts(int fertQuality = 0)
		{
			double[] dist = Dist(fertQuality);
			for(int quality = 0; quality < dist.Length; quality++)
			{
				Amounts[quality].Amount = dist[quality] * AvgCrops;
			}
			Amounts[0].Amount += AvgExtraCrops;
		}

		private double[] Dist(int fertQuality)
		{
			double[] dist = new double[3];
			dist[2] = 0.01 + 0.2 * (Skills.Farming.BuffedLevel / 10.0 + fertQuality * (Skills.Farming.BuffedLevel + 2) / 12.0);
			dist[1] = System.Math.Min(2 * dist[2], 0.75) * (1 - dist[2]);
			dist[0] = 1 - dist[1] - dist[2];
			return dist;
		}
	}
}
