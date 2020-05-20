namespace StardewValleyStonks
{
    public class ForageDistribution : ICropDistribution
    {
		public void SetAmounts(int fertQuality = 0)
		{
			double[] dist = Dist;
			foreach (Amount[] distribution in Distributions)
			{
				for (int quality = 0; quality < dist.Length; quality++)
				{
					distribution[quality].Value = dist[quality] / Distributions.Length;
				}
			}
		}

		private readonly Amount[][] Distributions;
		private readonly SkillsState Skills;
		private double[] Dist
		{
			get
			{
				double[] dist = new double[4];
				if (Skills.Botanist.Active)
				{
					dist[3] = 1;
				}
				else
				{
					dist[2] = Skills.Foraging.BuffedLevel / 30.0;
					dist[1] = Skills.Foraging.BuffedLevel / 15.0 * (1 - dist[2]);
					dist[0] = 1 - dist[1] - dist[2];
				}
				if (Skills.Gatherer.Active)
				{
					for (int i = 0; i < dist.Length; i++)
					{
						dist[i] *= Skills.Gatherer.Value;
					}
				}
				return dist;
			}
		}
	}
}
