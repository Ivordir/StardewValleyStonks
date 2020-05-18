using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class ForageAmount : BaseCropAmount
    {
		private readonly SkillsState Skills;

		private readonly Dictionary<IItem, Amount> Amounts;

		public override void SetAmounts(int fertQuality = 0)
		{
			double[] dist = Dist;
			//foreach(ItemAmount[] amount in Amounts)
			//{
			//	for (int quality = 0; quality < dist.Length; quality++)
			//	{
			//		amount[quality].Amount = dist[quality] * (AvgCrops + AvgExtraCrops) / Amounts.Length;
			//	}
			//}
		}

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
