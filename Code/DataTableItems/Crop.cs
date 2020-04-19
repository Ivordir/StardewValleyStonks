﻿using Microsoft.AspNetCore.Components;
using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
	public class Crop : DataTableItem
	{
		private static readonly IPriceManager<IProductSource, IPricedItem>[] NoOtherProducts;

		static Crop()
		{
			NoOtherProducts = new IPriceManager<IProductSource, IPricedItem>[0];
		}

		[Inject] private SkillsState Skills { get; }

		public Season AllowedSeasons { get; }
		public Season SelectedSeasons { get; set; }
		public int TotalGrowthTime { get; }

		//find what should be protected and what should be private
		List<IPenalty> Penalties { get; }

		private readonly int[] GrowthStagesOrg;
		private readonly int[] GrowthStages;
		private readonly double AvgCrops, AvgExtraCrops;
		//typeof(QualityManager), [index][quality], harvestables with quality (i.e. crops), profit determined at runtime according to fertilizer.
		protected readonly IPriceManager<IProductSource, IPricedItem>[][] Crops;
		//typeof(PriceManager), [index], harvestables without any quality (i.e. sunflower seeds), profit determined before runtime.
		protected readonly IPriceManager<IProductSource, IPricedItem>[] OtherProducts;
		//typeof(???)
		protected readonly IRanker<ISource, IReplant> Replant;

		public Crop(
			string name,
			Season seasons,
			int[] growthStages,
			double avgCrops,
			double avgExtraCrops,
			IPriceManager<IProductSource, IPricedItem>[][] crops,
			IPriceManager<IProductSource, IPricedItem>[] otherProducts = null)
			: base(name)
		{
			GrowthStagesOrg = growthStages;
			GrowthStages = new int[GrowthStagesOrg.Length];
			ResetGrowthStages();
			for (int i = 0; i < GrowthStagesOrg.Length; i++)
			{
				TotalGrowthTime += GrowthStagesOrg[i];
			}

			AllowedSeasons = seasons;
			SelectedSeasons = AllowedSeasons;

			Crops = crops;
			OtherProducts = otherProducts ?? NoOtherProducts;

			AvgCrops = avgCrops;
			AvgExtraCrops = avgExtraCrops;
		}

		public virtual int GrowthTime(double speedMultiplier)
		{
			if (speedMultiplier == 0)
			{
				return TotalGrowthTime;
			}
			int maxReduction = (int)Math.Ceiling((TotalGrowthTime - GrowthStages[^1]) * speedMultiplier);
			int daysReduced = 0;
			for (int passes = 0; daysReduced < maxReduction && passes < 3; passes++)
			{
				for (int stage = 0; stage < GrowthStages.Length; stage++)
				{
					if (stage > 0 || GrowthStages[stage] > 1)
					{
						GrowthStages[stage]--;
						daysReduced++;
						if (maxReduction == daysReduced)
						{
							break;
						}
					}
				}
			}
			ResetGrowthStages();
			return TotalGrowthTime - daysReduced;
		}

		public virtual int HarvestsWithin(int days, double speed = 0)
		{
			return days / GrowthTime(speed);
		}

		public int HarvestsWithin(ref int days, double speed = 0)
		{
			int growthTime = GrowthTime(speed);
			int numHarvests = days / growthTime;
			days -= growthTime * numHarvests;
			return numHarvests;
		}

		public override bool Active
		{
			get
			{
				throw new NotImplementedException();
			}
		}

		private void ResetGrowthStages()
		{
			for (int i = 0; i < GrowthStagesOrg.Length; i++)
			{
				GrowthStages[i] = GrowthStagesOrg[i];
			}
		}

		public double GoldPerDay(Fertilizer fert)
		{
			return Profit(fert.Quality) / GrowthTime(fert.Speed);
		}

		public double Profit(int fertQuality, int harvests = 1)
		{
			double profit = 0;
			double[] dist = Dist(fertQuality);
			double[] extraDist = Dist(0);
			foreach (IPriceManager<IProductSource, IPricedItem>[] crop in Crops)
			{
				for (int quality = 0; quality < crop.Length; quality++)
				{
					crop[quality].Amount = dist[quality] * AvgCrops + extraDist[quality] * AvgExtraCrops;
					profit += crop[quality].Price;
				}
			}

			foreach (IPriceManager<IProductSource, IPricedItem> product in OtherProducts)
			{
				profit += product.Price;
			}

			double seeds = 1;
			foreach (List<KeyValuePair<ISource, IReplant>> rank in Replant.BestItems)
			{
				foreach (KeyValuePair<ISource, IReplant> replant in rank)
				{
					if (replant.Value.Seeds >= seeds)
					{
						profit -= seeds * replant.Value.UnitPrice;
						break;
					}
					seeds -= replant.Value.Seeds;
					profit -= replant.Value.Price;
				}
			}

			return profit * harvests;
		}
		//not in season
		//no products
		//no replant

		private double[] Dist(int fertQuality)
		{
			double[] dist = new double[3];
			dist[2] = 0.01 + 0.2 * (Skills.BuffedFarmLvl / 10 + fertQuality * (Skills.BuffedFarmLvl + 2) / 12); //gold //check for int division
			dist[1] = Math.Min(2 * dist[2], 0.75) * (1 - dist[2]); //silver
			dist[0] = 1 - dist[1] - dist[2]; //normal
			return dist;
		}
	}
}
