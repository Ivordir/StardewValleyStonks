using Microsoft.AspNetCore.Components;
using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
	public class Crop : DataTableItem
	{
		[Inject] private SkillsState Skills { get; }
		[Inject] private SettingsState Settings { get; }

		private readonly IGrow Grow;
		public bool Regrows => Grow is IRegrow;
		public int RegrowTime => ((IRegrow)Grow).RegrowTime;
		public int GrowthTime => Grow.TotalTime;
		public int GrowthTimeWith(double speed) => Grow.Time(speed);
		public int HarvestsWithin(int days, double speed = 0) => Grow.HarvestsWithin(days, speed);
		public int HarvestsWithin(ref int days, double speed = 0) => Grow.HarvestsWithin(ref days, speed);

		private readonly double ExtraCrops;

		private readonly bool Scythe;
		private readonly bool Giant;
		private readonly bool Indoors;
		//find what should be protected and what should be private
		//List<IPenalty> Penalties { get; }

		protected readonly IPriceFinder<IProductSource, IProduct>[] Products;
		protected readonly IRanker<ISource, IReplant> Replant;

		public Crop(
			string name,
			IPriceFinder<IProductSource, IProduct>[] products,
			IRanker<ISource, IReplant> replant)
			: base(name)
		{

			Products = products;
			Replant = replant;
		}


		public override bool Active
		{
			get
			{
				//not in season
				//no products
				//no replant
				throw new NotImplementedException();
			}
		}

		

		public double GoldPerDay(Fertilizer fert)
		{
			return Profit(fert.Quality) / Grow.Time(fert.Speed);
		}

		public virtual double Profit(int fertQuality = 0, int harvests = 1)
		{
			return (ProfitPerHarvest(fertQuality) - ReplantCost) * harvests;
		}

		protected virtual double ProfitPerHarvest(int fertQuality = 0)
		{
			double profit = 0;
			double[] dist = Dist(fertQuality);
			double[] extraDist = Dist(0);
			for (int quality = 0; quality < Products.Length; quality++)
			{
				Products[quality].Amount = dist[quality] * AvgCrops + extraDist[quality] * AvgExtraCrops;
				profit += Products[quality].Price;
			}
			return profit;
		}

		private double AvgCrops => 1 - GiantCrops;
		private double AvgExtraCrops => (Scythe ? 0 : (ExtraCrops + 1) * Settings.DoubleCropChance) + ExtraCrops - GiantCrops;
		private double GiantCrops => Giant ? 2 * (1 - Math.Pow(0.99, Settings.GiantCropChecksPerTile)) : 0;

		protected virtual double ReplantCost
		{
			get
			{
				double cost = 0;
				double seeds = 1;
				foreach (List<KeyValuePair<ISource, IReplant>> rank in Replant.Ranks)
				{
					foreach (KeyValuePair<ISource, IReplant> replant in rank)
					{
						if (replant.Value.Seeds >= seeds)
						{
							cost += seeds * replant.Value.UnitPrice;
							break;
						}
						seeds -= replant.Value.Seeds;
						cost += replant.Value.Price;
					}
				}
				return cost;
			}
		}

		private double[] Dist(int fertQuality)
		{
			double[] dist = new double[3];
			dist[2] = 0.01 + 0.2 * (Skills.BuffedFarmLvl / 10 + fertQuality * (Skills.BuffedFarmLvl + 2) / 12); //gold //check for int division
			dist[1] = Math.Min(2 * dist[2], 0.75) * (1 - dist[2]); //silver
			dist[0] = 1 - dist[1] - dist[2]; //normal
			return dist;
		}

		public

		struct Process
		{
			Product product;
			int numForProduct;
			int numForReplant;
		}
	}
}
