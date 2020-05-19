using System;
using System.Collections.Generic;
using System.Linq;

namespace StardewValleyStonks
{
	public class Crop : DataTableItem
	{
		public Seasons Seasons { get; }
		public Seasons SelectedSeasons { get; set; }

		public int[] GrowthStages => Grow.GrowthStages;
		public int GrowthTime => Grow.TotalTime;
		public bool Regrows => Grow.Regrows;
		public int RegrowTime => Grow.RegrowTime;

		public int GrowthTimeWith(double speed) => Grow.Time(speed);
		public int HarvestsWithin(int days, double speed = 0) => Grow.HarvestsWithin(days, speed);
		public int HarvestsWithin(ref int days, double speed = 0) => Grow.HarvestsWithin(ref days, speed);

		private readonly Grow Grow;
		private readonly DateState Date;
		private readonly bool Indoors;
		//find what should be protected and what should be private
		//List<IPenalty> Penalties { get; }
		private readonly ICropAmount CropAmount;
		private readonly Dictionary<IItem, IAmount> HarvestedItems;
		private readonly BestList<SingleProcess>[] SingleProcesses;
		private readonly Process[] Processes;
		private readonly Process[] Replants;
		private readonly Source BuySource;

		public Crop(
			string name,
			Seasons seasons,
			Grow grow,
			ICropAmount cropAmount,
			Dictionary<IItem, IAmount> harvestedItems,
			Process[] processes,
			Process[] replants,
			Source buySource,
			BestDict<Source, BuyPrice> priceManager)
			: base(name, priceManager)
		{
			Seasons = seasons;
			SelectedSeasons = Seasons;
			Grow = grow;
			CropAmount = cropAmount;
			HarvestedItems = harvestedItems;
			Processes = processes;
			Replants = replants;
			BuySource = buySource;
		}

		public override bool Active
		{
			get
			{
				if ((Date.Seasons & SelectedSeasons) == 0)
				{

				}
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
			CropAmount.SetAmounts(fertQuality);
			//List<ItemAmount> inputs = new List<ItemAmount>();
			return 0;
			//return ProfitPerHarvest(fertQuality) * harvests;
		}

		public List<(SoldItems, ReplantMethod)> Calculate
		{
			get
			{
				Dictionary<IItem, double> inputs = new Dictionary<IItem, double>();
				foreach (var input in HarvestedItems)
				{
					inputs.Add(input.Key, input.Value.Value);
				}
				List<(SoldItems, ReplantMethod)> best = new List<(SoldItems, ReplantMethod)> { (new SoldItems(null, 0), null) };
				List<ReplantMethod> replantMethods = ReplantMethods(inputs);
				if (PriceManager.HasBestItem && BuySource.Active)
				{
					replantMethods.Add(new ReplantMethod(new Dictionary<IItem, List<(IProcess, double)>>(), 1, inputs));
				}
				else
				{
					for (int i = 0; i < replantMethods.Count; i++)
					{
						if (replantMethods[i].BoughtSeeds > 0)
						{
							replantMethods.RemoveAt(i);
							i--;
						}
					}
				}
				foreach (ReplantMethod method in replantMethods)
				{
					List<SoldItems> items = SoldMethod(method.LeftOver);
					if (PriceManager.HasBestItem && BuySource.Active)
					{
						foreach (SoldItems item in items)
						{
							item.Profit -= method.BoughtSeeds * Price;
						}
					}
					if (items[0].Profit > best[0].Item1.Profit)
					{
						best.Clear();
						foreach(SoldItems item in items)
						{
							best.Add((item, method));
						}
					}
					else if(items[0].Profit == best[0].Item1.Profit)
					{
						foreach (SoldItems item in items)
						{
							best.Add((item, method));
						}
					}
				}
				return best;
			}
		}

		private List<ReplantMethod> ReplantMethods(Dictionary<IItem, double> inputs, double seeds = 1)
		{
			if (inputs.Count == 0)
			{
				return new List<ReplantMethod>();
			}
			List<ReplantMethod> methods = new List<ReplantMethod>();
			foreach (Process replant in Replants)
			{
				if (replant.Active)
				{
					double maxOutput = replant.MaxOutput(inputs);
					if (maxOutput == 0)
					{
						continue;
					}

					Dictionary<IItem, double> leftOver = new Dictionary<IItem, double>(inputs);
					Dictionary<IItem, List<(IProcess, double)>> usages; 
					if (maxOutput >= seeds)
					{
						maxOutput = seeds;
						usages = replant.ConsumeInput(leftOver, maxOutput);
					}
					else
					{
						usages = replant.ConsumeInput(leftOver, maxOutput);
						foreach (ReplantMethod method in ReplantMethods(leftOver, seeds - maxOutput))
						{
							foreach (var kvp in usages)
							{
								method.Usages[kvp.Key].AddRange(kvp.Value);
							}
							methods.Add(method);
						}
					}
					methods.Add(new ReplantMethod(
								usages,
								seeds - maxOutput,
								leftOver
								));
				}
			}
			return methods;
		}

		private List<SoldItems> SoldMethod(Dictionary<IItem, double> inputs)
		{
			if (inputs.Count == 0)
			{
				return new List<SoldItems>()
				{
					new SoldItems( new Dictionary<IItem, List<(IProcess, double)>>(), 0)
				};
			}
			List<SoldItems> soldItems = new List<SoldItems>();
			foreach (Process process in Processes)
			{
				if (process.Active)
				{
					double maxOutput = process.MaxOutput(inputs);
					if (maxOutput == 0)
					{
						continue;
					}
					Dictionary<IItem, double> leftOver = new Dictionary<IItem, double>(inputs);
					Dictionary<IItem, List<(IProcess, double)>> usages = process.ConsumeInput(leftOver, maxOutput);
					double profit = process.Profit(maxOutput);
					foreach (SoldItems method in SoldMethod(leftOver))
					{
						method.Profit += profit;
						foreach (var kvp in usages)
						{
							method.Products[kvp.Key].AddRange(kvp.Value);

						}
						soldItems.Add(method);
					}
				}
			}
			foreach (BestList<SingleProcess> bestList in SingleProcesses)
			{
				foreach(SingleProcess process in bestList.BestItems)
				{
					double maxOutput = process.MaxOutput(inputs);
					if (maxOutput == 0)
					{
						break;
					}
					Dictionary<IItem, double> leftOver = new Dictionary<IItem, double>(inputs);
					Dictionary<IItem, List<(IProcess, double)>> usages = process.ConsumeInput(leftOver, maxOutput);
					double profit = process.Profit(maxOutput);
					foreach (SoldItems method in SoldMethod(leftOver))
					{
						method.Profit += profit;
						foreach (var kvp in usages)
						{
							method.Products[kvp.Key].AddRange(kvp.Value);
						}
						soldItems.Add(method);
					}
				}
			}
			double maxProfit = soldItems.Max(i => i.Profit);
			return soldItems.FindAll(i => i.Profit == maxProfit);
		}

		public class ReplantMethod
		{
			public Dictionary<IItem, List<(IProcess, double)>> Usages { get; }
			public double BoughtSeeds { get; }
			public Dictionary<IItem, double> LeftOver { get; }

			public ReplantMethod(
				Dictionary<IItem, List<(IProcess, double)>> usages,
				double seeds,
				Dictionary<IItem, double> leftOver)
			{
				Usages = usages;
				BoughtSeeds = seeds;
				LeftOver = leftOver;
			}
		}

		public class SoldItems
		{
			public Dictionary<IItem, List<(IProcess, double)>> Products { get; }
			public double Profit { get; set; }

			public SoldItems(Dictionary<IItem, List<(IProcess, double)>> products, double profit)
			{
				Products = products;
				Profit = profit;
			}
		}
	}
}
