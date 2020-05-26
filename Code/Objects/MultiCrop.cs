using ExtentionsLibrary.Collections;
using ExtentionsLibrary.Limits;
using System.Collections.Generic;
using System.Linq;

namespace StardewValleyStonks
{
    public class MultiCrop
    {
        public string Name { get; }
        public Seasons Seasons { get; }
        public int[] GrowthStages => Grow.GrowthStages;
        public bool Regrows => Grow.Regrows;
        public int RegrowTime => Grow.RegrowTime;
        public int Price { get; }
        public Source[] Sources { get; }

        public int GrowthTimeWith(double speed) => Grow.Time(SpeedMultiplier + speed);
        public int HarvestsWithin(int days, double speed = 0) => Grow.HarvestsWithin(days, SpeedMultiplier + speed);
        public int HarvestsWithin(ref int days, double speed = 0) => Grow.HarvestsWithin(ref days, SpeedMultiplier + speed);

        private void ApplyDistribution(double[] dist)
        {
            InputAmounts[CropItem][0] = dist[0] * QualityCrops + NormalCrops;
            InputAmounts[CropItem][1] = dist[1] * QualityCrops;
            InputAmounts[CropItem][2] = dist[2] * QualityCrops;
        }

        public bool DestroysFertilizer { get; }
        public bool FertilizerCompatable { get; }

        private readonly Grow Grow;
        private readonly double SpeedMultiplier;
        private readonly Source BuySource;
        private readonly IItem CropItem;
        private readonly double QualityCrops;
        private readonly double NormalCrops;

        public MultiCrop(CropDIO crop, Source buySource)
        {
            Name = crop.Name;
            Grow = crop.Grow;
			SpeedMultiplier = crop.SpeedMultipliers
				.Where(m => m.Active)
				.Sum(m => m.Value);
            BuySource = buySource; //needs to be a copy from factory
			CropItem = crop.Crop;
            QualityCrops = crop.QualityCrops;
            NormalCrops = crop.NormalCrops;
			InputAmounts = crop.HarvestedItems.ToDictionary
				(kvp => kvp.Key, kvp => kvp.Value.Select(v => (double)v).ToList());
			InputOrder = new List<IItem>(InputAmounts.Keys
				.OrderBy(i => i.Name));
			Price = crop.Price;
			Sources = crop.BestPrices
				.Select(x => x.Source)
				.ToArray();

			EqualAlternativesTo = new Dictionary<MultiProcess, List<MultiProcess>>();
			LinkedList<MultiProcess> processes = new LinkedList<MultiProcess>
				(crop.Processes
				.Where(p => p.Active))
				.DoComparisons(EqualAlternativesTo);
			ProcessesWith = crop.Normals.ToDictionary
				(item => item, item => processes.TakeRemove(p => p.HasInput(item)));

			List<SoldItems> soldItems = SoldItemsCalc(InputOrder, InputAmounts);

			Replants = crop.Replants
				.Where(r => r.Active)
				.ToArray();
		}

        private readonly List<IItem> InputOrder;
        private readonly Dictionary<IItem, List<double>> InputAmounts;

		private readonly Dictionary<MultiProcess, List<MultiProcess>> EqualAlternativesTo;
		private readonly Dictionary<IItem, List<MultiProcess>> ProcessesWith;

		public List<SoldItems> SoldItemsCalc(List<IItem> inputs, Dictionary<IItem, List<double>> amounts)
		{
			if (amounts.Count == 0)
			{
				return new List<SoldItems>() { new SoldItems() };
			}
			List<SoldItems> soldItems = new List<SoldItems>();
			foreach (MultiProcess process in ProcessesWith[inputs[^1]])
			{
				Dictionary<IItem, List<double>> leftOver = amounts.ToDictionary
					(kvp => kvp.Key, kvp => new List<double>(kvp.Value));
				List<IItem> newInputs = new List<IItem>(inputs);

				double output = process.MaxOutput(amounts);
				Dictionary<IItem, List<(double, MultiProcess)>> consumed = new Dictionary<IItem, List<(double, MultiProcess)>>();
				foreach (IItem item in process.Inputs.Keys)
				{
					List<double> qualities = leftOver[item];
					qualities[^1] -= output * process.Inputs[item];
					consumed.Add(item, new List<(double, MultiProcess)> { (output * process.Inputs[item], process) });
					if (qualities[^1] == 0)
					{
						qualities.RemoveLast();
						if (qualities.Count == 0)
						{
							newInputs.Remove(item);
							leftOver.Remove(item);
						}
					}
				}

				SoldItems sold = new SoldItems(process.Profit(output), consumed);
				List<SoldItems> subsequent = SoldItemsCalc(newInputs, leftOver);
				subsequent[0].Add(sold);
				if (soldItems.Count == 0 || subsequent[0].Profit.CompareTo(soldItems[0].Profit) == 0)
				{
					for (int i = 1; i < subsequent.Count; i++)
					{
						subsequent[i].Add(sold);
					}
					soldItems.AddRange(subsequent);
				}
				else if (subsequent[0].Profit.CompareTo(soldItems[0].Profit) == 1)
				{
					soldItems.Clear();
					for (int i = 1; i < subsequent.Count; i++)
					{
						subsequent[i].Add(sold);
					}
					soldItems.AddRange(subsequent);
				}
			}
			return soldItems;
		}


		private readonly MultiProcess[] Replants;

		private List<ReplantMethod> ReplantMethods(Dictionary<IItem, List<double>> inputs, double seeds = 1)
		{
			if (inputs.Count == 0 || seeds == 0)
			{
				return new List<ReplantMethod>();
			}
			List<ReplantMethod> methods = new List<ReplantMethod>();
			foreach (MultiProcess replant in Replants)
			{
				if (replant.HasOutput(inputs))
				{
					double output = replant.MaxOutput(inputs).WithMax(seeds);
					Dictionary<IItem, List<double>> leftOver = inputs.ToDictionary
						(kvp => kvp.Key, kvp => new List<double>(kvp.Value));
					Dictionary<IItem, List<(double, MultiProcess)>> consumed = new Dictionary<IItem, List<(double, MultiProcess)>>();
					foreach (IItem item in replant.Inputs.Keys)
					{
						List<double> qualities = leftOver[item];
						qualities[^1] -= output * replant.Inputs[item];
						consumed.Add(item, new List<(double, MultiProcess)> { (output * replant.Inputs[item], replant) });
						if (qualities[^1] == 0)
						{
							leftOver.Remove(item);
							//newInputs.Remove(item);
						}
					}
					foreach (ReplantMethod method in ReplantMethods(leftOver, seeds - output))
					{
						foreach (var kvp in consumed)
						{
							method.Usages[kvp.Key].AddRange(kvp.Value);
						}
						methods.Add(method);
					}
					methods.Add(new ReplantMethod(
								consumed,
								seeds - output,
								leftOver
								));
				}
			}
			return methods;
		}

		//public List<(SoldItems, ReplantMethod)> Calculate
		//{
		//	get
		//	{
		//		List<(SoldItems, ReplantMethod)> best = new List<(SoldItems, ReplantMethod)> { (new SoldItems(null, 0), null) };
		//		List<ReplantMethod> replantMethods = ReplantMethods(inputs);
		//		if (BestPrice.Exists)// && BuySource.Active)
		//		{
		//			replantMethods.Add(new ReplantMethod(new Dictionary<IItem, List<(IProcess, double)>>(), 1, inputs));
		//		}
		//		else
		//		{
		//			for (int i = 0; i < replantMethods.Count; i++)
		//			{
		//				if (replantMethods[i].BoughtSeeds > 0)
		//				{
		//					replantMethods.RemoveAt(i);
		//					i--;
		//				}
		//			}
		//		}
		//		foreach (ReplantMethod method in replantMethods)
		//		{
		//			List<SoldItems> items;
		//			if (singleCalc)
		//			{
		//				items = SoldMethod(method.LeftOver);
		//			}
		//			else
		//			{
		//				items = SingleSoldMethod(method.LeftOver);
		//			}
		//			if (BestPrice.Exists)// && BuySource.Active)
		//			{
		//				foreach (SoldItems item in items)
		//				{
		//					item.Profit -= method.BoughtSeeds * Price;
		//				}
		//			}
		//			if (items[0].Profit > best[0].Item1.Profit)
		//			{
		//				best.Clear();
		//				foreach (SoldItems item in items)
		//				{
		//					best.Add((item, method));
		//				}
		//			}
		//			else if (items[0].Profit == best[0].Item1.Profit)
		//			{
		//				foreach (SoldItems item in items)
		//				{
		//					best.Add((item, method));
		//				}
		//			}
		//		}
		//		return best;
		//	}
		//}

		public class ReplantMethod
		{
			public Dictionary<IItem, List<(double, MultiProcess)>> Usages { get; }
			public double BoughtSeeds { get; }
			public Dictionary<IItem, List<double>> LeftOver { get; }

			public ReplantMethod(
				Dictionary<IItem, List<(double, MultiProcess)>> usages,
				double seeds,
				Dictionary<IItem, List<double>> leftOver)
			{
				Usages = usages;
				BoughtSeeds = seeds;
				LeftOver = leftOver;
			}
		}

		public class SoldItems
		{
			public Dictionary<IItem, List<(double, MultiProcess)>> Products { get; }
			public double Profit { get; set; }

			public void Add(SoldItems items)
			{
				Profit += items.Profit;
				foreach(IItem item in items.Products.Keys)
				{
					if (Products.ContainsKey(item))
					{
						Products[item].AddRange(items.Products[item]);
					}
					else
					{
						Products.Add(item, items.Products[item]);
					}
				}
			}

			public SoldItems(double profit, Dictionary<IItem, List<(double, MultiProcess)>> products)
			{
				Profit = profit;
				Products = products;
			}
			public SoldItems() : this(0, new Dictionary<IItem, List<(double, MultiProcess)>>()) { }
		}
	}
}
