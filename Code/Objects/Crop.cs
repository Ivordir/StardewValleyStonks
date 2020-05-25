﻿using ExtentionsLibrary.Collections;
using System.Collections.Generic;
using System.Linq;

namespace StardewValleyStonks
{
    public class Crop
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
            InputAmounts[Crops[0]] = dist[0] * QualityCrops + NormalCrops;
            InputAmounts[Crops[1]] = dist[1] * QualityCrops;
            InputAmounts[Crops[2]] = dist[2] * QualityCrops;
        }

        public bool DestroysFertilizer { get; }
        public bool FertilizerCompatable { get; }

        private readonly Grow Grow;
        private readonly double SpeedMultiplier;
        private readonly Source BuySource;
        private readonly IItem[] Crops;
        private readonly double QualityCrops;
        private readonly double NormalCrops;

        public Crop(CropDIO crop, Source buySource)
        {
            Name = crop.Name;
            Grow = crop.Grow;
			SpeedMultiplier = crop.SpeedMultipliers
				.Where(m => m.Active)
				.Sum(m => m.Value);
            BuySource = buySource; //needs to be a copy from factory
			Crops = crop.Crops;
            QualityCrops = crop.QualityCrops;
            NormalCrops = crop.NormalCrops;
			InputAmounts = crop.HarvestedItems.ToDictionary
				(kvp => kvp.Key, kvp => kvp.Value.Value);
			InputOrder = InputAmounts.Keys
				.OrderBy(i => i.Name)
				.ToArray();
			Price = crop.Price;
			Sources = crop.BestPrices
				.Select(x => x.Source)
				.ToArray();

			EqualAlternativesTo = new Dictionary<Process, List<Process>>();
			LinkedList<Process> processes = new LinkedList<Process>
				(crop.Processes
				.Where(p => p.Active))
				.DoComparisons(EqualAlternativesTo);
			ProcessesWith = InputOrder.ToDictionary
				(item => item, item => processes.TakeRemove(p => p.HasInput(item)));

			List<IItem> inputs = new List<IItem>(InputOrder);
			List<SoldItems> soldItems = SoldItemsCalc(inputs, InputAmounts);
		}

        private readonly IItem[] InputOrder;
        private readonly Dictionary<IItem, double> InputAmounts;

		private readonly Dictionary<Process, List<Process>> EqualAlternativesTo;
		private readonly Dictionary<IItem, List<Process>> ProcessesWith;

		public List<SoldItems> SoldItemsCalc(List<IItem> inputs, Dictionary<IItem, double> amounts)
		{
			if (amounts.Count == 0)
			{
				return new List<SoldItems>() { new SoldItems() };
			}
			List<SoldItems> soldItems = new List<SoldItems>();
			foreach (Process process in ProcessesWith[inputs[0]])
			{
				Dictionary<IItem, double> leftOver = new Dictionary<IItem, double>(amounts);
				List<IItem> newInputs = new List<IItem>(inputs);

				double output = process.MaxOutput(amounts);
				Dictionary<IItem, List<(double, Process)>> consumed = new Dictionary<IItem, List<(double, Process)>>();
				foreach (IItem item in process.Inputs.Keys)
				{
					leftOver[item] -= output * process.Inputs[item];
					consumed.Add(item, new List<(double, Process)> { (output * process.Inputs[item], process) });
					if (leftOver[item] == 0)
					{
						leftOver.Remove(item);
						newInputs.Remove(item);
					}
				}

				double profit = process.Profit(output);
				SoldItems sold = new SoldItems(profit, consumed);
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
			public Dictionary<IItem, List<(Process, double)>> Usages { get; }
			public double BoughtSeeds { get; }
			public Dictionary<IItem, double> LeftOver { get; }

			public ReplantMethod(
				Dictionary<IItem, List<(Process, double)>> usages,
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
			public Dictionary<IItem, List<(double, Process)>> Products { get; }
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

			public SoldItems(double profit, Dictionary<IItem, List<(double, Process)>> products)
			{
				Profit = profit;
				Products = products;
			}
			public SoldItems() : this(0, new Dictionary<IItem, List<(double, Process)>>()) { }
		}
	}
}
