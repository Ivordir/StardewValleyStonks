using System;
using System.Collections.Generic;
using System.Linq;

namespace StardewValleyStonks
{
	public class CropDIO : DataTableItem
	{
		public Seasons Seasons { get; }
		public Seasons SelectedSeasons { get; set; }

		public int GrowthTime => Grow.TotalTime;
		public IMultiplier[] SpeedMultipliers { get; }
		public bool Regrows => Grow.Regrows;
		public int RegrowTime => Grow.RegrowTime;
		public bool DestroysFertilizer { get; }
		public double QualityCrops => NoGiantCropProb;
		public double NormalCrops => (
			(DoubleCrops ? ExtraCrops * Settings.DoubleCropProb + Settings.DoubleCropProb : 0)
			+ ExtraCrops) * NoGiantCropProb + GiantCrops;


		private readonly double ExtraCrops;
		private readonly bool Giant, DoubleCrops;
		private double GiantCrops => 2 * (1 - NoGiantCropProb);
		private double NoGiantCropProb => Giant ? Settings.NoGiantCropProb : 1;

		private readonly Settings Settings;
		private readonly Date Date;
		private readonly bool Indoors;

		internal Grow Grow { get; }
		internal IItem Crop { get; }
		internal Dictionary<IItem, IValue[]> HarvestedItems { get; }
		internal Process[] Processes { get; }
		internal IItem[] Normals { get; }
		internal Process[] Replants { get; }

		public CropDIO(
			string name,
			Seasons seasons,
			Grow grow,
			Dictionary<IItem, IValue[]> harvestedItems,
			Process[] processes,
			Process[] replants,
			Dictionary<Source, Price> priceFrom)
			: base(name, priceFrom)
		{
			Seasons = seasons;
			SelectedSeasons = Seasons;
			Grow = grow;
			HarvestedItems = harvestedItems;
			Processes = processes;
			Replants = replants;
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
		public override List<Warning> Warnings => throw new NotImplementedException();

		public virtual double Profit(int fertQuality = 0, int harvests = 1)
		{
			//CropDistribution.SetAmounts(fertQuality);
			//List<ItemAmount> inputs = new List<ItemAmount>();
			return 0;
			//return ProfitPerHarvest(fertQuality) * harvests;
		}

		//public List<(SoldItems, ReplantMethod)> Calculate
		//{
		//	get
		//	{
		//		Dictionary<IItem, double> inputs = new Dictionary<IItem, double>();
		//		foreach (var input in HarvestedItems)
		//		{
		//			inputs.Add(input.Key, input.Value.Value);
		//		}
		//		if(Processes.Length > 0 && Processes.Any(p => p.Active))
		//		{
		//			AllProcesses.Clear();
		//			foreach (Process process in Processes)
		//			{
		//				if (process.Active)
		//				{
		//					AllProcesses.Add(process);
		//				}
		//			}
		//		}
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
		//			List<SoldItems> items = SoldMethod(method.LeftOver);
		//
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
		//				foreach(SoldItems item in items)
		//				{
		//					best.Add((item, method));
		//				}
		//			}
		//			else if(items[0].Profit == best[0].Item1.Profit)
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

		//private List<ReplantMethod> ReplantMethods(Dictionary<IItem, double> inputs, double seeds = 1)
		//{
		//	if (inputs.Count == 0)
		//	{
		//		return new List<ReplantMethod>();
		//	}
		//	List<ReplantMethod> methods = new List<ReplantMethod>();
		//	foreach (Process replant in Replants)
		//	{
		//		if (replant.Active)
		//		{
		//			double maxOutput = replant.MaxOutput(inputs);
		//			if (maxOutput == 0)
		//			{
		//				continue;
		//			}

		//			Dictionary<IItem, double> leftOver = new Dictionary<IItem, double>(inputs);
		//			Dictionary<IItem, List<(IProcess, double)>> usages; 
		//			if (maxOutput >= seeds)
		//			{
		//				maxOutput = seeds;
		//				usages = replant.ConsumeInput(leftOver, maxOutput);
		//			}
		//			else
		//			{
		//				usages = replant.ConsumeInput(leftOver, maxOutput);
		//				foreach (ReplantMethod method in ReplantMethods(leftOver, seeds - maxOutput))
		//				{
		//					foreach (var kvp in usages)
		//					{
		//						method.Usages[kvp.Key].AddRange(kvp.Value);
		//					}
		//					methods.Add(method);
		//				}
		//			}
		//			methods.Add(new ReplantMethod(
		//						usages,
		//						seeds - maxOutput,
		//						leftOver
		//						));
		//		}
		//	}
		//	return methods;
		//}

		//private List<SoldItems> SoldMethod(Dictionary<IItem, double> inputs)
		//{
		//	List<SoldItems> soldItems = new List<SoldItems>();
		//	if (inputs.Count == 0)
		//	{
		//		soldItems.Add(new SoldItems(new Dictionary<IItem, List<(IProcess, double)>>(), 0));
		//		return soldItems;
		//	}
		//	foreach (Process process in AllProcesses)
		//	{
		//		double maxOutput = process.MaxOutput(inputs);
		//		if (maxOutput == 0)
		//		{
		//			continue;
		//		}
		//		Dictionary<IItem, double> leftOver = new Dictionary<IItem, double>(inputs);
		//		Dictionary<IItem, List<(IProcess, double)>> usages = process.ConsumeInput(leftOver, maxOutput);
		//		double profit = process.Profit(maxOutput);
		//		foreach (SoldItems method in SoldMethod(leftOver))
		//		{
		//			method.Profit += profit;
		//			foreach (var kvp in usages)
		//			{
		//				method.Products[kvp.Key].AddRange(kvp.Value);
		//			}
		//			soldItems.Add(method);
		//		}
		//	}
		//	double maxProfit = soldItems.Max(i => i.Profit);
		//	return soldItems.FindAll(i => i.Profit == maxProfit);
		//}		
	}
}
