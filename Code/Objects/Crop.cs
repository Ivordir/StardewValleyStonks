using ExtentionsLibrary.Collections;
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
				.OrderBy(i => i.Name).ToArray();
			Price = crop.Price;
			Sources = crop.BestPrices
				.Select(x => x.Source).ToArray();

			HashSet<IProcess> allProcesses = crop.Processes
				.FindAll(p => p.Active).ToHashSet();
			foreach (IItem item in InputOrder)
			{
				IProcess process = allProcesses.FirstOrDefault(p => p.HasInput(item));
				while (process != null)
				{
					List<IProcess> pro = new List<IProcess>() { process };
					pro.AddRange(allProcesses.Where(p => process.SameInputs(p)));
					allProcesses.RemoveAll(pro);

					LinkedList<IProcess> link = new LinkedList<IProcess>(pro);
					LinkedListNode<IProcess> node = link.First;
					while(node != link.Last)
					{
						LinkedListNode<IProcess> compare = node.Next;
						while (compare != link.Last)
						{
							if (true) //can compare
							{
								if (true) //pro[i] == pro[j]
								{
									EqualProcesses[node.Value].Add(compare.Value);
									link.Remove(compare);
								}
								else if (true) //pro[i] > pro[j]
								{
									link.Remove(compare);
								}
								else //pro[i] < pro[j]
								{

									link.Remove(node);
								}
							}
							compare = compare.Next;
						}
						node = node.Next;
					}


					Perms[item].Add(pro);

					process = allProcesses.FirstOrDefault(p => p.HasInput(item));
				}
			}
		}

		//private readonly IProcess[] Processes;

        private readonly IItem[] InputOrder;
        private readonly Dictionary<IItem, double> InputAmounts;

		private readonly Dictionary<IProcess, List<IProcess>> EqualProcesses;
		
		private readonly Dictionary<IItem, List<List<IProcess>>> Perms;


		//private Dictionary<IItem, List<List<IProcess>>> Permu(List<IProcess> processes)
		//{
		//	InputAmounts.Keys.Contains(InputAmounts.Keys);
		//}


		public List<(SoldItems, ReplantMethod)> Calculate
		{
			get
			{
				List<(SoldItems, ReplantMethod)> best = new List<(SoldItems, ReplantMethod)> { (new SoldItems(null, 0), null) };
				List<ReplantMethod> replantMethods = ReplantMethods(inputs);
				if (BestPrice.Exists)// && BuySource.Active)
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
					List<SoldItems> items;
					if (singleCalc)
					{
						items = SoldMethod(method.LeftOver);
					}
					else
					{
						items = SingleSoldMethod(method.LeftOver);
					}
					if (BestPrice.Exists)// && BuySource.Active)
					{
						foreach (SoldItems item in items)
						{
							item.Profit -= method.BoughtSeeds * Price;
						}
					}
					if (items[0].Profit > best[0].Item1.Profit)
					{
						best.Clear();
						foreach (SoldItems item in items)
						{
							best.Add((item, method));
						}
					}
					else if (items[0].Profit == best[0].Item1.Profit)
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
