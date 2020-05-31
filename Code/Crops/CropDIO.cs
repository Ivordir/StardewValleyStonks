using System.Collections.Generic;
using System.Linq;
using ExtentionsLibrary.Collections;

namespace StardewValleyStonks
{
	public class CropDIO : DataTableItem
	{
		public Seasons Seasons { get; }
		public Seasons SelectedSeasons { get; set; }

		public int GrowthTime => Grow.DaysPerHarvest(GrowthMultipliers.Where(m => m.Active).Sum(m => m.Value));
		public IMultiplier[] GrowthMultipliers { get; }
		public bool Regrows => Grow.Regrows;
		public int RegrowTime => Grow.RegrowTime;
		public bool DestroysFertilizer { get; }
		public double QualityCrops => NoGiantCropProb;
		public double NormalCrops =>
			((DoubleCrops ? ExtraCrops * Settings.DoubleCropProb + Settings.DoubleCropProb : 0)
			+ ExtraCrops) * NoGiantCropProb + GiantCrops;
		public bool BuySeeds { get; set; }
		public override bool Active
		{
			get
			{
				if (!IsInSeason)
				{
					return false;
				}
				else if (!CanGiveOneHarvest)
				{
					return false;
				}
				// no products
				// no replant
				return true;
			}
		}
		public override string Warnings
		{
			get
			{
				string warnings = string.Empty;
				if (!Active)
				{
					if (!IsInSeason)
					{
						warnings += $"{Name} is not in season.";
					}
					else if (!CanGiveOneHarvest)
					{
						warnings += $"There are not enough days for {Name} to give at least one harvest.";
					}
				}
				return warnings;
			}
		}

		public bool GrowsIn(Seasons seasons)
		=> (Seasons & seasons) > 0;
		public bool IsInSeason => GrowsIn(Date.Seasons);
		bool CanGiveOneHarvest
		{
			get
			{
				if (GrowsIn(Date.Seasons))
				{
					int adjacentSum = 0;
					foreach (Seasons season in Date.SingleSeasons())
					{
						if (GrowsIn(season))
						{
							adjacentSum += Date.DaysInSeason(season);
							if (adjacentSum >= GrowthTime)
							{
								return true;
							}
						}
						else
						{
							adjacentSum = 0;
						}
					}
				}
				return false;
			}
		}
		public Item Seed { get; }
		public Crop ToCrop()
		{
			Dictionary<Item, Process[][]> processes = new Dictionary<Item, Process[][]>();
			foreach(Item item in Processes.Keys)
			{
				for (int quality = 0; quality < HarvestedItems[item].Length; quality++)
				{
					processes[item][quality] = Processes[item].MaxElements(Process.Comparers[quality]).ToArray();
				}
			}
			Dictionary<Item, Process[][]> replants = new Dictionary<Item, Process[][]>();
			foreach (Item item in Replants.Keys)
			{
				for (int quality = 0; quality < HarvestedItems[item].Length; quality++)
				{
					replants[item][quality] = Replants[item].MaxElements(Process.Comparers[quality]).ToArray();
				}
			}
			return new Crop(
			Name,
			Grow,
			GrowthMultipliers.
				Where(m => m.Active).
				Sum(m => m.Value),
			CropItem,
			QualityCrops,
			NormalCrops,
			HarvestedItems.ToDictionary
				(kvp => kvp.Key, kvp => (QualityDist)kvp.Value),
			processes,
			replants,
			BuySeeds,
			Price,
			BestPrices.
				Select(p => p.Source).
				ToArray());
		}

		readonly Grow Grow;
		readonly Dictionary<Item, double[]> HarvestedItems;
		readonly Dictionary<Item, Process[]> Processes;
		readonly Dictionary<Item, Process[]> Replants;
		readonly Source BuySeedsSource;

		readonly Item CropItem;
		readonly double ExtraCrops;
		readonly bool Giant, DoubleCrops;
		double GiantCrops => 2 * (1 - NoGiantCropProb);
		double NoGiantCropProb => Giant ? Settings.NoGiantCropProb : 1;

		readonly Settings Settings;
		readonly Date Date;
		readonly bool Indoors;

		public CropDIO(
			string name,
			Seasons seasons,
			Grow grow,
			IMultiplier[] growthMultipliers,
			Item cropItem,
			double extraCropChance,
			int yield,
			bool giant,
			bool doubleCrop,
			Settings settings,
			Date date,
			Dictionary<Item, Process[]> processes,
			Dictionary<Item, Process[]> replants,
			Dictionary<Source, Price> priceFrom,
			Dictionary<Item, double[]> harvestedItems,
			Item seed)
			: base (name, priceFrom)
		{
			Seasons = seasons;
			SelectedSeasons = Seasons;
			Grow = grow;
			GrowthMultipliers = growthMultipliers;
			CropItem = cropItem;
			ExtraCrops = 1 / (1 - extraCropChance) + yield - 2;
			Giant = giant;
			DoubleCrops = doubleCrop;
			Settings = settings;
			Date = date;
			Processes = processes;
			Replants = replants;
			HarvestedItems = harvestedItems;
			Seed = seed;
		}
	}
}
