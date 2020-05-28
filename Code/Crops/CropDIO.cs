﻿using System;
using System.Collections.Generic;
using System.Linq;

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
				throw new NotImplementedException();
			}
		}
		public override string Warnings
		{
			get
			{
				if (!Active)
				{
					if (!IsInSeason)
					{

					}
					else if (!CanGiveOneHarvest)
					{

					}
				}
				return "";
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

		//public Crop ToCrop() => new Crop(
		//	Name,
		//	Grow,
		//	SpeedMultipliers.
		//		Where(m => m.Active).
		//		Sum(m => m.Value),
		//	Crop,
		//	QualityCrops,
		//	NormalCrops,
		//	Price,
		//	BestPrices.
		//		Select(p => p.Source).
		//		ToArray(),
		//	HarvestedItems.ToDictionary
		//		(kvp => kvp.Key, kvp => (QualityDist)kvp.Value.Select(v => v.Value).ToArray()),
		//	Processes.ToDictionary
		//		(kvp => kvp.Input, kvp => new Process[] { kvp }),
		//	Replants.ToDictionary
		//		(kvp => kvp.Input, kvp => new Process[] { kvp }));

		readonly Grow Grow;
		readonly Dictionary<Item, IValue[]> HarvestedItems;
		public Process[] _Processes => Processes;
		readonly Process[] Processes;
		readonly Process[] Replants;

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
			Process[] processes,
			Dictionary<Source, Price> priceFrom,
			Dictionary<Item, IValue[]> harvestedItems = null)
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
			HarvestedItems = harvestedItems ?? new Dictionary<Item, IValue[]>();
		}
	}
}