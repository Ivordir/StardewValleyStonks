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
		internal Item Crop { get; }
		internal Dictionary<Item, Value[]> HarvestedItems { get; }
		internal MultiProcess[] Processes { get; }
		internal MultiProcess[] Replants { get; }

		public CropDIO(
			string name,
			Seasons seasons,
			Grow grow,
			Dictionary<Item, Value[]> harvestedItems,
			MultiProcess[] processes,
			MultiProcess[] replants,
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

	}
}
