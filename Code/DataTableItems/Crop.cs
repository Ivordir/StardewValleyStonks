﻿using Microsoft.AspNetCore.Components;
using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
	public class Crop : DataTableItem
	{
		
		[Inject] private SettingsState Settings { get; }

		private readonly IGrow Grow;
		public bool Regrows => Grow.Regrows;
		public int RegrowTime => Grow.RegrowTime;
		public int GrowthTime => Grow.TotalTime;
		public int GrowthTimeWith(double speed) => Grow.Time(speed);
		public int HarvestsWithin(int days, double speed = 0) => Grow.HarvestsWithin(days, speed);
		public int HarvestsWithin(ref int days, double speed = 0) => Grow.HarvestsWithin(ref days, speed);


		private readonly bool Indoors;
		//find what should be protected and what should be private
		//List<IPenalty> Penalties { get; }
		protected readonly ICropAmount CropAmount;
		protected readonly IItemAmount[] HarvestedItems;
		protected readonly Process[] processes;
		protected readonly Process[] replantProcesses;

		public Crop(
			string name,
			IGrow grow,
			ICropAmount cropAmount,
			IItemAmount[] harvestedItems,
			IBestItemTracker<ISource, IPrice> priceManager)
			: base(name, priceManager)
		{
			Grow = grow;
			CropAmount = cropAmount;
			HarvestedItems = harvestedItems;
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
			CropAmount.SetAmounts(fertQuality);
			List<ItemAmount> inputs = new List<ItemAmount>();
			
			return ProfitPerHarvest(fertQuality) * harvests;
		}

		public List<List<Process>> ReplantPlans(List<ItemAmount> inputs, int seeds = 1)
		{
			foreach (Process replant in replantProcesses)
			{
				if (replant.MaxOutput(inputs))
			}
		}
	}
}
