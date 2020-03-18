using System.Collections.Generic;

namespace StardewValleyMaximizeProfit
{
	public class InputState
	{
		public List<string> Sources { get; set; }

		public int FarmLvl { get; set; } = 0;

		public int StartSeason { get; set; } = 1;

		public int EndSeason { get; set; } = 3;

		public int StartDay { get; set; } = 1;

		public int EndDay { get; set; } = 28;

		public bool GreenhouseMode { get; set; } = false;

		public bool Till { get; set; } = false;

		public bool Agri { get; set; } = false;

		public bool Arti { get; set; } = false;

		public bool Irrigated { get; set; } = false;

		public bool QualityProducts { get; set; } = false;

		public Fertilizer StaringFert { get; set; } = Fertilizer.None;
	}
}