using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class SourceManager
    {
		public bool Enabled { get; set; }
		public Source Source { get; set; }
		public Dictionary<Source, BoughtItem> DataFrom { get; }

		public SourcedItem()
		{
			DataFrom = new Dictionary<Source, BoughtItem> { { Source.None, new BoughtItem(0, Source.None.IsEnabled) } };
			UpdateSource(Source.None);
		}

		public SourcedItem(Dictionary<Source, BoughtItem> dataFrom)
		{
			DataFrom = dataFrom;
			if (!DataFrom.ContainsKey(Source.None))
			{
				DataFrom.Add(Source.None, new BoughtItem(0, Source.None.IsEnabled));
			}
			FindBestSource();
		}

		public int Price => Source == null ? -1 : DataFrom[Source].Price;

		public void ToggleSource(Source source)
		{
			if (DataFrom.ContainsKey(source))
			{
				DataFrom[source].IsEnabled = !DataFrom[source].IsEnabled;
				UpdateSource(source);
			}
		}

		private void UpdateSource(Source source)
		{
			if (DataFrom[source].IsEnabled)
			{
				SourceWasEnabled(source);
			}
			else
			{
				SourceWasDisabled(source);
			}
		}

		private void SourceWasEnabled(Source source)
		{
			if (Source == null || DataFrom[Source].Price < Price)
			{
				Source = source;
			}
		}

		private void SourceWasDisabled(Source source)
		{
			if (Source == source)
			{
				FindBestSource();
			}
		}

		private void FindBestSource()
		{
			int cheapestPrice = int.MaxValue;
			bool oneValidSource = false;
			foreach (KeyValuePair<Source, BoughtItem> pair in DataFrom)
			{
				if (DataFrom[pair.Key].IsValid && pair.Value.Price < cheapestPrice)
				{
					oneValidSource = true;
					cheapestPrice = pair.Value.Price;
					Source = pair.Key;
				}
			}
			if (!oneValidSource)
			{
				Source = null;
			}
		}

		public void AddSource(Source source, int price, List<ICondition> conditions = null)
		{
			DataFrom.Add(source, new BoughtItem(price, source.IsEnabled, conditions));
			if (source.IsEnabled)
			{
				SourceWasEnabled(source);
			}
		}

		public void RemoveSource(Source source)
		{
			if (DataFrom[source].IsEnabled)
			{
				SourceWasDisabled(source);
			}
			DataFrom.Remove(source);
		}
	}
}
