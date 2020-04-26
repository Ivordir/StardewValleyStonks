using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
	//keeps track of the best SourcedItem(s) based on which Sources and Items are active.
	public abstract class BestItemTracker<TSource, TSourcedItem> : IBestItemTracker<TSource, TSourcedItem>
		where TSource : IActiveItem
		where TSourcedItem : IActiveItem
	{
		public List<TSource> BestSources { get; }
		public Dictionary<TSource, TSourcedItem> ItemFrom { get; }

		public BestItemTracker(Dictionary<TSource, TSourcedItem> itemFrom)
		{
			ItemFrom = itemFrom;
			BestSources = new List<TSource>();
		}

		public bool NoSource => BestSources.Count == 0;
		public bool HasSource => !NoSource;

		public void ToggleItemFrom(TSource source)
		{
			if (ItemFrom.ContainsKey(source))
			{
				ItemFrom[source].Enabled = !ItemFrom[source].Enabled;
				if (ItemFrom[source].Enabled)
				{
					ConsiderSource(source);
				}
				else if (BestSources.Remove(source))
				{
					CheckForUpdate();
				}
			}
		}

		public void UpdateBestSource()
		{
			BestSources.Clear();
			foreach (TSource source in ItemFrom.Keys)
			{
				ConsiderSource(source);
			}
		}

		public void Add(TSource source, TSourcedItem sourcedItem)
		{
			if (ItemFrom.TryAdd(source, sourcedItem))
			{
				ConsiderSource(source);
			}
		}

		public void Remove(TSource source)
		{
			if (ItemFrom.Remove(source))
			{
				CheckForUpdate();
			}
		}

		private void CheckForUpdate()
		{
			if (NoSource)
			{
				UpdateBestSource();
			}
		}

		private void ConsiderSource(TSource source)
		{
			if (source.Active && ItemFrom[source].Active)
			{
				if (NoSource)
				{
					BestSources.Add(source);
					return;
				}
				int comparison = Compare(source);
				if (comparison == 0)  //source is equal to BestSource
				{
					BestSources.Add(source);
				}
				else if (comparison == -1) //source is better than BestSource
				{
					BestSources.Clear();
					BestSources.Add(source);
				}
			}
		}

		protected abstract int Compare(TSource source);
	}
}
