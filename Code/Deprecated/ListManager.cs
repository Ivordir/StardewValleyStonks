using System.Collections.Generic;

namespace StardewValleyStonks
{
	public abstract class ListManager<TSource, TSourcedItem> : IListManager<TSource, TSourcedItem>
		where TSource : IActiveItem
		where TSourcedItem : IActiveItem
	{
		public List<TSource> BestSources { get; }
		public Dictionary<TSource, TSourcedItem[]> ItemsFrom { get; }

		public ListManager(Dictionary<TSource, TSourcedItem[]> itemsFrom)
		{
			ItemsFrom = itemsFrom;
			BestSources = new List<TSource>();
		}

		public void ToggleItemsFrom(TSource source)
		{
			if (ItemsFrom.ContainsKey(source))
			{
				foreach (TSourcedItem item in ItemsFrom[source])
				{
					item.Enabled = !item.Enabled;
					if (item.Enabled)
					{
						ConsiderSource(source);
					}
					else if (BestSources.Remove(source))
					{
						CheckForUpdate();
					}
				}
			}
		}

		public void UpdateBestSource()
		{
			BestSources.Clear();
			foreach (TSource source in ItemsFrom.Keys)
			{
				ConsiderSource(source);
			}
		}

		public void Add(TSource source, TSourcedItem[] sourcedItem)
		{
			if (ItemsFrom.TryAdd(source, sourcedItem))
			{
				ConsiderSource(source);
			}
		}

		public void Remove(TSource source)
		{
			if (ItemsFrom.Remove(source))
			{
				CheckForUpdate();
			}
		}

		private void CheckForUpdate()
		{
			if (BestSources.Count == 0)
			{
				UpdateBestSource();
			}
		}

		private void ConsiderSource(TSource source)
		{
			if (source.Active)
			{
				foreach (TSourcedItem item in ItemsFrom[source])
				{
					if (item.Active)
					{
						if (BestSources.Count == 0)
						{
							BestSources.Add(source);
							return;
						}
						int comparison = Compare(item);
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
			}
		}

		//-1: current BestSource is worse than source, 0: same, 1: current BestSource is better than source
		protected abstract int Compare(TSourcedItem item);
	}
}
