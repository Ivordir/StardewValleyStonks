using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
	public abstract class Ranker<TSource, TSourcedItem> : IRanker<TSource, TSourcedItem>
		where TSource : IActiveItem
		where TSourcedItem : IActiveItem, IComparable<TSourcedItem>
	{
		//[rank][index]
		public List<List<KeyValuePair<TSource, TSourcedItem>>> BestItems { get; }
		public Dictionary<TSource, List<TSourcedItem>> ItemsFrom { get; }

		public Ranker(Dictionary<TSource, List<TSourcedItem>> itemsFrom)
		{
			ItemsFrom = itemsFrom;
			BestItems = new List<List<KeyValuePair<TSource, TSourcedItem>>>();
		}

		public void ToggleItemsFrom(TSource source)
		{
			if (ItemsFrom.ContainsKey(source))
			{
				foreach (TSourcedItem item in ItemsFrom[source])
				{
					item.Enabled = !item.Enabled;
				}
			}
		}

		public void ReRank()
		{
			BestItems.Clear();
			foreach (TSource source in ItemsFrom.Keys)
			{
				if (source.Active)
				{
					foreach (TSourcedItem item in ItemsFrom[source])
					{
						if (item.Active)
						{
							if (BestItems.Count == 0)
							{
								BestItems.Add(NewRank(source, item));
							}
							else
							{
								bool added = false;
								for (int rank = 0; rank < BestItems.Count; rank++)
								{
									int comparison = Compare(BestItems[rank][0].Value, item);
									if (comparison == 0)  //source is equal to BestSource
									{
										BestItems[rank].Add(KVP(source, item));
										added = true;
										break;
									}
									else if (comparison == -1) //source is better than BestSource
									{
										BestItems.Insert(rank, NewRank(source, item));
										added = true;
										break;
									}
								}
								if (!added)
								{
									BestItems.Add(NewRank(source, item));
								}
							}
						}
					}
				}
			}
		}

		public void Add(TSource source, List<TSourcedItem> sourcedItem)
		{
			ItemsFrom.TryAdd(source, sourcedItem);
		}

		public void Remove(TSource source)
		{
			ItemsFrom.Remove(source);
		}

		private List<KeyValuePair<TSource, TSourcedItem>> NewRank(TSource source, TSourcedItem item)
		{
			return new List<KeyValuePair<TSource, TSourcedItem>> { KVP(source, item) };
		}

		private KeyValuePair<TSource, TSourcedItem> KVP(TSource source, TSourcedItem item)
		{
			return new KeyValuePair<TSource, TSourcedItem>(source, item);
		}

		//-1: current BestSource is worse than source, 0: same, 1: current BestSource is better than source
		protected abstract int Compare(TSourcedItem rank, TSourcedItem other);
	}
}
