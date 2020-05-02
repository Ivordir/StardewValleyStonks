using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
	public class Ranker<TSource, TSourcedItem> : IRanker<TSource, TSourcedItem>
		where TSource : ISelectable
		where TSourcedItem : ISelectable, IComparable<TSourcedItem>
	{
		//[rank][index]

		private readonly Dictionary<TSource, List<TSourcedItem>> Dict;

		private readonly List<List<KeyValuePair<TSource, TSourcedItem>>> _Ranks;

		public Ranker(Dictionary<TSource, List<TSourcedItem>> dict)
		{
			Dict = dict;
			_Ranks = new List<List<KeyValuePair<TSource, TSourcedItem>>>();
		}

		public List<List<KeyValuePair<TSource, TSourcedItem>>> Ranks
		{
			get
			{
				_Ranks.Clear();
				foreach (KeyValuePair<TSource, List<TSourcedItem>> kvp in Dict)
				{
					foreach (TSourcedItem item in kvp.Value)
					{
						if (kvp.Key.Active && item.Active)
						{
							if (_Ranks.Count == 0)
							{
								_Ranks.Add(NewRank(kvp.Key, item));
							}
							else
							{
								for (int rank = 0; rank < _Ranks.Count; rank++)
								{
									if (Compare(rank, item) == 0)  //item is equal to other items in this rnak
									{
										_Ranks[rank].Add(NewKvp(kvp.Key, item));
										goto Added;
									}
									else if (Compare(rank, item) == -1) //item is better than other items in this rank
									{
										_Ranks.Insert(rank, NewRank(kvp.Key, item));
										goto Added;
									}
								}
								//not added, append to end
								_Ranks.Add(NewRank(kvp.Key, item));
							}
						}
					Added:;
					}
				}
				return _Ranks;
			}
		}

		private List<KeyValuePair<TSource, TSourcedItem>> NewRank(TSource source, TSourcedItem item)
		{
			return new List<KeyValuePair<TSource, TSourcedItem>> { NewKvp(source, item) };
		}

		private KeyValuePair<TSource, TSourcedItem> NewKvp(TSource source, TSourcedItem item)
		{
			return new KeyValuePair<TSource, TSourcedItem>(source, item);
		}

		//-1: current BestSource is worse than source, 0: same, 1: current BestSource is better than source
		protected int Compare(int rank, TSourcedItem other)
		{
			return _Ranks[rank][0].Value.CompareTo(other);
		}
	}
}
