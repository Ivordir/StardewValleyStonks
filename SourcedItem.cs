using System.Collections.Generic;

public abstract class SourcedItem
{
	public string Name { get; }
	public bool Enabled { get; set; }
	public bool Invalid
	{
		get
		{
			return Errors.Count > 0 ? true : false;
		}
	}
	public List<string> Errors;
	public Source Source { get; set; }
	public Dictionary<Source, int> PriceFrom { get; }
	public Dictionary<Source, bool> SourceEnabled { get; }


	public SourcedItem(string name)
	{
		Name = name;
		PriceFrom = new Dictionary<Source, int>{ { Source.None, 0 } };
		SourceEnabled = new Dictionary<Source, bool> { { Source.None, Source.None.Enabled } };
		Source = Source.None;
	}

	public SourcedItem(string name, Dictionary<Source, int> priceFrom)
	{
		Name = name;
		PriceFrom = priceFrom;
		PriceFrom.Add(Source.None, 0);
		SourceEnabled = new Dictionary<Source, bool>();
		foreach (KeyValuePair<Source, int> pair in PriceFrom)
		{
			SourceEnabled.Add(pair.Key, pair.Key.Enabled);
		}
		FindBestSource();
	}

	public int Price
	{
		get
		{
			return PriceFrom[Source];
		}
	}

	public void EnableSource(Source source)
	{
		if (SourceEnabled.ContainsKey(source))
		{
			SourceEnabled[source] = true;
			if (PriceFrom[Source] < Price)
			{
				Source = source;
			}
		}
	}

	public void DisableSource(Source source)
	{
		if (SourceEnabled.ContainsKey(source))
		{
			SourceEnabled[source] = false;
			if (Source == source)
			{
				FindBestSource();
			}
		}
	}

	public void FindBestSource()
	{
		int cheapestPrice = int.MaxValue;
		foreach (KeyValuePair<Source, int> pair in PriceFrom)
		{
			if (SourceEnabled[pair.Key] && pair.Value < cheapestPrice)
			{
				cheapestPrice = pair.Value;
				Source = pair.Key;
			}
		}
	}
	
	public void AddSource(Source source, int price)
	{
		PriceFrom.Add(source, price);
		SourceEnabled.Add(source, source.Enabled);
		if (price < PriceFrom[Source])
		{
			Source = source;
		}
	}
	
	public void RemoveSource(Source source)
	{
		PriceFrom.Remove(source);
		SourceEnabled.Remove(source);
		if (Source == source)
		{
			FindBestSource();
		}
	}
	public bool OneValidSource
	{
		get
		{
			foreach (Source source in SourceEnabled.Keys)
			{
				if (SourceEnabled[source])
				{
					return true;
				}
			}
			return false;
		}
	}

	public string Image
	{
		get
		{
			return "images/" + Name + ".png";
		}
	}
}
