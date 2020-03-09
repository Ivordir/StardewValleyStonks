using System.Collections.Generic;

public abstract class SourcedItem
{
	public string Name { get; }
	public bool Enabled { get; set; }
	public Source Source { get; set; }
	public Dictionary<Source, int> PriceFrom { get; }
	public Dictionary<Source, bool> SourceEnabled { get; }

	public SourcedItem(string name)
	{
		Name = name;
		PriceFrom = new Dictionary<Source, int>{ { Source.None, 0 } };
		SourceEnabled = new Dictionary<Source, bool> { { Source.None, Source.None.Enabled } };
		UpdateSource(Source.None);
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
		
	public void ToggleSource(Source source)
	{
		if (SourceEnabled.ContainsKey(source))
		{
			SourceEnabled[source] = !SourceEnabled[source];
			UpdateSource(source);
		}
	}
	
	private void UpdateSource(Source source)
	{
		if (SourceEnabled[source])
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
		if (Source == null || PriceFrom[Source] < Price)
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
		foreach (KeyValuePair<Source, int> pair in PriceFrom)
		{
			if (SourceEnabled[pair.Key] && pair.Value < cheapestPrice)
			{
				oneValidSource = true;
				cheapestPrice = pair.Value;
				Source = pair.Key;
			}
		}
		if (!oneValidSource)
		{
			Source = null;
		}
	}
	
	public void AddSource(Source source, int price)
	{
		PriceFrom.Add(source, price);
		SourceEnabled.Add(source, source.Enabled);
		if (SourceEnabled[source])
		{
			SourceWasEnabled(source);
		}
	}
	
	public void RemoveSource(Source source)
	{
		PriceFrom.Remove(source);
		SourceEnabled.Remove(source);
		SourceWasDisabled(source);
	}

	public string Image
	{
		get
		{
			return "images/" + Name + ".png";
		}
	}
}
