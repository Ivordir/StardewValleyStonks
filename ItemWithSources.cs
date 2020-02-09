using System.Collections.Generic;

public abstract class ItemWithSources
{
	public int Price { get; set; }
	public string SelectedSource { get; set; }
	public int CheapestPrice { get; }
	public string CheapestSource { get; }
	public Dictionary<string, int> PriceFrom { get; }

	public ItemWithSources (Dictionary<Sources, int> priceFrom)
	{
		PriceFrom = priceFrom;
		CheapestPrice = int.MaxValue;
		CheapestSource = 0;
		foreach (KeyValuePair<Sources, int> pair in PriceFrom)
		{
			if (pair.Value < CheapestPrice)
			{
				CheapestPrice = pair.Value;
				CheapestSource = pair.Key;
			}
		}
		PriceFrom.Remove(CheapestSource);
		Price = CheapestPrice;
		SelectedSource = CheapestSource;
	}

	public void TrySetSource(string source)
	{
		if (SelectedSource != source && PriceFrom.ContainsKey(source))
		{
			Price = PriceFrom[source];
			SelectedSource = source;
		}
	}

	public void SetCheapestSource()
	{
		Price = CheapestPrice;
		SelectedSource = CheapestSource;
	}
	
	public void AddSource(string source, int price)
	{
		PriceFrom.Add(source, price);	
	}
	
	public void RemoveSource(string source)
	{
		PriceFrom.Remove(source);	
	}
}
