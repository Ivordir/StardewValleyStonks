using System.Collections.Generic;

public abstract class ItemWithSources
{
	public int Price { get; set; }

    private Sources SelectedSource;
    private readonly int CheapestPrice;
    private readonly Sources CheapestSource;
    private readonly Dictionary<Sources, int> PriceFrom;
		
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
		
	public void TrySetSource(Sources source)
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
}
