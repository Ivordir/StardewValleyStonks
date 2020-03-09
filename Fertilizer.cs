using System.Collections.Generic;

public class Fertilizer : SourcedItem
{
    public static Fertilizer None;
    public static List<Fertilizer> Fertilizers;
    
    static Fertilizer()
    {
        Fertilizers = new List<Fertilizer>();
        None = new Fertilizer("None", 0, 0);
    }

    public int Quality { get; }
    public float Speed { get; }
    private Fertilzer Superior; //tree where the parent node is a superior fertilizer
    private List<Fertilizer> Inferiors; //immediate children

    public Fertilizer(string name, int quality, float speed) : base(name)
    {
        Quality = quality;
        Speed = speed;
        Fertilizers.Add(this);
        FindSuperior();
    }

    public Fertilizer(string name, int quality, float speed, Dictionary<Source, int> priceFrom) : base(name, priceFrom)
    {
        Quality = quality;
        Speed = speed;
        Fertilizers.Add(this);
        FindSuperior();
    }
    
    public Fertilizer SuperiorFert
    {
        get
        {
            if (Superior == null)
            {
                return null;
            }
            else if (Superior.Superior == null)
            {
                return Superior;
            }
            return Superior.SuperiorFert;
        }
    }
    
    public Fertilizer FindSuperior()
    {
        Fertilizers.Remove(this);
        for (int i = 0; i < Fertilizers.Count; i++)
        {
            Fertilizer fert = Fertilizers[i];
            if (InferiorTo(fert))
            {
                fert.Inferiors.Add(this);
                Superior = fert;
            }
        }
        Superior = null;
        Fertilizers.Add(this);
    }
    
    public bool InferiorTo(Fertilizer fert)
    {
        return fert.Price <= Price && fert.Speed >= Speed && fert.Quality >= Quality;
    }
    
    private new void SourceWasEnabled(Source source)
	{
		if (Source == null || PriceFrom[Source] < Price)
		{
			Source = source;
            UpdateTree();
		}
	}
    
    private new FindBestSource()
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
		if (oneValidSource)
		{
            UpdateTree();
		}
        else
        {
            Source = null;
            for (int i = 0; i < Inferiors.Count; i++)
            {
                Fertilizer fert = Inferiors[i];
                fert.Superior = Superior;
            }
            Inferiors.Clear();
            Superior = null;
        }
    }
    
    private void UpdateTree()
    {
        Fertilizer oldSuperior = Superior;
        FindSuperior();
        if (Superior != null && olderSuperior != Superior && )
        {

        }
    }
}
